#!/usr/bin/env python3
"""Minimal, dependency-free MCP server exposing the current date.

Why this exists
---------------
The visualization query router (see .claude/agents/viz-query-router.md) resolves
relative time phrases like "previous two months" or "last 30 days". A language
model has no reliable knowledge of the real current date, so it must ask a tool.
This server provides that single fact via the Model Context Protocol (MCP) over
stdio, using ONLY the Python standard library -- no `pip install` needed, so the
hackathon repo stays reproducible offline.

Consistency with the R layer
-----------------------------
"Today" here matches `cdt_data_end_date()` in R/config.R: if the environment
variable `CDT_DATA_END_DATE=YYYY-MM-DD` is set (frozen build), that date is
returned; otherwise the system date is used. This keeps the router's window
anchoring aligned with the synthetic timeline, which is ingested daily up to the
same "today".

Protocol
--------
Implements the JSON-RPC 2.0 methods MCP needs for a stdio server:
  * initialize
  * notifications/initialized  (no response)
  * tools/list                 -> advertises `get_current_date`
  * tools/call                 -> runs `get_current_date`
  * ping                       -> {}

`get_current_date` takes an optional `timezone` string (informational; the date
is date-only) and returns the ISO date, weekday, and the source ("frozen" vs
"system"). It never mutates anything and reads no secrets.
"""

import sys
import os
import json
import datetime

PROTOCOL_VERSION = "2024-11-05"
SERVER_NAME = "cdt-date"
SERVER_VERSION = "1.0.0"


def _resolve_today():
    """Return (iso_date, source) mirroring R's cdt_data_end_date()."""
    override = os.environ.get("CDT_DATA_END_DATE", "").strip()
    if override:
        try:
            d = datetime.date.fromisoformat(override)
            return d.isoformat(), "frozen"
        except ValueError:
            # Fall through to system date on a malformed override, matching the
            # R helper's tolerant behavior.
            pass
    return datetime.date.today().isoformat(), "system"


def _get_current_date(arguments):
    """The single tool: report today's date + metadata."""
    tz = (arguments or {}).get("timezone")
    iso_date, source = _resolve_today()
    weekday = datetime.date.fromisoformat(iso_date).strftime("%A")
    payload = {
        "date": iso_date,
        "weekday": weekday,
        "source": source,
    }
    if tz:
        payload["timezone"] = str(tz)
    text = (
        f"Today is {iso_date} ({weekday}). "
        f"Source: {source} (CDT_DATA_END_DATE override respected)."
    )
    # MCP tool results carry human-readable content plus optional structured data.
    return {
        "content": [{"type": "text", "text": text}],
        "structuredContent": payload,
    }


TOOLS = [
    {
        "name": "get_current_date",
        "description": (
            "Return the current date (today) the digital-twin system reasons "
            "about, so relative time windows like 'previous two months' can be "
            "anchored. Honors CDT_DATA_END_DATE for frozen builds, else uses the "
            "system date."
        ),
        "inputSchema": {
            "type": "object",
            "properties": {
                "timezone": {
                    "type": "string",
                    "description": (
                        "Optional IANA timezone name for context "
                        "(e.g. 'Europe/Berlin'). The result is date-only."
                    ),
                }
            },
            "additionalProperties": False,
        },
    }
]


def _result(req_id, result):
    return {"jsonrpc": "2.0", "id": req_id, "result": result}


def _error(req_id, code, message):
    return {"jsonrpc": "2.0", "id": req_id, "error": {"code": code, "message": message}}


def _handle(msg):
    """Dispatch one JSON-RPC message; return a response dict or None (notification)."""
    method = msg.get("method")
    req_id = msg.get("id")

    if method == "initialize":
        return _result(
            req_id,
            {
                "protocolVersion": PROTOCOL_VERSION,
                "capabilities": {"tools": {}},
                "serverInfo": {"name": SERVER_NAME, "version": SERVER_VERSION},
            },
        )

    if method in ("notifications/initialized", "initialized"):
        return None  # notification: no response

    if method == "ping":
        return _result(req_id, {})

    if method == "tools/list":
        return _result(req_id, {"tools": TOOLS})

    if method == "tools/call":
        params = msg.get("params") or {}
        name = params.get("name")
        if name == "get_current_date":
            return _result(req_id, _get_current_date(params.get("arguments")))
        return _error(req_id, -32602, f"Unknown tool: {name}")

    if req_id is None:
        return None  # unknown notification: ignore
    return _error(req_id, -32601, f"Method not found: {method}")


def main():
    """Line-delimited JSON-RPC over stdio (one JSON object per line)."""
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            msg = json.loads(line)
        except json.JSONDecodeError:
            continue
        response = _handle(msg)
        if response is not None:
            sys.stdout.write(json.dumps(response) + "\n")
            sys.stdout.flush()


if __name__ == "__main__":
    main()
