# Claude Design Prompt — "Los Almendros" Fall-Risk Simulation Replay

Build a **single self-contained HTML file** (inline CSS + vanilla JS, no build step,
no external network calls) that plays back a 30-day multi-agent clinical simulation as
a cinematic, scrubbable **day-by-day replay**. This is a data-visualization / narrative
animation of an already-completed experiment — you are *replaying recorded results*, not
running anything live.

---

## 0. Design mandate (read first — non-negotiable)

You tend to converge toward generic, "on distribution" outputs. In frontend design, this
creates what users call the "AI slop" aesthetic. Avoid this: make creative, distinctive
frontends that surprise and delight. Focus on:

**Typography:** Choose fonts that are beautiful, unique, and interesting. Avoid generic
fonts like Arial and Inter; opt instead for distinctive choices that elevate the
frontend's aesthetics.

**Color & Theme:** Commit to a cohesive aesthetic. Use CSS variables for consistency.
Dominant colors with sharp accents outperform timid, evenly-distributed palettes. Draw
from IDE themes and cultural aesthetics for inspiration.

**Motion:** Use animations for effects and micro-interactions. Prioritize CSS-only
solutions for HTML. Use Motion library for React when available. Focus on high-impact
moments: one well-orchestrated page load with staggered reveals (animation-delay) creates
more delight than scattered micro-interactions.

**Backgrounds:** Create atmosphere and depth rather than defaulting to solid colors. Layer
CSS gradients, use geometric patterns, or add contextual effects that match the overall
aesthetic.

Avoid generic AI-generated aesthetics:
- Overused font families (Inter, Roboto, Arial, system fonts)
- Clichéd color schemes (particularly purple gradients on white backgrounds)
- Predictable layouts and component patterns
- Cookie-cutter design that lacks context-specific character

Interpret creatively and make unexpected choices that feel genuinely designed for the
context. Vary between light and dark themes, different fonts, different aesthetics. You
still tend to converge on common choices (Space Grotesk, for example) across generations.
Avoid this: it is critical that you think outside the box!

**Contextual steer (use, don't copy literally):** the setting is a fictional Andalusian
residential care home, *Residencia Los Almendros* ("The Almond Trees"), in Sevilla. Think
warm Mediterranean clinical calm meeting a precise clinical-instrument dashboard: sun-baked
terracotta / olive / almond-blossom against a deep ink or warm-charcoal ground, with one
sharp alert accent. An almond-blossom or azulejo (Andalusian tile) geometric motif is a
richer background than a flat gradient. This is **synthetic data, never for clinical use** —
surface that honestly but elegantly (a small persistent watermark).

---

## 1. What the simulation is (so your narrative is accurate)

A digital-twin fall-risk model is stress-tested over **30 days** on a fixed **10-resident
module** (P01–P10). Each simulated day runs an 8-step pipeline:

1. **Institution context** — weekday, physio days (Mon/Wed/Fri), doctor days (Tue/Thu),
   weekend staffing dip, optional flu outbreak.
2. **Social layer** — residents interact (shared activities, chats); one resident (P04,
   Manuel) is the social *connector*; another (P09, Pilar, 94) is *receive-only* and
   initiates nothing.
3. **Agent decisions** — each resident is an LLM agent emitting a daily behavioural JSON
   (mobility %, group activity, medication adherence, mood/fatigue, notable events).
4. **Sensor generation** — wearable readings (steps, heart rate, resting HR, blood
   pressure, accelerometry, posture hours) derived from that day's decision.
5. **Model inference** — the fall-risk model reads each resident's rolling timeline and
   emits **two probabilities per resident per day: P(fall within 24h) and P(fall within
   7d)**, each mapped to a tier: **Low < 0.15 ≤ Moderate < 0.35 ≤ High**.
6. **Intervention arm (Branch B only)** — see §2.
7. **Hidden ground truth** — a privileged, restricted layer (never shown on any clinical
   surface). In the animation you MAY reveal it as a clearly-labelled "director's cut"
   overlay because this is a results replay, but keep it visually distinct from what the
   model/clinician could see.
8. **Daily checkpoint gate** — four validation families (`social`, `agent_json`,
   `biological`, `model`) each pass/warn/fail; the day only advances if the gate passes.

### The blind A/B experiment on P08 (the emotional core of the story)
- **P08 = Joaquin, 77**, type-2 diabetes with mild peripheral neuropathy. His decline is
  **subtle and deliberately NOT visible from step volume** — he keeps walking. The risk
  shows up instead in **rising sedentary/guarding hours, less steady gait (accelerometer
  magnitude down), and a mild resting-HR drift**.
- A hidden latent risk rises from a seed-chosen **onset day (day 18 in Sim 1)** toward a
  hazard ceiling; a fall is sampled stochastically from that rising hazard.
- **Branch A = control** (no intervention). **Branch B = intervention**: when the *model's
  observable* 7-day risk stays over threshold for **2 consecutive days**, a **preventive
  review fires once**, which then damps the deterioration.
- A and B share one RNG stream, so they are a **valid counterfactual**: identical until the
  intervention, then they diverge.

---

## 2. The exact recorded data to visualize (Sim 1, `sim1_baseline`)

Hard-code this data as a JS object; it is the real recorded output. **P08 7-day fall
probability, Branch A (control) vs Branch B (intervention):**

```
day  A_p7d  B_p7d      day  A_p7d  B_p7d      day  A_p7d  B_p7d
 1   0.008  0.008      11   0.018  0.018      21   0.080  0.080
 2   0.009  0.009      12   0.022  0.022      22   0.147  0.163
 3   0.043  0.043      13   0.037  0.037      23   0.155  0.166
 4   0.044  0.044      14   0.027  0.027      24   0.230  0.238
 5   0.028  0.028      15   0.019  0.019      25   0.345  0.347
 6   0.028  0.028      16   0.010  0.010      26   0.361  0.354
 7   0.025  0.025      17   0.010  0.010      27   0.428  0.360
 8   0.018  0.018      18   0.012  0.012      28   0.326  0.187
 9   0.014  0.014      19   0.015  0.015      29   0.270  0.130
10   0.010  0.010      20   0.061  0.061      30   0.197  0.075
```

Key beats to choreograph:
- **Days 1–21:** the two lines are *identical* (overlapping) — the shared-stream truth.
- **Day 18:** hidden latent onset (mark subtly; it's "director's cut" info).
- **Day 22:** lines first diverge; risk crosses **Moderate** (0.15).
- **Days 25–27 (A) / 25–26 (B):** risk crosses **High** (0.35).
- **Day 26:** **Branch B intervention fires** — "Simulated preventive review, sustained
  threshold crossing." This is THE hero moment: a pulse/flare, the review event, then B's
  line bending back down.
- **Days 28–30:** Branch B recovers (0.075 by day 30) while control stays elevated (0.197).
- **Hidden outcome (director's cut only):** Control → **2 falls**, peak hazard 0.18.
  Intervention → **1 fall**, peak hazard 0.16. One prevented fall.

**The full 10-resident cohort on a representative High-risk day (Sim 1 / A / day 26)** —
use this to render the "cohort grid" state; interpolate other days plausibly or just
animate these ten as the headline snapshot:

```
id   name         age  p_24h   p_7d    tier_7d   persona signature
P01  Rosario      88   0.019   0.166   Moderate  fear-of-falling; activity below capacity
P02  Antonio      82   0.054   0.378   High      Parkinson's wearing-off around med timing
P03  Carmen       91   0.016   0.148   Low       night-concentrated risk; daytime flat
P04  Manuel       79   0.002   0.020   Low       stable control; SOCIAL CONNECTOR
P05  Dolores      85   0.014   0.127   Low       afternoon knee pain (PM < AM mobility)
P06  Francisco    90   0.170   0.684   High      progressive decline post new diuretic
P07  Isabel       83   0.112   0.572   High      morning instability (night benzodiazepine)
P08  Joaquin      77   0.051   0.361   High      SUBTLE — not visible from step volume (blind A/B)
P09  Pilar        94   0.068   0.436   High      high stable baseline; night agitation; receive-only
P10  Vicente      86   0.006   0.062   Low       non-linear post-hospital recovery (upward)
```

**Social affinity structure** (for the agent-interaction graph — weights 0–3, higher =
stronger tie): P04 (Manuel) is the hub connected to P01, P03, P05, P06, P08, P09, P10.
P08↔P04, P08↔P02, P08↔P10 are Joaquin's ties. P09 (Pilar) receives but initiates nothing.
Render as a small force-directed / radial node graph that lights up edges on days with
interactions.

**Checkpoint gate:** all 30 days passed all four families (`social`, `agent_json`,
`biological`, `model`) — 100% green. Show this as a reassuring, quietly satisfying
"integrity" strip. (Note: agents are grounded to only speak from clinical data, so several
social summaries honestly say "I won't invent an interaction" — a nice honesty detail you
can surface as flavor text.)

---

## 3. Required views / scenes

A single page with a **transport bar** (play / pause / step / scrub slider over days
1–30, plus a speed control). As the day index advances, ALL panels update in sync:

1. **Hero: P08 counterfactual chart** — animated dual-line plot of A vs B `p_7d` with the
   0.15 (Moderate) and 0.35 (High) threshold bands. Lines draw progressively as days
   advance; the overlap→divergence at day 22 and the intervention flare at day 26 are the
   centerpieces. A moving "today" playhead.
2. **Cohort grid** — 10 resident cards (name, age, persona glyph), each showing live
   **24h and 7d** probability as dual radial gauges or dual bars, tier color-coded
   (Low/Moderate/High). Cards reorder or pulse when a resident crosses into a higher tier.
3. **Alarm feed / driver panel** — when a resident crosses Moderate or High, emit an alert
   card naming the **driver** (e.g. P06 "diuretic-driven decline", P07 "morning
   benzodiazepine instability", P08 "rising sedentary hours + gait unsteadiness, steps
   held"). This is the "alarm-drivers" the user asked for — tie each alert to the persona
   signature above.
4. **Agent interaction graph** — the affinity node graph; edges pulse on interaction days,
   P04 visibly central, P09 clearly receive-only.
5. **Integrity strip** — the 4-family checkpoint gate, all green, ticking day by day.
6. **Director's cut toggle** — an explicit switch that overlays the hidden ground truth
   (latent onset day 18, sampled falls, hazard, "prevented fall" tally for B). Keep it
   visually walled-off (a different texture/border) and labelled "privileged — not visible
   to model or clinician," to teach the leak-isolation design.

---

## 4. Technical constraints

- **One `.html` file.** Inline everything. No external fonts/CDNs that require network at
  runtime — if you use a distinctive webfont, embed it or pick a close, self-hosted-style
  fallback stack; degrade gracefully offline.
- Vanilla JS + CSS only (no framework). SVG or Canvas for charts/graph — your choice.
- Respect `prefers-reduced-motion` (offer a calmer mode).
- Fully keyboard-accessible transport; ARIA labels on interactive controls.
- Data object hard-coded from §2. Do not fabricate clinical thresholds beyond those given
  (Moderate 0.15, High 0.35).
- Persistent, tasteful **"SYNTHETIC DATA — NOT FOR CLINICAL USE"** watermark.

## 5. Deliverable
A finished, striking single-file animation that a non-technical reviewer can scrub through
and immediately grasp: *how each resident's fall risk evolves, when and why alarms fire,
how the P08 intervention changes the outcome, and that the hidden experiment never leaks
onto the clinical surface.* Make it feel authored, not generated.
