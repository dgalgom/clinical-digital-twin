# Panel matutino del clínico
### Pantalla que se ve al iniciar sesión cada mañana, tras la sincronización nocturna de datos

Este documento especifica el contenido y la disposición de la pantalla
"/panel" para que Claude Code la implemente en `app.R` (Shiny). Es la vista
por defecto al iniciar sesión — el punto de entrada diario del clínico, no un
informe que haya que ir a buscar.

---

## Principio de diseño

La pregunta que esta pantalla debe responder en menos de 10 segundos de
lectura es: **"¿a quién tengo que atender primero hoy, y por qué?"** — no
"aquí tienes todos los datos de 10 residentes para que los explores". El
orden de la información sigue esa prioridad: alertas primero, cohorte
completa después.

---

## Estructura de la pantalla (de arriba a abajo)

### 1. Cabecera

```
┌──────────────────────────────────────────────────────────────────┐
│  Residencia Los Almendros · Módulo A (10 residentes)              │
│  Panel matutino — martes, 14 de julio · datos sincronizados 06:40 │
│  Cobertura de datos anoche: 9/10 residentes con wear-time ≥ 8h    │
│  ⚠ P03 — Carmen: solo 5,2h de registro (dispositivo retirado a    │
│     las 02:15, sin retomar)                                       │
└──────────────────────────────────────────────────────────────────┘
```

- Fecha y hora de la última sincronización — el clínico debe poder confiar
  en que estos datos son de esta noche, no de hace tres días.
- **Indicador de cobertura de datos**, no solo de riesgo: si un residente
  tiene datos incompletos, el clínico debe saberlo antes de fiarse de un
  riesgo "bajo" que en realidad es "sin datos suficientes para calcularlo
  bien".

### 2. Bloque de alertas (lo primero, siempre visible sin hacer scroll)

Ordenadas por severidad, no por orden alfabético ni por ID de paciente.

```
┌──────────────────────────────────────────────────────────────────┐
│  🔴 ALERTAS DE HOY (2)                                             │
│                                                                    │
│  P08 · Joaquín          Riesgo 7 días: 12% → 27% (▲15 pts)        │
│    Motivo: tendencia de pasos estable pero cadencia de marcha     │
│    reducida 3 días consecutivos. Sin evento notable reportado.    │
│    [ Ver ficha ]  [ Simular qué-pasa-si ]  [ Marcar revisado ]    │
│                                                                    │
│  P06 · Francisco        Riesgo 24h: 6% → 19% (▲13 pts)            │
│    Motivo: patrón ortostático más marcado desde el día 3 tras     │
│    inicio de nuevo diurético. 2 episodios de mareo al levantarse. │
│    [ Ver ficha ]  [ Simular qué-pasa-si ]  [ Marcar revisado ]    │
│                                                                    │
│  🟡 AVISOS DE CALIDAD DE DATO (1)                                  │
│  P03 · Carmen — cobertura nocturna insuficiente (ver cabecera)    │
└──────────────────────────────────────────────────────────────────┘
```

- Cada alerta muestra **el cambio (delta), no solo el valor absoluto** — un
  27% de riesgo dice poco sin saber si ayer era 25% o 12%.
- Cada alerta incluye **el motivo en una frase**, citando la(s) variable(s)
  que más ha cambiado — nunca un número sin explicación.
- Tres acciones directas por alerta: ver la ficha completa, abrir el
  simulador what-if ya precargado con ese paciente, o marcar como revisada
  (quedando registrada la revisión, quién y cuándo — trazabilidad).
- Los avisos de calidad de dato van en su propio bloque, con color distinto
  (ámbar, no rojo) — no deben mezclarse visualmente con alertas clínicas
  reales, mezclar ambas cosas es lo que causa fatiga de alarma.

### 3. Tabla de cohorte completa (los 10 residentes)

```
┌────┬───────────┬───────────┬───────────┬────────────┬────────────────┐
│    │ Residente │ Riesgo    │ Riesgo    │ Tendencia  │ Último evento  │
│    │           │ 24h       │ 7 días    │ 7 días     │ notable        │
├────┼───────────┼───────────┼───────────┼────────────┼────────────────┤
│ 🔴 │ P08 Joaquín│    9%     │   27%     │   ▲▲       │ (ver alerta)   │
│ 🔴 │ P06 Franc. │   19%     │   24%     │   ▲        │ (ver alerta)   │
│ 🟡 │ P07 Isabel │    7%     │   15%     │   →        │ somnolencia AM │
│ 🟢 │ P01 Rosario│    4%     │    9%     │   →        │ —              │
│ 🟢 │ P02 Antonio│    5%     │   11%     │   →        │ —              │
│ 🟡 │ P03 Carmen │    —      │    —      │  dato incompleto           │
│ 🟢 │ P04 Manuel │    2%     │    5%     │   →        │ —              │
│ 🟢 │ P05 Dolores│    6%     │   13%     │   ▼        │ menor dolor    │
│ 🔴 │ P09 Pilar  │   22%     │   31%     │   →  (basal)│ agitación noct.│
│ 🟢 │ P10 Vicente│    8%     │   16%     │   ▼▼       │ mejora prog.   │
└────┴───────────┴───────────┴───────────┴────────────┴────────────────┘
   Ordenado por riesgo a 7 días (descendente). Clic en fila → ficha del residente.
```

- **Orden por defecto: riesgo a 7 días descendente** — quien más lo necesita,
  arriba, sin que el clínico tenga que ordenar nada.
- Código de color de una sola letra visual (🔴/🟡/🟢) según el nivel de riesgo
  configurado por la institución — no depende de que el clínico interprete
  el número.
- **P09 (riesgo alto pero estable, "→")** se muestra de forma distinguible de
  P08/P06 (riesgo alto **y subiendo**) — un riesgo alto pero estable no es
  una alerta activa, es un estado basal conocido; mezclarlo con las alertas
  nuevas sería ruido.
- Fila de P03 muestra explícitamente "dato incompleto" en vez de un riesgo
  calculado con datos insuficientes — nunca se debe mostrar un número que
  parezca fiable si no lo es.

### 4. Barra de acceso rápido (pie de la tabla, siempre visible)

```
[ Abrir simulador qué-pasa-si ]   [ Preguntar al bot (Telegram) ]   [ Exportar resumen de turno ]
```

- "Exportar resumen de turno" genera el mismo contenido de esta pantalla en
  formato imprimible/compartible — para el cambio de turno con el personal
  que no usa el dashboard directamente.

---

## Comportamiento y reglas de negocio

- La pantalla se recalcula automáticamente tras cada sincronización nocturna
  (no requiere que el clínico pulse "actualizar").
- Una alerta desaparece del bloque de "Alertas de hoy" en cuanto se marca
  como revisada, pero el residente permanece visible en la tabla de cohorte
  con su nivel de riesgo real — revisar una alerta no oculta al paciente,
  solo reconoce que el clínico ya lo ha visto.
- Si no hay ninguna alerta activa, el bloque de alertas se sustituye por un
  mensaje breve y neutro ("Sin alertas nuevas desde ayer") — nunca se deja
  vacío sin explicación, para que quede claro que el sistema sí ha
  comprobado y no que se ha olvidado de mostrar algo.
- Los datos de esta pantalla, al ser parte de la simulación con agentes,
  deben excluir explícitamente cualquier información de la colección
  `ground_truth_evaluation` (ver addendum técnico, §3.3) — el panel clínico
  nunca debe tener acceso a la verdad oculta del experimento de caída, o se
  invalidaría la prueba.

---

## Nota para Claude Code

Esta pantalla reutiliza directamente `cdt_cohort_snapshot()` (ya existente en
`R/service.R`) para la tabla de cohorte, y requiere una nueva función de
servicio (p. ej. `cdt_daily_alerts()`) que calcule los deltas día-a-día por
paciente y aplique el umbral de alerta configurado en `R/config.R`, coherente
con el mecanismo de alertas ya propuesto en el plan de mejora previo del
proyecto (triaje de turno).
