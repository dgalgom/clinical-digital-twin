# Simulación multi-agente de un mes — Documento maestro para Claude Code
### Gemelo digital clínico · Residencia "Los Almendros", Sevilla · 10 residentes

Este documento se entrega **junto con** tres archivos ya preparados, que
Claude Code debe leer primero y tratar como parte de esta especificación,
no como referencia opcional:

- `simulacion_agentes_institucion_pacientes.md` — perfil de la institución
  (versión base) y las 10 fichas de agentes-paciente (system prompts).
- `addendum_tecnico_simulacion.md` — institución anclada en normativa real
  de Sevilla/Andalucía, mecanismo de interacción social, diseño del
  experimento ciego de caída (P08), pipeline de checkpoints, caso límite de
  gripe.
- `panel_matutino_clinico.md` — especificación de la pantalla "/panel" que
  ve el clínico cada mañana.

Trabaja sobre el repositorio R existente (`clinical-digital-twin`). Sigue las
convenciones ya establecidas: toda la lógica va en `R/`, los front-ends son
delgados, todo debe seguir siendo reproducible desde una semilla, y
`verify.R` / `tests/run_tests.R` deben seguir pasando al final.

---

## 1. Alcance de esta simulación

- **Duración:** 30 días simulados, línea temporal continua (no días
  aislados). El brote de gripe (sección 5 del addendum) ocurre embebido
  dentro de este mismo mes, hacia el día 15–18, afectando a P06 y P09 y, de
  forma más leve, a la disponibilidad de personal del módulo — **no se
  ejecuta como una simulación separada**; es un tramo dentro de la misma
  línea temporal.
- **Dos ramas de ejecución, misma semilla base:**
  - **Rama A (control):** sin intervención clínica activada por el modelo.
  - **Rama B (intervención guiada por el gemelo digital):** si el riesgo de
    P08 cruza el umbral definido en el addendum (§3.2), se dispara
    automáticamente la intervención predefinida y se modifica su estado
    latente oculto a partir de ese día.
  - Todo lo demás (los otros 9 pacientes, el brote de gripe, la institución,
    las interacciones sociales) debe ser **idéntico** entre ambas ramas,
    salvo por el efecto directo de la intervención sobre P08 — esto es lo
    que hace válida la comparación.
- **Objetivo de la simulación principal:** no es solo "generar datos
  sintéticos más realistas". Es responder a dos preguntas concretas:
  1. ¿El modelo (gemelo digital) detecta con antelación razonable el riesgo
     creciente de P08, un caso deliberadamente sutil?
  2. ¿Una intervención disparada por esa detección habría evitado o mitigado
     la caída, comparado con no intervenir?
- **Además de esta simulación principal**, se ejecutan **simulaciones
  adicionales independientes**, más cortas y centradas cada una en un modo
  de fallo distinto, para someter al sistema a condiciones de estrés que la
  simulación principal no cubre por diseño. Se especifican en la sección 8.
  Cada una es una ejecución aparte (no ramas del mismo experimento), con su
  propio informe corto — no comparten la semilla del experimento A/B de P08.

---

## 2. Pipeline diario (orden obligatorio, para cada día y cada rama)

```
1. Cargar contexto institucional del día
   (turno, ratio de personal disponible, eventos activos: ¿brote de gripe
   en curso?, ¿fisioterapeuta disponible hoy?, fin de semana/festivo)

2. Capa social (determinista + LLM ligero)
   - Muestrear interacciones del día según la matriz de afinidad
     (addendum §2.1–2.2)
   - Resumen breve por interacción (LLM ligero o plantilla)
   - Validar (IDs válidos, sin auto-interacción, máximo razonable/día)
   - Persistir en `social_interactions`

3. Llamada a cada uno de los 10 agentes-paciente (independiente entre sí)
   - Prompt = ficha fija del paciente + contexto institucional del día +
     resumen de sus últimos 3–5 días + interacciones sociales del día que le
     afecten + (si aplica) estado latente oculto de P08 traducido a
     contexto conductual sin revelar números crudos al propio agente
   - Salida: JSON estructurado (esquema en `simulacion_agentes_...md` §0)
   - VALIDAR esquema JSON (tipos, rangos declarados). Si falla: reintento
     con temperatura reducida; si vuelve a fallar, usar el valor del día
     anterior y marcar `agent_output_invalid`
   - Persistir en `agent_decisions` (decisión + prompt exacto usado, para
     poder auditar y reproducir sin volver a llamar al LLM)

4. Generación de la señal de sensor (motor estadístico existente)
   - `synthetic_sensors.R` consume las decisiones conductuales de (3) como
     modificadores sobre la línea base de cada paciente
   - VALIDAR biológicamente cada valor resultante (rangos fisiológicos,
     suma de horas ≤24h, wear-time mínimo, saltos imposibles día a día) —
     ver `explicacion_tecnica_sistema_gemelo_digital.md` §3 para las reglas
     exactas ya definidas
   - Persistir en `sensor_readings` con sus `quality_flags`

5. Inferencia del gemelo digital (para los 10 pacientes)
   - Ejecutar `predict_fall_risk()` a 24h y 7 días para cada paciente con
     los datos ya validados del día
   - VALIDAR la salida: rango [0,1], sin NaN/Inf, aviso si el salto diario
     de riesgo es grande sin evento que lo explique
   - Persistir en `model_predictions`

6. (Solo Rama B) Comprobación de umbral e intervención
   - Si el riesgo de P08 cruza el umbral definido (addendum §3.2) y la
     intervención aún no se ha activado: activarla, registrar el día y
     modificar su estado latente oculto a partir de este punto

7. (Motor oculto del orquestador, invisible para el resto del pipeline)
   - Actualizar el estado latente real de P08 (addendum §3.2)
   - Muestrear estocásticamente si ocurre la caída hoy
   - Si ocurre: registrar en `ground_truth_evaluation` (día, rama,
     circunstancias) — **nunca escribir esto en `fall_events` ni en
     ninguna tabla visible para el modelo, el panel o el bot**

8. Puerta de fin de día ("day gate")
   - Todos los pasos 2–5 deben haber pasado (o quedar marcados/resueltos)
   - Registrar el resultado del día en `daily_checkpoint_log`
     (éxito/aviso/fallo por cada sub-validación)
   - Solo entonces avanzar a `t+1`
```

---

## 3. Requisitos de validación (resumen ejecutable)

Implementar como funciones R independientes y testeables, no como código
inline dentro del bucle de simulación:

- `validate_agent_json(response, schema)` → boolean + lista de errores
- `validate_social_interactions(events, valid_ids, max_per_day)` → boolean +
  errores
- `validate_biological_plausibility(sensor_row)` → boolean + `quality_flags`
  (reutilizar/extender las reglas ya definidas en el documento técnico
  general, sección 3)
- `validate_model_output(prediction)` → boolean + avisos
- `run_daily_checkpoint_gate(day, branch)` → agrega los cuatro anteriores,
  escribe en `daily_checkpoint_log`, devuelve si el día puede considerarse
  válido para avanzar

Cada una de estas funciones necesita su propio test en `tests/testthat/`,
igual que el resto del proyecto — no son un añadido opcional, son parte del
mismo estándar de calidad que ya exige `verify.R`.

---

## 4. Caso límite embebido: brote de gripe

Tal como se especifica en el addendum (§5), pero recordando el punto
importante de esta clarificación: **no se ejecuta como una simulación
aparte**. Se activa como un evento institucional dentro del mismo mes
(días ~15–22 por defecto, configurable), afectando a P06 y P09
directamente y al ratio de personal del módulo de forma más leve para el
resto. Debe aplicarse **de forma idéntica en ambas ramas (A y B)**, ya que
no está relacionado con el experimento de P08 — sirve para comprobar que el
sistema distingue una escalada de riesgo transitoria y clínicamente
explicada de una escalada crónica, y que el riesgo vuelve a su nivel basal
tras la recuperación.

---

## 5. Informe auditable final (el entregable principal de esta fase)

Al terminar las 30 días × 2 ramas, generar un informe (`.html` vía R
Markdown, coherente con el resto del stack del proyecto) con:

### 5.1 Resumen ejecutivo (primera página)
- Duración simulada, nº de pacientes, nº de días con checkpoint limpio vs.
  con avisos vs. con fallos (tabla simple).
- Resultado del experimento P08: ¿ocurrió la caída en la Rama A? ¿En la
  Rama B? Día exacto (o "no ocurrió" si el muestreo estocástico no la
  disparó en 30 días — resultado igualmente válido y debe reportarse como
  tal, no forzarse).
- Día en que el modelo cruzó por primera vez el umbral de alerta para P08 en
  cada rama, y cuántos días de antelación supuso eso respecto al inicio del
  ascenso del riesgo latente oculto.

### 5.2 Una sección por paciente (10 secciones, misma estructura)
Para cada paciente, un gráfico principal:

- **Evolución del riesgo a 24h y a 7 días a lo largo del mes** (dos líneas,
  mismo eje temporal), con anotaciones verticales para: inicio/fin del brote
  de gripe (si le afecta), intervenciones registradas, avisos de calidad de
  dato relevantes, y —solo para P08 y solo en la versión final del
  informe, nunca antes— el día real de la caída oculta si ocurrió.
- Debajo del gráfico, una tabla compacta con las variables de sensor clave
  (pasos, FC reposo, PA, horas sedentarias) resumidas por semana, no día a
  día, para mantener el informe legible.
- Una línea de texto breve resumiendo el patrón observado ese mes para ese
  paciente (generada de forma determinista a partir de los datos, no por el
  LLM, para evitar que el informe de auditoría dependa a su vez de una
  fuente no determinista).

### 5.3 Apéndice de trazabilidad
- Enlace/referencia a `daily_checkpoint_log` completo.
- Conteo de cuántas decisiones de agente requirieron reintento o valor por
  defecto (`agent_output_invalid`), por paciente — una tasa alta para un
  paciente concreto es en sí misma una señal a revisar (¿su ficha genera
  respuestas inconsistentes del LLM?).

**Requisito de diseño no negociable:** este informe es el único lugar donde
se revela el contenido de `ground_truth_evaluation`. La pantalla `/panel`
(especificada en `panel_matutino_clinico.md`) y el bot de Telegram no deben
tener acceso a esa colección en ningún momento durante la simulación —
verificarlo explícitamente con un test (`test_ground_truth_not_leaked.R` o
similar) que confirme que ninguna consulta usada por el panel o el bot
referencia esa colección.

---

## 8. Simulaciones adicionales de estrés (casos límite anticipados)

Cada una es una ejecución **corta e independiente** (7–10 días simulados,
no 30), sobre los mismos 10 agentes y la misma institución, con un único
evento disparador distinto por escenario. El objetivo no es narrativo como
en la simulación principal, sino **encontrar dónde se rompe el sistema**:
qué capa de validación falla, qué parte de la pipeline no está preparada, o
qué parte de la interfaz clínica se degrada bajo condiciones anómalas.
Genera un informe corto por escenario (misma estructura que la sección 5,
sin necesidad del apéndice de trazabilidad completo).

### 8.1 Corrupción de datos de sensor (fallo de hardware, no de comportamiento)
**Qué prueba:** la capa de validación biológica (`validate_biological_plausibility`),
no el modelo. Inyectar directamente, sin pasar por ningún agente, valores de
sensor imposibles para un paciente concreto en un día concreto: p. ej. 42.000
pasos, presión sistólica de 310 mmHg, o una suma de horas sentado/de
pie/caminando/acostado que exceda 24h. **Comportamiento esperado:** el
registro se marca con `quality_flags`, no se descarta silenciosamente, y
**no** se usa para inferencia del modelo ese día — verificar explícitamente
que `predict_fall_risk()` no recibe el valor corrupto.

### 8.2 Fallo sostenido del LLM (no un solo día, varios consecutivos)
**Qué prueba:** el mecanismo de reintento/fallback (`validate_agent_json` +
la lógica de "usar el valor del día anterior") bajo fallo **repetido**, no
puntual. Forzar que un agente concreto devuelva JSON inválido o que la
llamada falle (timeout simulado) durante 4–5 días seguidos.
**Comportamiento esperado:** el sistema no debe congelar indefinidamente al
paciente en el mismo valor sin marcarlo como cada vez más obsoleto; a partir
de cierto número de días consecutivos con `agent_output_invalid`, debe
escalar de "aviso" a "fallo" en el `daily_checkpoint_log` y generar una
alerta de calidad de dato visible en el panel (no una alerta clínica, una de
infraestructura) — decidir y documentar ese umbral explícitamente.

### 8.3 Hospitalización con ausencia total de datos y reingreso
**Qué prueba:** un vacío de datos **completo** (no solo wear-time bajo), muy
distinto de P10 (que ya vuelve de hospitalización al iniciar la simulación).
Elegir un paciente distinto y previamente estable (p. ej. P04) y simular su
hospitalización días 3–7: ausencia total de lecturas de sensor porque el
dispositivo no está físicamente en la institución. **Comportamiento
esperado:** el sistema no debe interpolar datos inventados durante el vacío,
debe mostrar explícitamente "sin datos — residente hospitalizado" en el
panel, y al reingresar (día 8) debe tratar su nueva línea base como
potencialmente distinta a la previa (desacondicionamiento post-hospitalario),
no comparar automáticamente contra el baseline de antes del ingreso como si
nada hubiera pasado.

### 8.4 Aislamiento social súbito
**Qué prueba:** si el sistema puede atribuir una escalada de riesgo a un
factor **no fisiológico**. Simular que las visitas de la hija de P01 cesan
abruptamente (evento institucional: "familiar no visita desde el día X"),
sin ningún otro cambio clínico. **Comportamiento esperado:** si el riesgo de
P01 sube en este escenario, la explicación generada (`cdt_feature_importance`
o equivalente) y el texto de la alerta deben poder señalar la caída de
interacción social como factor, no solo variables fisiológicas — si el
sistema no tiene forma de reflejar esto en la explicación, es un hallazgo de
la prueba en sí mismo (limitación a documentar), no algo que deba forzarse
artificialmente para que "funcione".

### 8.5 Crisis de personal a nivel de institución (más allá de la gripe)
**Qué prueba:** si el riesgo agregado del módulo completo responde a un
evento puramente institucional, sin ningún cambio clínico en ningún
residente. Simular una reducción drástica de plantilla (huelga o bajas
simultáneas no relacionadas con enfermedad de los residentes) durante 3–4
días, afectando a los 10 residentes por igual vía menor tiempo de paseo
asistido y menor supervisión. **Comportamiento esperado:** verificar que el
sistema puede mostrar una tendencia de riesgo al alza correlacionada
temporalmente entre varios residentes a la vez sin que haya un evento
clínico individual que lo explique en cada ficha — y que el panel no trata
esto como 10 alertas individuales no relacionadas, sino que idealmente las
agrupa o las contextualiza (si la implementación actual no lo hace, es otro
hallazgo a documentar, no a forzar).

### 8.6 Ráfaga de alertas simultáneas (estrés de interfaz, no clínico)
**Qué prueba:** el panel matutino y el `daily_checkpoint_log` bajo carga,
no el modelo clínico. Forzar sintéticamente (sin justificación narrativa,
es una prueba de sistema) que 6 de los 10 pacientes crucen su umbral de
alerta el mismo día. **Comportamiento esperado:** el bloque de alertas de
la sección 2 del panel matutino debe seguir siendo legible y ordenado por
severidad, no debe romper el layout ni provocar timeouts en la consulta
`cdt_daily_alerts()` — esta es una prueba de rendimiento/UI tanto como de
lógica.

### 8.7 Cierre de ficha a mitad de simulación (baja de un residente)
**Qué prueba:** un caso de ciclo de vida real y frecuente en una
institución (traslado a otro centro, alta, o fallecimiento) que el sistema
debe poder manejar sin errores. Simular que un residente (p. ej. P09) causa
baja del módulo en el día 5. **Comportamiento esperado:** su ficha deja de
generar nuevas predicciones ni alertas a partir de ese día, desaparece de
la tabla activa de cohorte del panel sin provocar errores en
`cdt_cohort_snapshot()`, pero su historial permanece consultable (no se
borra) para trazabilidad. Este es un caso puramente técnico de robustez del
ciclo de vida de los datos, no debe tratarse como un evento clínico a
explicar.

---

## 9. Reproducibilidad

- Semilla fija y documentada para: la simulación estocástica de la caída de
  P08, el muestreo de interacciones sociales, y cualquier parámetro
  aleatorio del generador estadístico de sensores.
- Cada llamada al LLM (agentes + capa social ligera) debe cachearse/loggearse
  de forma que la simulación completa sea reproducible **reproduciendo el
  log**, sin necesidad de volver a llamar a la API — importante tanto para
  auditar como para poder regenerar el informe sin coste adicional.
- El sistema debe poder ejecutarse en modo mock (sin `ANTHROPIC_API_KEY`)
  usando decisiones de agente deterministas de repuesto, igual que el resto
  del proyecto ya soporta mock mode — útil para que `verify.R` pueda seguir
  ejecutándose sin claves.

---

## 10. Entregables de esta fase

1. Código de la simulación (orquestador + las 5 funciones de validación de
   la sección 3) en `R/`, siguiendo la convención de módulos ya existente
   (p. ej. `R/simulation.R`, `R/simulation_validation.R`).
2. Las 10 fichas de agente y el perfil institucional cargados como
   configuración (no hardcodeados dentro del orquestador).
3. Script ejecutable (p. ej. `data-raw/run_simulation.R`) que corre las dos
   ramas completas y genera el informe final.
4. El informe `.html` resultante.
5. Actualización de `app.R` para que la pantalla de inicio de sesión sea el
   panel matutino especificado, usando los datos de la Rama A por defecto
   como demo (sin revelar nunca el resultado del experimento oculto).
6. Tests nuevos en `tests/testthat/` para las 5 funciones de validación y
   para la no-filtración de `ground_truth_evaluation`.
7. `verify.R` actualizado para incluir un modo rápido de esta simulación (p.
   ej. 3 días en vez de 30) como parte de la verificación end-to-end, sin
   que la verificación completa tarde demasiado.
8. Los 7 escenarios de estrés de la sección 8, cada uno como script
   independiente y reproducible (p. ej.
   `data-raw/stress_scenarios/8_1_sensor_corruption.R`, etc.), con su
   informe corto correspondiente. Para cada uno, un párrafo final en el
   informe indicando explícitamente si el comportamiento esperado se
   cumplió o si reveló una limitación real del sistema — ambos resultados
   son válidos y deben documentarse tal cual, no ajustarse a posteriori
   para que "todo pase".

Confirma el resultado de cada paso con el checkpoint correspondiente antes
de continuar al siguiente — no implementar el pipeline completo de una vez
sin poder validar cada pieza por separado.
