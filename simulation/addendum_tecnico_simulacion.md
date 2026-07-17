# Addendum técnico a la simulación multi-agente
### Institución en Sevilla · interacción social · caso ciego de caída · checkpoints · caso límite

---

## 1. La institución, anclada en datos reales de Sevilla/Andalucía

En lugar de inventar ratios de personal, se usan los valores reales que rigen
los centros residenciales en Andalucía, con las fuentes citadas. Esto importa
porque el ratio de personal —especialmente por la noche— es en sí mismo un
factor de riesgo de caída que el modelo debería poder captar indirectamente
(menos supervisión → menor tiempo de respuesta → mayor riesgo real).

**Marco normativo aplicable:** Decreto 388/2010 (Andalucía) y, en transición
hasta 2030, el marco estatal de 2022 ("Acuerdo Belarra"), que eleva
progresivamente el ratio general de atención directa a 0,51 trabajador/plaza
(1 por cada 2 residentes). Un hallazgo relevante de un informe sindical (CCOO)
sobre residencias andaluzas es que **el 55,8% de los centros sigue acogiéndose
a la normativa anterior, menos exigente (1997)**, y que un número no menor de
residencias **no cuenta con personal de enfermería en el turno de noche en
absoluto** — se ha incorporado explícitamente a la institución simulada, ya
que es una condición realista y clínicamente relevante, no un empeoramiento
artificial.

**Perfil actualizado de la institución — Residencia "Los Almendros", Sevilla:**

- **Ubicación:** zona urbana de alta densidad de Sevilla (p. ej. entorno
  Nervión/Bellavista, a efectos de la simulación). Por normativa estatal de
  2022, los centros en zona urbana de alta densidad no pueden superar 120
  plazas.
- **Tamaño real del centro:** 84 plazas, organizado en módulos de convivencia
  de hasta 25 plazas cada uno (requisito SEGG/normativa autonómica). **Los 10
  residentes simulados constituyen un módulo dentro de ese centro mayor** —
  esto es importante porque los ratios de personal especializado (médico,
  fisioterapeuta, trabajador social) se calculan sobre el total del centro
  (84 plazas), no sobre el módulo de 10, y por tanto su disponibilidad real
  para nuestros 10 residentes es proporcionalmente escasa, tal como ocurre en
  la práctica.
- **Enfermería titulada (DUE):** ratio autonómico ≈ 0,04/residente (1 por
  cada 25) → ≈ 3,4 profesionales de enfermería en plantilla para todo el
  centro, repartidos en turnos. **Turno de noche (23:00–07:00): sin
  presencia de enfermera titulada** en este centro simulado (consistente con
  la práctica real documentada); cualquier incidencia nocturna se gestiona
  con auxiliares presentes + protocolo de escalado telefónico a 112/urgencias.
- **Auxiliares de geriatría/gerocultores:** ratio ≈ 0,39 (norma 1997, aún
  mayoritaria). Por turno, aplicado al módulo de 10 residentes:
  - Mañana (7:00–15:00): 1 auxiliar por cada 5–8 residentes → 1–2 auxiliares
    presentes en el módulo.
  - Tarde (15:00–22:00): 1 por cada 7–10 → 1 auxiliar presente.
  - Noche (22:00–7:00): 1 por cada 10–14, con **mínimo absoluto de 2
    personas en todo el centro** (no por módulo) → en la práctica, el módulo
    de 10 puede quedar sin auxiliar dedicado varios intervalos de la noche,
    cubierto solo por rondas del personal de guardia del centro completo.
- **Fisioterapeuta, terapeuta ocupacional, trabajador social:** ratio ≈ 1
  profesional de cada por cada 100 residentes → para un centro de 84 plazas,
  esencialmente **una persona de cada, a tiempo parcial, compartida por todo
  el centro**. Traducido al módulo de 10: cobertura de fisioterapia 3
  días/semana, mañanas (como ya se había asumido, ahora justificado por el
  ratio real).
- **Médico responsable:** mismo ratio (~1/100), visita presencial 2
  días/semana + guardia telefónica el resto — igual que en la versión
  anterior, ahora anclado en la normativa real.

Esta fundamentación no cambia sustancialmente los números ya usados en la
versión anterior del documento, pero sí añade dos elementos realistas nuevos
que conviene explotar en la simulación: **(a) la ausencia real de enfermería
nocturna**, y **(b) la escasez de fisioterapia/medicina compartida entre
módulos**, que hace que la priorización de a qué residente atiende antes el
fisioterapeuta ese día sea un recurso limitado y con impacto real en la
simulación (el orquestador puede decidir a qué residentes del módulo llega
la sesión de fisioterapia cuando hay más demanda de la que caben en el hueco
disponible).

---

## 2. Interacción entre agentes (realismo social)

Simular diálogos completos entre los 10 agentes cada día sería costoso y
metodológicamente ruidoso (ver la nota de factibilidad previa). Se propone un
mecanismo de dos niveles que da realismo social sin disparar el coste:

### 2.1 Matriz de afinidad social (estática, definida una vez)

Antes de iniciar la simulación, se define una matriz 10×10 de afinidad
social, derivada directamente de las fichas de personalidad ya escritas:

| | P01 | P02 | P03 | P04 | P05 | P06 | P07 | P08 | P09 | P10 |
|---|---|---|---|---|---|---|---|---|---|---|
| Ejemplo de lectura | — | baja | media | alta | alta | media | baja | media | ninguna* | media |

*P09 (deterioro cognitivo avanzado) no inicia interacción social activa con
otros residentes, pero puede ser objeto de interacción iniciada por otros
(p. ej. P04, el "conector social", suele sentarse con ella).

Reglas para construirla (aplicadas una vez, no por el LLM cada día):
- P04 (sociable, "conector") tiene afinidad alta con casi todos.
- P02 (reservado) y P07 (ansiosa en grupo) tienen afinidad baja con la
  mayoría, salvo interacciones puntuales con personal, no con otros
  residentes.
- P01 y P05 (ambas con movilidad limitada por razones distintas) tienen
  afinidad media-alta entre sí — comparten actividades sedentarias
  (tejer, conversar) de forma natural.
- P09 solo recibe interacción, no la inicia.

### 2.2 Generación diaria de eventos sociales (barato, determinista + LLM ligero)

Cada día simulado, **antes** de llamar a los 10 agentes individuales:

1. Un muestreo determinista ponderado por la matriz de afinidad decide qué
   pares/grupos interactúan ese día (p. ej. "desayuno compartido P01–P05",
   "P04 se sienta con P09 en el jardín"), sujeto a las restricciones de la
   institución ese día (actividad grupal programada, disponibilidad de
   personal para trasladar a residentes con movilidad reducida, etc.).
2. Solo esos eventos —2 a 4 interacciones/día típicamente entre 10
   residentes— se resumen en una única llamada ligera al LLM (o incluso una
   plantilla determinista) que genera una frase descriptiva breve por
   interacción (para dar color narrativo al panel clínico y al bot), **sin
   generar diálogo completo**.
3. El resultado —una lista corta de eventos sociales del día— se añade al
   contexto de cada agente-paciente afectado antes de su llamada individual,
   de forma que su decisión de movilidad/ánimo pueda reflejar coherentemente
   "hoy socialicé" o "hoy no tuve compañía".

### 2.3 Excepción: hilo narrativo enriquecido para el caso de caída oculta

Para el paciente designado como caso de caída (sección 3), y solo para él y
su vecino social más próximo según la matriz de afinidad, se permite una
interacción más rica (2–3 turnos simulados) en los días inmediatamente
anteriores al evento — esto añade valor narrativo para la demo sin encarecer
el conjunto de la simulación, ya que se aplica a un único par de agentes, no
a los 10.

### 2.4 Persistencia

Nueva colección `social_interactions`: `day`, `participants` (lista de
patient_id), `type` (grupal/pareja), `summary_text`, `initiated_by`. Se
valida igual que el resto de estructuras (sección 4).

---

## 3. Caso ciego de caída: diseño experimental

### 3.1 Paciente seleccionado

**P08 — Joaquín**, no P06. Se elige deliberadamente **el caso menos obvio**
de los diez: su riesgo real (neuropatía periférica, alteración de la
sensibilidad plantar) **no se refleja en el volumen agregado de pasos**, que
se mantiene alto y estable — es precisamente el tipo de caso donde un modelo
que mira solo "¿bajaron los pasos?" fallaría. Usar el caso más obvio (P06,
con el cambio de diurético ya escrito como pista explícita) sería una prueba
poco exigente del modelo.

### 3.2 Mecanismo: riesgo latente oculto + evento estocástico, no un guion fijo

Para que el experimento sea metodológicamente honesto (no "programar" la
caída como un evento determinista que luego "se descubre" trivialmente), se
diseña así:

1. **Estado latente oculto** (invisible para el modelo, el panel y el bot;
   solo lo mantiene el orquestador): a partir de un día `D` elegido
   aleatoriamente dentro de la ventana de simulación (semilla fija para
   reproducibilidad, pero no revelada al equipo que analiza los resultados
   hasta después), el "verdadero" riesgo de caída de P08 empieza a subir de
   forma progresiva y sutil — cambios en la técnica de marcha (velocidad
   ligeramente reducida, apoyo alterado) que el propio agente-persona no
   necesariamente verbaliza como "notable_event" (coherente con su
   personalidad: independiente, resta importancia a sus síntomas).
2. **El evento de caída no es determinista**: cada día a partir de `D`, la
   probabilidad real (oculta) de que ocurra una caída ese día se deriva de
   ese estado latente mediante una función de riesgo (hazard) creciente pero
   con un techo razonable — se muestrea estocásticamente, no se fuerza. Esto
   evita el problema de "sabemos que va a caer el día 23 porque lo escribimos
   así", que invalidaría la prueba.
3. **Rama experimental A/B (la parte que responde a la pregunta real del
   encargo — "¿evitar la caída con una decisión distinta?"):**
   - **Rama A (control, sin intervención):** la simulación corre tal cual;
     si el muestreo estocástico dispara la caída, se registra el día y las
     circunstancias.
   - **Rama B (con intervención guiada por el gemelo digital):** se ejecuta
     una segunda simulación, **con la misma semilla aleatoria subyacente**
     (para que sea comparable), pero con una regla adicional: si en algún
     día el riesgo a 24h o a 7 días que reporta el modelo para P08 cruza un
     umbral definido, se activa automáticamente una intervención predefinida
     (p. ej. valoración podológica/ajuste de calzado + sesión de fisioterapia
     adicional centrada en equilibrio), que **modifica el estado latente
     real** de P08 (reduce el hazard subyacente) a partir de ese día.
   - **Resultado a reportar:** ¿ocurre la caída en la Rama A? ¿Se evita (o
     se retrasa, o se reduce su severidad esperada) en la Rama B? Esto es lo
     que responde de forma rigurosa a si "una decisión distinta basada en el
     gemelo digital" habría cambiado el desenlace — no una afirmación
     narrativa, sino una comparación de dos ejecuciones controladas.

### 3.3 Qué se audita después

- ¿En qué día el modelo cruzó por primera vez un umbral de alerta para P08 en
  la Rama A (sin intervención)? ¿Cuántos días antes de la caída real
  (oculta) fue eso — o no la detectó en absoluto (falso negativo)?
- En la Rama B, ¿la intervención se disparó a tiempo (antes del día `D`
  oculto donde el hazard empieza a subir) o demasiado tarde?
- Esto se registra en una colección separada `ground_truth_evaluation`, con
  acceso restringido — no debe filtrarse a las tablas que alimenta el
  modelo o el panel clínico, o se contaminaría el propio experimento.

---

## 4. Checkpoints y validación en cada día simulado

Pipeline obligatorio antes de avanzar de un día simulado al siguiente —
ninguno de estos pasos es opcional:

1. **Validación de esquema JSON** de cada una de las 10 respuestas de agente
   (sección 0 del documento anterior) — campos presentes, tipos correctos,
   `mobility_pct_of_baseline` dentro de [0, 2] (permite hasta el doble de la
   línea base, rechaza valores absurdos). Si falla: un reintento con
   temperatura reducida; si vuelve a fallar, se usa un valor por defecto
   (igual al día anterior) y se marca el registro como `agent_output_invalid`
   para revisión posterior — **nunca se detiene silenciosamente la
   simulación ni se inventa un valor sin marcarlo**.
2. **Validación de las interacciones sociales del día**: los `participants`
   deben ser IDs válidos de la cohorte, sin auto-interacción, y el número de
   interacciones/día debe estar dentro de un máximo razonable (evita que un
   fallo genere, p. ej., 40 interacciones simultáneas).
3. **Validación biológica de los valores de sensor resultantes** (ya
   especificada en el documento técnico general): rangos fisiológicos,
   coherencia de la suma de horas ≤24h, wear-time mínimo, detección de saltos
   imposibles día a día. Aplica igual a los datos modulados por agentes que a
   los generados de forma puramente paramétrica.
4. **Validación de la salida del modelo (riesgo de caída)**: la probabilidad
   debe estar en [0,1], sin `NaN`/`Inf`; se registra un aviso (no un error
   duro) si el riesgo a 7 días es menor que el riesgo a 24h para el mismo
   paciente el mismo día, ya que en el diseño del modelo pooled-logistic esto
   es posible pero infrecuente y merece revisión si ocurre de forma
   sistemática; se registra igualmente un aviso si el riesgo de un paciente
   cambia más de X puntos porcentuales en un solo día sin un
   `notable_event`/intervención que lo explique.
5. **Puerta de fin de día ("day gate")**: el día `t+1` solo se procesa si los
   cuatro pasos anteriores han pasado (o han sido marcados y resueltos con el
   valor por defecto documentado). Todos los resultados de la puerta —éxito,
   aviso o fallo— se escriben en una colección `daily_checkpoint_log` con
   marca de tiempo, para que la simulación completa sea auditable día a día,
   no solo al final.

---

## 5. Caso límite: brote de gripe estacional

Se define como un **escenario independiente** (`scenario: flu_outbreak`),
ejecutado por separado del escenario base (`scenario: baseline`, que incluye
el experimento A/B de la sección 3). Mismos 10 agentes e institución; distinto
guion de eventos.

**Diseño del brote:**

- **Día de inicio:** configurable, por defecto día 10 de la simulación.
- **Residentes afectados:** P06 (ya con insuficiencia cardiaca — mayor
  vulnerabilidad realista a complicaciones) y P09 (deterioro cognitivo
  avanzado, mayor fragilidad basal). Elegidos por ser los más vulnerables de
  la cohorte, como sería esperable en un brote real.
- **Efecto en los pacientes afectados** (inyectado como contexto adicional en
  su prompt diario durante 5–8 días): frecuencia cardiaca en reposo elevada
  varios puntos por encima de su línea base, movilidad reducida
  marcadamente, menor participación social, posible pérdida de apetito. Estos
  valores deben seguir pasando la validación biológica de rango (una FC
  elevada por fiebre es fisiológicamente plausible, no debe marcarse como
  error de sensor) — **este caso límite sirve precisamente para comprobar
  que la capa de validación distingue "elevado pero plausible por causa
  clínica conocida" de "artefacto de sensor"**, un objetivo explícito de la
  prueba.
- **Efecto institucional simultáneo:** el brote también reduce la plantilla
  disponible (bajas del propio personal), aplicado como evento institucional
  ya contemplado en el diseño (sección 1 del documento original) — durante el
  brote, el ratio de auxiliares se reduce un 20–30% adicional sobre el ya
  ajustado basal, afectando indirectamente a los 8 residentes no enfermos
  también (menos tiempo de paseo asistido disponible para todos).
- **Qué se espera observar y validar:** que el riesgo de caída de P06 y P09
  suba de forma transitoria durante el brote y **vuelva a su nivel basal
  tras la recuperación** (no debe quedar una escalada permanente injustificada
  una vez resuelto el cuadro agudo) — esto es en sí mismo un checkpoint de
  calidad del modelo, no solo de los datos.

---

## 6. Resumen de lo que queda listo antes del documento maestro

- Institución anclada en normativa real de Sevilla/Andalucía, con la ausencia
  de enfermería nocturna incorporada explícitamente como condición realista.
- Mecanismo de interacción social de dos niveles (matriz de afinidad +
  generación ligera diaria), con hilo narrativo enriquecido solo para el caso
  de caída.
- Diseño experimental A/B para el caso ciego de caída (P08), con estado
  latente oculto y evento estocástico — no un guion determinista — que
  permite responder si una intervención guiada por el modelo habría evitado
  o mitigado la caída.
- Pipeline de checkpoints obligatorio en cada día simulado, con registro
  auditable de éxitos, avisos y fallos.
- Escenario independiente de brote de gripe como caso límite, con efecto
  combinado en pacientes e institución.

Con esto, el documento maestro para Claude Code puede construirse sobre una
base ya validada conceptualmente, en lugar de dejar estas decisiones para
resolverse ad hoc durante la implementación.
