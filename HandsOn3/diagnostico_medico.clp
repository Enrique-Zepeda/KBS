;; Plantilla de enfermedades
(deftemplate enfermedad
  (slot nombre)
  (slot tipo)
  (slot signos)
  (slot sintomas))

;; Base de hechos inicial
(deffacts enfermedades-iniciales
  (enfermedad (nombre "Gripe") (tipo "Viral") (signos "Fiebre, dolor de cabeza") (sintomas "Tos, congestión nasal"))
  (enfermedad (nombre "Amigdalitis") (tipo "Bacterial") (signos "Dolor de garganta, fiebre") (sintomas "Dificultad al tragar"))
  (enfermedad (nombre "Neumonía") (tipo "Bacterial") (signos "Fiebre alta, dolor en el pecho") (sintomas "Dificultad para respirar"))
  (enfermedad (nombre "Varicela") (tipo "Viral") (signos "Erupciones en la piel, fiebre") (sintomas "Picazón, malestar general"))
  (enfermedad (nombre "Bronquitis") (tipo "Bacterial") (signos "Tos con flema, fiebre leve") (sintomas "Dificultad para respirar, dolor en el pecho"))
  (enfermedad (nombre "COVID-19") (tipo "Viral") (signos "Fiebre alta, tos seca") (sintomas "Cansancio, pérdida de olfato"))
  (enfermedad (nombre "Otitis") (tipo "Bacterial") (signos "Dolor de oído, fiebre") (sintomas "Irritabilidad, pérdida de audición"))
  (enfermedad (nombre "Gastroenteritis") (tipo "Viral") (signos "Vómitos, diarrea") (sintomas "Dolor abdominal, deshidratación"))
  (enfermedad (nombre "Tifoidea") (tipo "Bacterial") (signos "Fiebre persistente, debilidad") (sintomas "Dolor de cabeza, pérdida de apetito"))
  (enfermedad (nombre "Sarampión") (tipo "Viral") (signos "Erupciones cutáneas, fiebre alta") (sintomas "Conjuntivitis, tos persistente")))

;; Función para agregar enfermedades
(deffunction agregar-enfermedad (?nombre ?tipo ?signos ?sintomas)
   (assert (enfermedad
      (nombre ?nombre)
      (tipo ?tipo)
      (signos ?signos)
      (sintomas ?sintomas)))
   (printout t "Enfermedad " ?nombre " agregada exitosamente." crlf))


;; Regla para consultar enfermedades
(defrule buscar-enfermedad
  ?e <- (enfermedad (nombre ?nombre) (signos ?signos) (sintomas ?sintomas))
  =>
  (printout t "Enfermedad encontrada: " ?nombre crlf
            "Signos: " ?signos crlf
            "Síntomas: " ?sintomas crlf))

;; Función para actualizar enfermedades
(deffunction actualizar-enfermedad (?nombre ?nuevo-tipo ?nuevos-signos ?nuevos-sintomas)
  ;; Búsqueda del hecho mediante do-for-fact
  (do-for-fact ((?f enfermedad)) 
      (eq ?f:nombre ?nombre)
      (progn
         ;; Eliminar el hecho encontrado
         (retract (fact-index ?f))
         ;; Insertar la versión actualizada
         (assert (enfermedad
            (nombre ?nombre)
            (tipo ?nuevo-tipo)
            (signos ?nuevos-signos)
            (sintomas ?nuevos-sintomas)))
         ;; Mensaje de éxito
         (printout t "Enfermedad " ?nombre " actualizada exitosamente." crlf)
         ;; Finalizar el bucle
         (return)))
  ;; Si no se encuentra el hecho
  (printout t "Error: Enfermedad " ?nombre " no encontrada." crlf))


(deffunction borrar-enfermedad (?nombre)
  ;; Iterar sobre los hechos tipo 'enfermedad'
  (do-for-fact ((?f enfermedad))
      (eq ?f:nombre ?nombre)
      (progn
         ;; Eliminar el hecho
         (retract (fact-index ?f))
         ;; Mensaje de éxito
         (printout t "Enfermedad " ?nombre " eliminada exitosamente." crlf)
         ;; Finalizar el bucle
         (return)))
  ;; Si no se encuentra el hecho
  (printout t "Error: Enfermedad " ?nombre " no encontrada." crlf))



