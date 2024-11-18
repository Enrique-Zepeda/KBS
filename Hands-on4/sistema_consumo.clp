(deftemplate smartphone
  (slot marca)
  (slot modelo)
  (slot color)
  (slot precio))

(deftemplate computadora
  (slot marca)
  (slot modelo)
  (slot color)
  (slot precio))

(deftemplate tarjetacred
  (slot banco)
  (slot grupo)
  (slot exp-date))

(deftemplate cliente
  (slot nombre)
  (slot tipo)) ;; Tipo: menudista o mayorista

(deftemplate orden
  (slot producto) ;; Puede ser smartphone o computadora
  (slot modelo)
  (slot cantidad)
  (slot cliente)
  (slot metodo-pago)) ;; Ejemplo: Banamex, Liverpool VISA

(deftemplate vale
  (slot cliente)
  (slot cantidad)) ;; Cantidad de vales otorgados

(deftemplate inventario
  (slot producto) ;; Puede ser smartphone o computadora
  (slot modelo)
  (slot cantidad)) ;; Cantidad disponible en stock

(deffacts base-conocimientos
  ;; Smartphones
  (smartphone (marca "Apple") (modelo "iPhone16") (color "Rojo") (precio 27000))
  (smartphone (marca "Samsung") (modelo "Note21") (color "Negro") (precio 22000))

  ;; Computadoras
  (computadora (marca "Apple") (modelo "MacBookPro") (color "Gris") (precio 47000))

  ;; Inventario
  (inventario (producto "smartphone") (modelo "iPhone16") (cantidad 50))
  (inventario (producto "smartphone") (modelo "Note21") (cantidad 30))
  (inventario (producto "computadora") (modelo "MacBookPro") (cantidad 20))

  ;; Tarjetas de crédito
  (tarjetacred (banco "Banamex") (grupo "Oro") (exp-date "01-12-23"))
  (tarjetacred (banco "Liverpool") (grupo "VISA") (exp-date "15-11-24"))

  ;; Clientes
  (cliente (nombre "Juan Perez") (tipo "menudista"))
  (cliente (nombre "Ana Lopez") (tipo "mayorista"))
)

;; Promociones
(defrule promocion-banamex
  (orden (producto ?producto) (modelo ?modelo) (metodo-pago "Banamex"))
  =>
  (printout t "Promoción: 24 meses sin intereses para el producto " ?producto " modelo " ?modelo crlf))

(defrule promocion-liverpool
  (orden (producto ?producto) (modelo "Note21") (metodo-pago "Liverpool VISA"))
  =>
  (printout t "Promoción: 12 meses sin intereses para el producto " ?producto " modelo Note21." crlf))

(defrule promocion-regalo
  (orden (producto "smartphone") (modelo ?modelo) (cantidad ?cantidad))
  (test (>= ?cantidad 20))
  =>
  (printout t "Promoción: Regalo de una funda para el modelo " ?modelo crlf))

;; Clasificación de clientes
(defrule clasificar-clientes
  (orden (cliente ?cliente) (cantidad ?cantidad))
  (cliente (nombre ?cliente) (tipo ?tipo))
  (test (< ?cantidad 10))
  =>
  (printout t "Cliente " ?cliente " clasificado como Menudista (Cantidad: " ?cantidad ")." crlf))

(defrule clasificar-clientes-mayorista
  (orden (cliente ?cliente) (cantidad ?cantidad))
  (cliente (nombre ?cliente) (tipo ?tipo))
  (test (>= ?cantidad 10))
  =>
  (printout t "Cliente " ?cliente " clasificado como Mayorista (Cantidad: " ?cantidad ")." crlf))

;; Vales
(defrule otorgar-vales
  (orden (cliente ?cliente) (cantidad ?cantidad))
  (test (>= (* ?cantidad 1000) 10000)) ;; Cada 1000 pesos
  =>
  (bind ?vales (/ (* ?cantidad 1000) 1000))
  (assert (vale (cliente ?cliente) (cantidad ?vales)))
  (printout t "Cliente " ?cliente " recibe " ?vales " vales por su compra." crlf))

;; Actualización de stock
(deffunction actualizar-stock (?producto ?modelo ?cantidad)
  (do-for-fact ((?f inventario))
     (and (eq ?f:producto ?producto) (eq ?f:modelo ?modelo))
     (progn
        (bind ?nuevo-stock (- ?f:cantidad ?cantidad))
        (if (>= ?nuevo-stock 0) then
          (retract ?f)
          (assert (inventario (producto ?producto) (modelo ?modelo) (cantidad ?nuevo-stock)))
          (printout t "Stock actualizado para " ?producto " modelo " ?modelo ": " ?nuevo-stock crlf))
        else
          (printout t "Error: No hay stock suficiente para " ?producto " modelo " ?modelo crlf))))

;; Mostrar stock total
(deffunction mostrar-stock-total ()
   (printout t "Inventario actual:" crlf)
   (do-for-all-facts ((?f inventario)) ;; Ensure this loops through all inventario facts
      TRUE
      (printout t "Producto: " (fact-slot-value ?f producto)
                    ", Modelo: " (fact-slot-value ?f modelo)
                    ", Cantidad: " (fact-slot-value ?f cantidad) crlf)))





;; Eliminar órdenes procesadas
(defrule eliminar-orden
   ?orden <- (orden (producto ?producto) (modelo ?modelo) (cantidad ?cantidad) (cliente ?cliente) (metodo-pago ?metodo))
   =>
   (retract ?orden)
   (printout t "Orden procesada y eliminada: " ?producto " modelo " ?modelo crlf))

;; Validación de datos
(defrule validar-cantidad
   (orden (cantidad ?cantidad))
   (test (<= ?cantidad 0))
   =>
   (printout t "Error: La cantidad de la orden debe ser mayor a 0." crlf))
