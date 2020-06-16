-------Ventas de PC-----------
type Nombre = String
type Precio = Double


vendedores = ["Martin", "Diego", "Claudio", "Jose"]


nombreComponente (n,_) = n
vendedor (_,vendedor,_) = vendedor
fecha (fecha,_,_) = fecha
listaComponentes (_,_,listaComponentes) = listaComponentes

precios::[(String,Double)]
precios = [("Monitor GPRS 3000", 200), ("Motherboard ASUS 1500", 120), ("Monitor ASC 543", 250), ("Motherboard ASUS 1200", 100), ("Motherboard Pindorcho", 30)]

find criterio = head . filter criterio

ventas = [((1,2,2006), "Martin", ["Monitor GPRS 3000", "Motherboard ASUS 1500"]),((1,2,2006), "Diego", ["Monitor ASC 543", "Motherboard Pindorcho"]),((10,2,2006), "Martin", ["Monitor ASC 543", "Motherboard ASUS 1200"]),((12,2,2006), "Diego", ["Monitor GPRS 3000", "Motherboard ASUS 1200"]),((4,3,2006), "Diego", ["Monitor GPRS 3000", "Motherboard ASUS 1500"])]


---1.1
precioMaquina::Foldable t => t String -> Double
precioMaquina listaComponentes = foldl (\acum t -> acum + (encontrarPrecio t) ) 0 listaComponentes

encontrarPrecio componente = snd (find ((== componente).nombreComponente) precios)

---1.2

cantVentasComponente nombre =  ((length).filter (==nombre)) ventasLista

ventasLista = concatMap (listaComponentes) ventas

--concatMap (listaComponentes) ventas
--ventasLista = [ listaComponentes venta |venta<-ventas]

--1.3

dia (dia,_,_) = dia
mes (_,mes,_) = mes
anio (_,_,anio) = anio

vendedorDelMes (m,nio) =  (maximoSegun (montoVentas)) (listaVentasMesAnio (m,nio))

listaVentasMesAnio (m,nio) = filter (\x -> (filFecha x (m,nio) )) ventas

filFecha venta (m,nio) = (((==m).mes).fecha) venta && (((==nio).anio).fecha) venta


maximoSegun funcionMaxima (x:xs) 
  | aplicar funcionMaxima (x:xs) == funcionMaxima x = x
  | otherwise = maximoSegun funcionMaxima xs

aplicar funcionMaxima [x] = funcionMaxima x
aplicar funcionMaxima (x:xs) = max (aplicar funcionMaxima [x]) (aplicar funcionMaxima xs)

montoVentas venta = precioMaquina (listaComponentes venta) 

--1.4
-- ***********Primer Orden*********** --

ventasCriterio criterio = (foldl (\acum t -> acum + (((precioMaquina).listaComponentes) t) ) 0) (filter criterio ventas)

-- criterioFecha::criterio
esDeLaFecha fecha (fechaVenta,_,_) = fecha == fechaVenta

--1.4.1

ventasMes (m,anio) = (((==m).mes).fecha) 

--1.4.2

ventasVendedor ven = ((==ven).vendedor)

--1.4.3 

huboVentas (m,anio) = ((not).(null).(filter (ventasMes (m,anio)))) ventas