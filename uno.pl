% inversibilidad conceptos mas interesantes del paradigma
% verificar - E o existencia Con una variable

%De cada ciudad se conoce su nombre, los productos que produce y los productos que requiere.

%armando la base de conocimiento

nombreDeCiudad(jujuy).
nombreDeCiudad(neuquen).
nombreDeCiudad(mendoza).
nombreDeCiudad(cordoba).
nombreDeCiudad(corrientes).

produce(cobre,jujuy).
produce(hierro,jujuy).
produce(energia,neuquen).
produce(vino,mendoza).
produce(soja,cordoba).
produce(ganado,cordoba).
produce(arroz,corrientes).

requiere(agua,jujuy).
requiere(petroleo,neuquen).
requiere(uvas,mendoza).
requiere(semillas,cordoba).
requiere(semillas,corrientes).
requiere(petroleo,santafe).

%De los camiones conocemos su patente y otros datos más, según que tipo de camión sea
%Comun, se sabe el volumen de carga que puede llevar.
%Semi remolque, se conoce el largo, el ancho y la altura del remolque.
%Con acoplado, se registra el volumen de la caja del camion y el del acoplado.

%o se puede hacer camion(TIPO,patente).
patenteDecamion(ab123cd,comun).
patenteDecamion(aa234bb,comun).
patenteDecamion(cd124ed,semiremolque).
patenteDecamion(ch421de,semiremolque).
patenteDecamion(ed125fg,acoplado).
patenteDecamion(de521gf,acoplado).

 %volumen
comun(ab123cd,200).
comun(aa234bb,400).
semiremolque(cd124ed,12,1,3).
semiremolque(ch421de,15,2,8).
acoplado(ed125fg,100,50).
acoplado(de521gf,100,200).

%Se tiene registrado cada uno de los viajes que se realizaron, indicando la ciudad de origen, 
%la ciudad de destino, la mercancía transportada, como así también la patente del camión que la transporta.

viajes(jujuy,mendoza,uvas,ab123cd).
viajes(corrientes,mendoza,uvas,ed125fg).
viajes(neuquen,mendoza,agua,cd124ed).
viajes(neuquen,cordoba,petroleo,ab123cd).
viajes(neuquen,corrientes,petroleo,cd124ed).
viajes(neuquen,mendoza,petroleo,ab123cd).
viajes(neuquen,jujuy,petroleo,cd124ed).
viajes(neuquen,santafe,petroleo,ed125fg).


 %el volumen transportado
volumenTransportado(Patente,Volumen):-
    comun(Patente,Vol),
    Volumen is Vol.

volumenTransportado(Patente,Volumen):-
    acoplado(Patente,Caja,Acoplado), 
    Volumen is Caja + Acoplado.

volumenTransportado(Patente,Volumen):-
    semiremolque(Patente,Largo,Ancho,Altura),
    Volumen is Largo*Ancho*Altura.

 %El mayor viaje que llegó a una ciudad de cada producto que la ciudad requiere o produce.
 % Inversible

viajeConMayorVolumen(Ciudad, Volumen, Patente, Producto):-  
    hayUnViajeConProducto(Ciudad, Volumen, Patente, Producto),
    requiere(Producto,Ciudad),
    forall((hayUnViaje(Ciudad, Volumen2,_,_),requiere(Producto,Ciudad)), Volumen>=Volumen2).

viajeConMayorVolumen(Ciudad, Volumen, Patente, Producto):-  
    hayUnViajeConProducto(Ciudad, Volumen, Patente, Producto),
    requiere(Producto,Ciudad),
    forall((hayUnViaje(Ciudad, Volumen2,_,_),produce(Producto,Ciudad)), Volumen>=Volumen2).

hayUnViajeConProducto(Ciudad, Volumen, Patente, Producto):-
    requiere(Producto,Ciudad),
    viajes(_,Ciudad,Producto,Patente),
    volumenTransportado(Patente,Volumen).

hayUnViajeConProducto(Ciudad, Volumen, Patente, Producto):-
    produce(Producto,Ciudad),
    viajes(_,Ciudad,Producto,Patente),
    volumenTransportado(Patente,Volumen).

%El menor viaje que alguna vez llegó o salió de la ciudad.

viajeConMenorVolumen(Ciudad, Volumen, Patente, Producto):-    
    hayUnViaje(Ciudad, Volumen, Patente, Producto),
    forall(hayUnViaje(Ciudad, Volumen2,_,_), Volumen=<Volumen2).

hayUnViaje(Ciudad, Volumen, Patente, Producto):-
    viajes(_,Ciudad,Producto,Patente),
    volumenTransportado(Patente,Volumen).

hayUnViaje(Ciudad, Volumen, Patente, Producto):-
    viajes(Ciudad,_,Producto,Patente),
    volumenTransportado(Patente,Volumen).

 % Se necesita encontrar las ciudades que: Nunca les llegó un camión con algún producto que requieran.

ciudadDebastecida(Ciudad):-
    viajes(_,Ciudad,_,_),
    forall(requiere(Producto,Ciudad), not(viajes(_,Ciudad,Producto,_))).

ciudadDebastecidaNoInV(Ciudad):-
    forall(requiere(Producto,Ciudad), not(viajes(_,Ciudad,Producto,_))).


 % Se necesita encontrar las ciudades que:Recibieron viajes de todos los productos que requieren

ciudadAbastecida(Ciudad):-
    viajes(_,Ciudad,_,_),
    forall(requiere(Producto,Ciudad), viajes(_,Ciudad,Producto,_)).


ciudadAbastecidaNoInv(Ciudad):-
    forall(requiere(Producto,Ciudad), viajes(_,Ciudad,Producto,_)).

 % Se necesita encontrar las ciudades que:salen viajes de un único producto 

ciudadMateriaPrima(Ciudad):-  
    viajes(Ciudad,_,Producto,_),
    forall( viajes(Ciudad,_,Producto,_),unicoProducto(Ciudad,Producto)).

ciudadMateriaPrimaNoInv(Ciudad):-  
    forall( viajes(Ciudad,_,Producto,_),unicoProducto(Ciudad,Producto)).

unicoProducto(Ciudad,Producto):-
    viajes(Ciudad,_,Producto,_),
    not(esOtroProducto(Ciudad,Producto)).

unicoProductoNoInv(Ciudad,Producto):-
    not(esOtroProducto(Ciudad,Producto)).

esOtroProducto(Ciudad,Producto):-
    viajes(Ciudad,_,Producto2,_),
    Producto \= Producto2.
 %Se necesita encontrar las ciudades que:Alguna vez pasó por ella un camión con acoplado donde la caja tiene más capacidad que el acoplado.
camionConCajaGrande(Patente):-
    acoplado(Patente, Caja, Acoplado),
    Caja>Acoplado.

ciudadesConVisitaDeCamion(Ciudad):-
    viajes(_,Ciudad,_,Patente),
    forall( viajes(_,Ciudad,_,Patente),camionConCajaGrande(Patente)).

ciudadesConVisitaDeCamionNoInv(Ciudad,Patente):-
    forall( viajes(_,Ciudad,_,Patente),camionConCajaGrande(Patente)).


sinVisitasPatente(Patente):-
    not(comun(Patente,_)),
    not(acoplado(Patente,_,_) ).
    


soloRecibeSemiremolques(Ciudad):-
    viajes(_,Ciudad,_,_),   
forall( viajes(_,Ciudad,_,Patente), semiremolque(Patente,_,_,_) ).


%forAll 


% costo(gaseosa,50).
% costo(soda,100). 
% costo(galletita,200).
% costo(alfajor,200).


% recargo(Cliente,Producto,Recargo):-
%     es

% calcularPrecio(Cliente,Producto,PrecioVenta):- 
%     costo(Producto,Costo), 
%     recargo(Cliente,Producto,Recargo),
%      PrecioVenta is (Costo*Recargo)/100. 



