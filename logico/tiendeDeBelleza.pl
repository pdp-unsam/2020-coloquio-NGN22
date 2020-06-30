% Tenemos una tienda de belleza, con clientes asiduos, 
% la lista de tratamientos y el costo, y los cliente que tomaron ese tratamiento.
% queremos averiguar:  el maximo tratamiento, cliente que mas veces fue a un tratamiento.
%cliente que fue a un tratamiento pero no otro.
%inversibilidad

cliente(tobias,23).
cliente(esperanza,25).
cliente(lorena,34).
cliente(guadalupe,34).
cliente(cristina,34).


tratamiento(unas,350).
tratamiento(permanente,560).
tratamiento(maquillaje,600).
tratamiento(peluqueria,900).
tratamiento(corporal,800).

descuento(unas).


tomoTratamiento(cristina,corporal,mayo).
tomoTratamiento(cristina,unas,mayo).
tomoTratamiento(guadalupe,peluqueria,abril).
tomoTratamiento(guadalupe,peluqueria,mayo).
tomoTratamiento(guadalupe,permanente,febrero).
tomoTratamiento(lorena,maquillaje,junio).
tomoTratamiento(esperanza,permanente,julio).
tomoTratamiento(tobias,unas,mayo).

%todos los clientes de mayo
%Solo esperanza no fue en junio
clientesMayo(Cliente):-
    tomoTratamiento(Cliente,_,mayo).


%tratamiento mas caro de la tienda
tratamientoMasCaro(Tratamiento):-
    tratamiento(Tratamiento,Precio),
    forall( tratamiento(_,Precio2),Precio >= Precio2).


%Clientes que tomaron tratamientos con descuento
conDescuento(Cliente):-
    tomoTratamiento(Cliente,Tratamiento,_),
    forall( tomoTratamiento(Cliente,Tratamiento,_), descuento(Tratamiento)).


%Clientes que solo tomaron tratamiento con Descuento
soloTratamientosConDescuento(Cliente):-
    tomoTratamiento(Cliente,_,_),
    forall( tomoTratamiento(Cliente,Tratamiento,_), descuento(Tratamiento)).

%quiero saber la cantidad e veces que cada cliente tomo un tratamiento de peluqueria
cantidadVecesQueTomaronPeLuqueria(Cliente,Cantidad):-
    cliente(Cliente,_),
    findall(peluqueria,tomoTratamiento(Cliente,peluqueria,_), Numero ),
    length(Numero, Cantidad).


%clietes que no recibieron tratamiento 
clientesSinTratamientoConDescuento(Cliente):-
    tomoTratamiento(Cliente,_,_),
    not(conDescuento(Cliente)).


% clientesSinTratamientoConDescuento(Cliente).
    % Cliente = guadalupe ;
    % Cliente = guadalupe ;
    % Cliente = lorena ;
    % Cliente = esperanza ;


% soloTratamientosConDescuento(Cliente).
    %Cliente = tobias.

% conDescuento(Cliente).
    % Cliente = cristina ;
    % Cliente = tobias.

% tratamientoMasCaro(Tratamiento).
%   Tratamiento = peluqueria ;    
