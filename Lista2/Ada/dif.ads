-- Autor: Aleksandra Maciąga 
package dif is
    -- zmienna odpowiedzialna za opoznienie prezesa
    CeoSleepTime : Duration := 0.5;
    -- zmienna odpowiedzialna za opoznienie pracownika
    WorkerSleepTime : Duration := 1.0;
    -- zmienna odpowiedzialna za opoznienie klienta
    ClientSleepTime : Duration := 0.75;

    --zmienna odpowiedzialna za czas oczekiwania niecierpliwego pracownika
    ImpatientWorkerWaitTime : Duration := 0.25;

    -- zmienna odpowiedzialna za pojemnośc listy zadań
    TaskListCapacity : Integer := 20;
    -- zmienna odpowiedzialna za pojemnośc magazynu
    WarehouseCapacity : Integer := 20;
    -- zmienna odpowiedzialna za ilość pracowników
    NumberOfWorkers : Integer := 5;

    -- zmienna odpowiedzialna za ilosc maszyn dodajacych
    NumberOfAddMachines : Integer := 3;
    -- zmienna odpowiedziala za czas pracy maszyn dodajacych
    AddMachineWorkTime : Duration := 2.0;


    -- zmienna odpowiedzialna za ilosc maszyn mnożących
    NumberOfMultMachines : Integer := 3;
    -- zmienna odpowiedziala za czas pracy maszyn mnożących
    MultMachineWorkTime : Duration := 2.0;

    -- zmienna odpowiedzialna za wybór trybu "gadatliwego" lub "spokojnego"
    LoudMode : Boolean := False;

end dif;