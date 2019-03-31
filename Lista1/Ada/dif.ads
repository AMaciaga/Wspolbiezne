-- Autor: Aleksandra Maciąga 236369
package dif is
    -- zmienna odpowiedzialna za opoznienie prezesa
    CeoSleepTime : Duration := 0.5;
    -- zmienna odpowiedzialna za opoznienie pracownika
    WorkerSleepTime : Duration := 1.0;
    -- zmienna odpowiedzialna za opoznienie klienta
    ClientSleepTime : Duration := 0.75;

    -- zmienna odpowiedzialna za pojemnośc listy zadań
    TaskListCapacity : Integer := 20;
    -- zmienna odpowiedzialna za pojemnośc magazynu
    WarehouseCapacity : Integer := 20;
    -- zmienna odpowiedzialna za ilość pracowników
    NumberOfWorkers : Integer := 5;

    -- zmienna odpowiedzialna za wybór trybu "gadatliwego" lub "spokojnego"
    LoudMode : Boolean := False;

end dif;