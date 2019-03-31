-- Autor: Aleksandra Maciąga 236369

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random; use Ada.Numerics;
with Ada.Containers.Vectors; use Ada.Containers;
with dif;
procedure Main is
-- struktura przechowywujaca zadania
type My_Task is record
    FirstArg : Integer;
    SecondArg : Integer;
    Op : String(1..1);
end record;
-- wątek prezesa
task Ceo;
--  watek przchowywujący liste zadań
task TaskMag is
    entry Insert(A_Task : in My_Task);
    entry Remove(A_Task : out My_Task);
    entry State;
end TaskMag;
--  wątek pracownika
task type Worker is
    entry Start (N : Integer);
end Worker;
-- watek przchowywujący magazyn z rozwiązaniami
task ResultWarehouse is
    entry Insert(A_Result : in Integer);
    entry Remove(A_Result : out Integer);
    entry State;
end ResultWarehouse;
--  wątek klienta
task Client;
--  wątek to obsługi typu "spokojnego"
task Prompt is
    entry Start;
    entry Done;
end Prompt;

task body Ceo is
    type My_Arr is array (0..2) of String(1..1);
    -- tablica z mozliwymi operatorami
    Ops : My_Arr := ("+","-","*");
    subtype Value_Range is Integer range 0 .. 2;
    package V is new Discrete_Random (Value_Range);
    -- generator liczb losowych z zakresu 0 .. 2
    GV : V.Generator;
    subtype Value_Range2 is Integer range 0 .. 10;
    package V2 is new Discrete_Random (Value_Range2);
    -- generator liczb losowych z zakresu 0 .. 10
    GV2 : V2.Generator;
    r : Integer;
    t : My_Task;
begin
    loop
        V.Reset (GV);
        V2.Reset(GV2);
        r := V.Random(GV);
        -- stworzenie nowego zadania
        t := (V2.Random(GV2),V2.Random(GV2),Ops(r));
        -- dodanie stworzonego zadania do listy zadań
        TaskMag.Insert(t);
        if dif.LoudMode then
             Put_Line("Prezes dodal zadanie: [" & Integer'Image(t.FirstArg) & "," & Integer'Image(t.SecondArg) & "," & t.Op & "]");
        end if;
        -- uspienie prezesa
        delay(dif.CeoSleepTime);
    end loop;
end Ceo;

task body TaskMag is
    package Vector_Pkg is new Vectors(Natural,My_Task);
    --  wektor przechowujacy liste zadan
    TaskList : Vector_Pkg.Vector;
begin
    loop
        select
        -- obsługa wstawienia zadania do listy zadan
            when Integer(TaskList.Length) < dif.TaskListCapacity =>
                accept Insert(A_Task : in My_Task) do
                    TaskList.Append(A_Task);
                end Insert;
                
        or
        -- obsługa pobrania zadania z listy zadan
            when Integer(TaskList.Length) > 0 =>
                accept Remove (A_Task : out My_Task) do
                    A_Task := TaskList.First_Element;
                end Remove;
                TaskList.Delete_First;
        or 
        -- wypisanie zawartości listy zadań
            accept State do
                Put_Line("Lista zadan: ");
                for I in 0..(Integer(TaskList.Length)-1) loop
                    Put("[");
                    Put(Integer'Image(TaskList.Element(I).FirstArg));
                    Put(",");
                    Put(Integer'Image(TaskList.Element(I).SecondArg));
                    Put(",");
                    Put(TaskList.Element(I).Op);
                    Put("]");
                end loop;
                Put_Line("");
            end State;
        end select;
    end loop;
end TaskMag;

task body Worker is
    t : My_Task;
    r : Integer;
    id: Integer;
begin
    accept Start (N : Integer) do
         id := N;
    end Start;
    loop
        r := 0;
        --  pobranie zadania z listy zadan
        TaskMag.Remove(t);
        if dif.LoudMode then
            Put_Line("Pracownik nr" & Integer'Image(id) &" rozwiazuje zadanie: [" & Integer'Image(t.FirstArg) & "," & Integer'Image(t.SecondArg) & "," & t.Op & "]");
        end if;
        -- rozwiazanie zadania
        if t.Op = "+" then
            r := t.FirstArg + t.SecondArg;
        elsif t.Op = "-" then
            r := t.FirstArg - t.SecondArg;
        else
            r := t.FirstArg * t.SecondArg;
        end if;
        --  wstawienie rozwiazania do magazynu
        ResultWarehouse.Insert(r);
        if dif.LoudMode then
            Put_Line("Pracownik nr" & Integer'Image(id) &" otrzymal wynik:" & Integer'Image(r) );
        end if;
        --  uspienie pracownika
        delay(dif.WorkerSleepTime);
    end loop;
end Worker;

task body ResultWarehouse is
    package Vector_Pkg is new Vectors(Natural,Integer);
    --  wektor przechowujacy magazyn rozwiązań
    Warehouse : Vector_Pkg.Vector;
begin
    loop
        select
        -- obsługa dodania rozwiązania do magazynu
            when Integer(Warehouse.Length) < dif.WarehouseCapacity =>
                accept Insert(A_Result : in Integer) do
                    Warehouse.Append(A_Result);
                end Insert;
        or
        -- obsługa pobrania rozwiązania z magazynu
            when Integer(Warehouse.Length) > 0 =>
                accept Remove (A_Result : out Integer) do
                    A_Result := Warehouse.First_Element;
                end Remove;
                Warehouse.Delete_First;
        or 
        -- wypisanie zawartości magazynu
            accept State do
                Put_Line("Magazyn");
                for I in 0..(Integer(Warehouse.Length)-1) loop
                    Put("[");
                    Put(Integer'Image(Warehouse.Element(I)));
                    Put("]");
                end loop;
                Put_Line("");
            end State;
        end select;
    end loop;
end ResultWarehouse;

task body Client is
    r : Integer;
begin
    loop
    -- pobranie rozwiazania z magazynu
        ResultWarehouse.Remove(r);
        if dif.LoudMode then
            Put_Line("Klient kupil rozwiazanie:" & Integer'Image(r) );
        end if;
        delay(dif.ClientSleepTime);
    end loop;
end Client;

task body Prompt is
    S: String(1 .. 1);
    Last: Integer;
begin
    accept Start  do
        null;
    end Start;
    loop
    -- wyswietlenie menu dla trybu cichego
        Put_Line("Menu");
        Put_Line("1. Wyswietl stan magazynu");
        Put_Line("2. Wyswietl liste zadan do wykonania");
        Put("Wybierz nr czynnosci: ");
        Get_Line(S, Last);
        -- wykonanie czynnosci wybranej przez uzytkownika
        if S = "1" then
            ResultWarehouse.State;
            Get_Line(S, Last);
        elsif S = "2" then
            TaskMag.State;
            Get_Line(S, Last);
        else
            Put_Line("Podano niepoprawny nr czynnosci");
        end if;
    end loop;
end Prompt;
-- tablica pracownikow
Workers : array (1..dif.NumberOfWorkers) of Worker;

begin
    -- rozpoczecie pracy pracownikow
    for I in Workers'Range loop
        Workers(I).Start(I);
    end loop;
    --  uruchominie menu dla trybu "spokojnego"
    if not dif.LoudMode then
        Prompt.Start;
    end if;
end Main;