-- Autor: ALeksandra Maciąga 236369

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random; use Ada.Numerics;
with Ada.Containers.Vectors; use Ada.Containers;
with dif;
procedure Main is

type My_Task is record
    FirstArg : Integer;
    SecondArg : Integer;
    Op : String(1..1);
end record;

task Ceo;

task TaskMag is
    entry Insert(A_Task : in My_Task);
    entry Remove(A_Task : out My_Task);
    entry State;
end TaskMag;

task type Worker is
    entry Start (N : Integer);
end Worker;

task ResultWarehouse is
    entry Insert(A_Result : in Integer);
    entry Remove(A_Result : out Integer);
    entry State;
end ResultWarehouse;

task Client;

task Prompt is
    entry Start;
    entry Done;
end Prompt;

task body Ceo is
    type My_Arr is array (0..2) of String(1..1);
    Ops : My_Arr := ("+","-","*");
    subtype Value_Range is Integer range 0 .. 2;
    package V is new Discrete_Random (Value_Range);
    GV : V.Generator;
    subtype Value_Range2 is Integer range 0 .. 10;
    package V2 is new Discrete_Random (Value_Range2);
    GV2 : V2.Generator;
    r : Integer;
    t : My_Task;
begin
    loop
        V.Reset (GV);
        V2.Reset(GV2);
        r := V.Random(GV);
        t := (V2.Random(GV2),V2.Random(GV2),Ops(r));
        TaskMag.Insert(t);
        if dif.LoudMode then
             Put_Line("Prezes dodal zadanie: [" & Integer'Image(t.FirstArg) & "," & Integer'Image(t.SecondArg) & "," & t.Op & "]");
        end if;
        delay(dif.CeoSleepTime);
    end loop;
end Ceo;

task body TaskMag is
    package Vector_Pkg is new Vectors(Natural,My_Task);
    TaskList : Vector_Pkg.Vector;
begin
    loop
        select
            when Integer(TaskList.Length) < dif.TaskListCapacity =>
                accept Insert(A_Task : in My_Task) do
                    TaskList.Append(A_Task);
                end Insert;
                
        or
            when Integer(TaskList.Length) > 0 =>
                accept Remove (A_Task : out My_Task) do
                    A_Task := TaskList.First_Element;
                end Remove;
                TaskList.Delete_First;
        or 
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
        TaskMag.Remove(t);
        if dif.LoudMode then
            Put_Line("Pracownik nr" & Integer'Image(id) &" rozwiazuje zadanie: [" & Integer'Image(t.FirstArg) & "," & Integer'Image(t.SecondArg) & "," & t.Op & "]");
        end if;
        if t.Op = "+" then
            r := t.FirstArg + t.SecondArg;
        elsif t.Op = "-" then
            r := t.FirstArg - t.SecondArg;
        else
            r := t.FirstArg * t.SecondArg;
        end if;
        ResultWarehouse.Insert(r);
        if dif.LoudMode then
            Put_Line("Pracownik nr" & Integer'Image(id) &" otrzymal wynik:" & Integer'Image(r) );
        end if;
        delay(dif.WorkerSleepTime);
    end loop;
end Worker;

task body ResultWarehouse is
    package Vector_Pkg is new Vectors(Natural,Integer);
    Warehouse : Vector_Pkg.Vector;
begin
    loop
        select
            when Integer(Warehouse.Length) < dif.WarehouseCapacity =>
                accept Insert(A_Result : in Integer) do
                    Warehouse.Append(A_Result);
                end Insert;
        or
            when Integer(Warehouse.Length) > 0 =>
                accept Remove (A_Result : out Integer) do
                    A_Result := Warehouse.First_Element;
                end Remove;
                Warehouse.Delete_First;
        or 
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
        Put_Line("Menu");
        Put_Line("1. Wyswietl stan magazynu");
        Put_Line("2. Wyswietl liste zadan do wykonania");
        Put("Wybierz nr czynnosci: ");
        Get_Line(S, Last);
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

Workers : array (1..dif.NumberOfWorkers) of Worker;

begin
    for I in Workers'Range loop
        Workers(I).Start(I);
    end loop;
    if not dif.LoudMode then
        Prompt.Start;
    end if;
end Main;