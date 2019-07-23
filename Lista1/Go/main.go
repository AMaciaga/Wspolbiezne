// Autor: Aleksandra Maciąga 
package main

import (
	dif "./dif"
	"bufio"
	"fmt"
	"math/rand"
	"os"
	"time"
)

type task struct {
	firstArg  int
	secondArg int
	op        string
}

// zabezpiecznie przed odwolaniem sie  do pustego miejsca w pamieci
func safeGetTask(b bool, t []task) *task {
	if !b {
		return nil
	}
	return &t[0]
}

// zabezpieczenie przed przepelnieniem listy zadan
func taskWriteGuardian(b bool, c <-chan *task) <-chan *task {
	if !b {
		return nil
	}
	return c
}

// zabezpiecznie przed odwolaniem sie  do pustego miejsca w pamieci
func taskGetGuardian(b bool, c chan<- *task) chan<- *task {
	if !b {
		return nil
	}
	return c
}

// zabezpiecznie przed odwolaniem sie  do pustego miejsca w pamieci
func safeGetResult(b bool, t []int) int {
	if !b {
		return 0
	}
	return t[0]
}

// zabezpieczenie przed przepelnieniem magazynu
func resultWriteGuardian(b bool, c <-chan int) <-chan int {
	if !b {
		return nil
	}
	return c
}

// zabezpiecznie przed odwolaniem sie  do pustego miejsca w pamieci
func resultGetGuardian(b bool, c chan<- int) chan<- int {
	if !b {
		return nil
	}
	return c
}

// watek prezesa
func ceo(taskWriteChan chan<- *task) {
	ops := [3]string{"+", "-", "*"}
	for {
		i := rand.Intn(3)
		o := ops[i]
		//  stworzenie nowego zadania
		t := &task{
			firstArg:  rand.Intn(10),
			secondArg: rand.Intn(10),
			op:        o}
		// dodanie stworzonego zadania do listy zadań
		taskWriteChan <- t
		if dif.LoudMode {
			fmt.Println("Prezes dodal zadanie:", *t)
		}
		// uspienie prezesa
		time.Sleep(time.Duration(dif.CeoSleepTime) * time.Millisecond)
	}
}

// watek przchowywujący liste zadań
func taskMag(taskWriteChan <-chan *task, taskGetChan chan<- *task, getTaskListState <-chan chan bool) {
	// slice przechowujacy liste zadan
	taskList := make([]task, 0)
	for {
		select {
		// obsługa wstawienia zadania do listy zadan
		case msg := <-taskWriteGuardian(len(taskList) < dif.TaskListCapacity, taskWriteChan):
			{
				taskList = append(taskList, *msg)
			}
		// obsługa pobrania zadania z listy zadan
		case taskGetGuardian(len(taskList) > 0, taskGetChan) <- safeGetTask(len(taskList) > 0, taskList):
			{
				taskList = taskList[1:]
			}
		// wypisanie zawartości listy zadań
		case msg := <-getTaskListState:
			{
				fmt.Println("Lista zadań:", taskList)
				msg <- true
			}
		}

	}

}

// wątek pracownika
func worker(id int, taskGetChan <-chan *task, resultWriteChan chan<- int) {

	for {
		select {
		// pobranie zadania z listy zadan
		case msg := <-taskGetChan:
			{
				if dif.LoudMode {
					fmt.Println("Pracownik nr", id, "rozwiazuje zadanie:", *msg)
				}
				result := 0
				// rozwiazanie zadania
				switch msg.op {
				case "+":
					{
						result = msg.firstArg + msg.secondArg
					}
				case "-":
					{
						result = msg.firstArg - msg.secondArg
					}
				case "*":
					{
						result = msg.firstArg * msg.secondArg
					}
				}
				// wstawienie rozwiazania do magazynu
				resultWriteChan <- result
				if dif.LoudMode {
					fmt.Println("Pracownik nr", id, "otrzymał wynik:", result)
				}
			}
		}
		// uspienie pracownika
		time.Sleep(time.Duration(dif.WorkerSleepTime) * time.Millisecond)
	}

}

// watek przchowywujący magazyn z rozwiązaniami
func resultWarehouse(resultWriteChan <-chan int, resultGetChan chan<- int, getWarehouseState <-chan chan bool) {
	// slice przechowujacy magazyn rozwiązań
	warehouse := make([]int, 0)
	for {
		select {
		// obsługa dodania rozwiązania do magazynu
		case msg := <-resultWriteGuardian(len(warehouse) < dif.WarehouseCapacity, resultWriteChan):
			{
				warehouse = append(warehouse, msg)
			}
		// obsługa pobrania rozwiązania z magazynu
		case resultGetGuardian(len(warehouse) > 0, resultGetChan) <- safeGetResult(len(warehouse) > 0, warehouse):
			{
				warehouse = warehouse[1:]
			}
		// wypisanie zawartości magazynu
		case msg := <-getWarehouseState:
			{
				fmt.Println("Zawartość magazynu:", warehouse)
				msg <- true
			}
		}

	}
}

// wątek klienta
func client(resultGetChan <-chan int) {

	for {
		select {
		// pobranie rozwiazania z magazynu
		case msg := <-resultGetChan:
			{
				if dif.LoudMode {
					fmt.Println("Klient kupił rozwiązanie:", msg)
				}
			}
		}
		time.Sleep(time.Duration(dif.ClientSleepTime) * time.Millisecond)
	}

}

// wątek to obsługi typu "spokojnego"
func prompt(getTaskListState chan<- chan bool, getWarehouseState chan<- chan bool) {
	getTaskListReturn := make(chan bool)
	getWarehouseReturn := make(chan bool)
	scanner := bufio.NewScanner(os.Stdin)
	for {
		// wyswietlenie menu dla trybu cichego
		fmt.Println("Menu")
		fmt.Println("1. Wyswietl stan magazynu")
		fmt.Println("2. Wyswietl liste zadań do wykonania")
		fmt.Print("Wybierz nr czynnosci: ")
		scanner.Scan()
		text := scanner.Text()
		// wykonanie czynnosci wybranej przez uzytkownika
		switch text {
		case "1":
			{
				getWarehouseState <- getWarehouseReturn
				<-getWarehouseReturn
			}
		case "2":
			{
				getTaskListState <- getTaskListReturn
				<-getTaskListReturn
			}
		default:
			{
				fmt.Println("Podano niepoprawny nr czynnosci")
			}
		}

	}
}

func main() {
	// channel do przeslania zadania od prezesa do listy zadan
	taskWriteChan := make(chan *task)
	// channel do przeslania zadania z listy zadan do pracownika
	taskGetChan := make(chan *task)
	// channel do przeslania zadania od pracownika do magazynu
	resultWriteChan := make(chan int)
	// channel do przeslania zadania z magazynu do klienta
	resultGetChan := make(chan int)
	// channel do wywolania wypisania zawartości listy zadań
	getTaskListState := make(chan chan bool)
	// channel do wywolania wypisania zawartości magazynu
	getWarehouseState := make(chan chan bool)

	// wywołanie wątku prezesa
	go ceo(taskWriteChan)
	// wywołanie wątku listy zadań
	go taskMag(taskWriteChan, taskGetChan, getTaskListState)
	// wywołanie wątków pracowników
	for w := 1; w <= dif.NumberOfWorkers; w++ {
		go worker(w, taskGetChan, resultWriteChan)
	}
	// wywołanie wątku magazynu
	go resultWarehouse(resultWriteChan, resultGetChan, getWarehouseState)
	// wywołanie wątku klienta
	go client(resultGetChan)
	// wywołanie wątku dla trybu "spokojnego"
	if !dif.LoudMode {
		go prompt(getTaskListState, getWarehouseState)
	}
	for {
	}

}
