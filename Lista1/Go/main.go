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

func safeGetTask(b bool, t []task) *task {
	if !b {
		return nil
	}
	return &t[0]
}
func taskWriteGuardian(b bool, c <-chan *task) <-chan *task {
	if !b {
		return nil
	}
	return c
}
func taskGetGuardian(b bool, c chan<- *task) chan<- *task {
	if !b {
		return nil
	}
	return c
}
func safeGetResult(b bool, t []int) int {
	if !b {
		return 0
	}
	return t[0]
}
func resultWriteGuardian(b bool, c <-chan int) <-chan int {
	if !b {
		return nil
	}
	return c
}
func resultGetGuardian(b bool, c chan<- int) chan<- int {
	if !b {
		return nil
	}
	return c
}
func ceo(taskWriteChan chan<- *task) {
	for {
		ops := [3]string{"+", "-", "*"}
		i := rand.Intn(3)
		o := ops[i]
		t := &task{
			firstArg:  rand.Intn(10),
			secondArg: rand.Intn(10),
			op:        o}
		taskWriteChan <- t
		if dif.LoudMode {
			fmt.Println("Prezes dodal zadanie:", *t)
		}
		time.Sleep(time.Duration(dif.CeoSleepTime) * time.Millisecond)
	}
}
func taskMag(taskWriteChan <-chan *task, taskGetChan chan<- *task, getTaskListState <-chan chan bool) {

	taskList := make([]task, 0)
	for {
		select {
		case msg := <-taskWriteGuardian(len(taskList) < dif.TaskListCapacity, taskWriteChan):
			{
				taskList = append(taskList, *msg)
			}

		case taskGetGuardian(len(taskList) > 0, taskGetChan) <- safeGetTask(len(taskList) > 0, taskList):
			{
				taskList = taskList[1:]
			}
		case msg := <-getTaskListState:
			{
				fmt.Println("Lista zadań:", taskList)
				msg <- true
			}
		}

	}

}
func worker(id int, taskGetChan <-chan *task, resultWriteChan chan<- int) {

	for {
		select {
		case msg := <-taskGetChan:
			{
				if dif.LoudMode {
					fmt.Println("Pracownik nr", id, "rozwiazuje zadanie:", *msg)
				}
				result := 0
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
				resultWriteChan <- result
				if dif.LoudMode {
					fmt.Println("Pracownik nr", id, "otrzymał wynik:", result)
				}
			}
		}
		time.Sleep(time.Duration(dif.WorkerSleepTime) * time.Millisecond)
	}

}
func resultWarehouse(resultWriteChan <-chan int, resultGetChan chan<- int, getWarehouseState <-chan chan bool) {
	warehouse := make([]int, 0)
	for {
		select {
		case msg := <-resultWriteGuardian(len(warehouse) < dif.WarehouseCapacity, resultWriteChan):
			{
				warehouse = append(warehouse, msg)
			}

		case resultGetGuardian(len(warehouse) > 0, resultGetChan) <- safeGetResult(len(warehouse) > 0, warehouse):
			{
				warehouse = warehouse[1:]
			}
		case msg := <-getWarehouseState:
			{
				fmt.Println("Zawartość magazynu:", warehouse)
				msg <- true
			}
		}

	}
}
func client(resultGetChan <-chan int) {

	for {
		select {
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
func prompt(getTaskListState chan<- chan bool, getWarehouseState chan<- chan bool) {
	getTaskListReturn := make(chan bool)
	getWarehouseReturn := make(chan bool)
	scanner := bufio.NewScanner(os.Stdin)
	for {
		fmt.Println("Menu")
		fmt.Println("1. Wyswietl stan magazynu")
		fmt.Println("2. Wyswietl liste zadań do wykonania")
		fmt.Println("Wybierz nr czynnosci:")
		scanner.Scan()
		text := scanner.Text()
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
	taskWriteChan := make(chan *task)
	taskGetChan := make(chan *task)
	resultWriteChan := make(chan int)
	resultGetChan := make(chan int)
	getTaskListState := make(chan chan bool)
	getWarehouseState := make(chan chan bool)

	go ceo(taskWriteChan)
	go taskMag(taskWriteChan, taskGetChan, getTaskListState)
	for w := 1; w <= dif.NumberOfWorkers; w++ {
		go worker(w, taskGetChan, resultWriteChan)
	}
	go resultWarehouse(resultWriteChan, resultGetChan, getWarehouseState)
	go client(resultGetChan)
	if !dif.LoudMode {
		go prompt(getTaskListState, getWarehouseState)
	}
	for {
	}

}
