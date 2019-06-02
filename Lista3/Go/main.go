// Autor: Aleksandra Maciąga 236369
package main

import (
	"bufio"
	"fmt"
	"math/rand"
	"os"
	"time"

	dif "./dif"
)

type task struct {
	firstArg  int
	secondArg int
	op        string
	result    int
	notSolved bool
}
type info struct {
	aTask    *task
	userChan chan *task
}
type workerInfo struct {
	id        int
	impatient string
	solved    int
}
type machine struct {
	id    int
	mType int
}

// zabezpiecznie przed odwolaniem sie  do pustego miejsca w pamieci
func safeGetTask(b bool, t []task) *task {
	if !b {
		return nil
	}
	return &t[0]
}
func safeGetMachine(b bool, t []machine) *machine {
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
func machineGetGuardian(b bool, c chan<- *machine) chan<- *machine {
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
	ops := [3]string{"+", "*"}
	for {
		i := rand.Intn(2)
		o := ops[i]
		//  stworzenie nowego zadania
		t := &task{
			firstArg:  rand.Intn(10),
			secondArg: rand.Intn(10),
			op:        o,
			notSolved: true}
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

func service(reportMachineChan <-chan *machine, getMachineChan chan<- *machine, machineFixedChan <-chan *machine) {
	toFixList := make([]machine, 0)
	inRepairList := make([]machine, 0)
	for {
		select {
		// obsługa wstawienia zadania do listy zadan
		case msg := <-reportMachineChan:
			{
				alredyReported := false
				for i := 0; i < len(toFixList); i++ {
					if toFixList[i] == *msg {
						alredyReported = true
						break
					}
				}
				if !alredyReported {
					for i := 0; i < len(inRepairList); i++ {
						if inRepairList[i] == *msg {
							alredyReported = true
							break
						}
					}
				}
				if !alredyReported {
					toFixList = append(toFixList, *msg)
				}
			}
		// obsługa pobrania zadania z listy zadan
		case machineGetGuardian(len(toFixList) > 0, getMachineChan) <- safeGetMachine(len(toFixList) > 0, toFixList):
			{
				inRepairList = append(inRepairList, toFixList[0])
				toFixList = toFixList[1:]
			}
		// wypisanie zawartości listy zadań
		case msg := <-machineFixedChan:
			{
				index := -1
				for i := 0; i < len(inRepairList); i++ {
					if inRepairList[i] == *msg {
						index = i
						break
					}
				}
				part1 := inRepairList[0:index]
				part2 := inRepairList[index+1 : len(inRepairList)]
				inRepairList = part1
				for i := 0; i < len(part2); i++ {
					inRepairList = append(inRepairList, part2[i])
				}
			}
		}

	}
}
func serviceWorker(id int, getMachineChan <-chan *machine, machineFixedChan chan<- *machine, addMachines []chan chan bool, multMachines []chan chan bool) {
	sWorkerChan := make(chan bool)
	for {
		select {
		case msg := <-getMachineChan:
			{
				time.Sleep(time.Duration(dif.ServiceWorkerTravelTime) * time.Millisecond)
				if msg.mType == 0 {
					if dif.LoudMode {
						fmt.Println("Pracownik serwisowy nr", id, "naprawia maszyne dodajaca nr", msg.id)
					}

					addMachines[msg.id] <- sWorkerChan
					<-sWorkerChan
				} else {
					if dif.LoudMode {
						fmt.Println("Pracownik serwisowy nr", id, "naprawia maszyne mnozaca nr", msg.id)
					}

					multMachines[msg.id] <- sWorkerChan
					<-sWorkerChan
				}
				machineFixedChan <- msg
			}
		}
	}
}

// wątek pracownika
func worker(id int, taskGetChan <-chan *task, resultWriteChan chan<- int, workerChan chan *task, addMachines []chan *info, multMachines []chan *info, workerStateChan chan<- *workerInfo, reportMachineChan chan<- *machine) {
	isImpatient := rand.Intn(2)
	solved := 0
	s := "niecierpliwy"
	if isImpatient == 0 {
		s = "cierpliwy"
	}
	stat := &workerInfo{
		id:        id,
		impatient: s,
		solved:    solved}
	workerStateChan <- stat
	for {
		select {
		// pobranie zadania z listy zadan
		case msg := <-taskGetChan:
			{
				if dif.LoudMode {
					fmt.Println("Pracownik nr", id, "rozwiazuje zadanie:", *msg)
				}
				// rozwiazanie zadania
				t := *msg

				if isImpatient == 0 {
					switch msg.op {
					case "+":
						{
							machineID := rand.Intn(dif.NumberOfAddMachines)
							for {
								pack := &info{
									aTask:    msg,
									userChan: workerChan}
								addMachines[machineID] <- pack
								t = *<-workerChan
								if !t.notSolved {
									break
								}
								m := &machine{
									id:    machineID,
									mType: 0}
								fmt.Println("Pracownik nr", id, "zglosil maszyne dodajaca:", machineID)
								reportMachineChan <- m
								machineID = (machineID + 1) % dif.NumberOfAddMachines
								time.Sleep(time.Duration(dif.WorkerSleepTime) * time.Millisecond)

							}

						}
					case "*":
						{
							machineID := rand.Intn(dif.NumberOfMultMachines)

							for {
								pack := &info{
									aTask:    msg,
									userChan: workerChan}
								multMachines[machineID] <- pack
								t = *<-workerChan
								if !t.notSolved {
									break
								}
								m := &machine{
									id:    machineID,
									mType: 1}
								fmt.Println("Pracownik nr", id, "zglosil maszyne mnozaca:", machineID)
								reportMachineChan <- m
								machineID = (machineID + 1) % dif.NumberOfMultMachines
								time.Sleep(time.Duration(dif.WorkerSleepTime) * time.Millisecond)

							}

						}
					}
				} else {
					switch msg.op {
					case "+":
						{

							machineID := 0
							for {
								pack := &info{
									aTask:    msg,
									userChan: workerChan}
								acquired := false
								for !acquired {
									select {
									case addMachines[machineID] <- pack:
										acquired = true
									case <-time.After(time.Duration(dif.ImpatientWorkerWaitTime) * time.Millisecond):
										machineID = (machineID + 1) % dif.NumberOfAddMachines
									}
								}
								t = *<-workerChan
								if !t.notSolved {
									break
								}
								m := &machine{
									id:    machineID,
									mType: 0}
								fmt.Println("Pracownik nr", id, "zglosil maszyne dodajaca:", machineID)
								reportMachineChan <- m
								machineID = (machineID + 1) % dif.NumberOfAddMachines
								time.Sleep(time.Duration(dif.WorkerSleepTime) * time.Millisecond)
							}

						}
					case "*":
						{
							machineID := 0
							for {
								pack := &info{
									aTask:    msg,
									userChan: workerChan}
								acquired := false
								for !acquired {
									select {
									case multMachines[machineID] <- pack:
										acquired = true
									case <-time.After(time.Duration(dif.ImpatientWorkerWaitTime) * time.Millisecond):
										machineID = (machineID + 1) % dif.NumberOfMultMachines
									}
								}
								t = *<-workerChan
								if !t.notSolved {
									break
								}
								fmt.Println("Pracownik nr", id, "zglosil maszyne mnozaca:", machineID)
								m := &machine{
									id:    machineID,
									mType: 1}
								reportMachineChan <- m
								machineID = (machineID + 1) % dif.NumberOfMultMachines
								time.Sleep(time.Duration(dif.WorkerSleepTime) * time.Millisecond)
							}

						}
					}
				}

				// wstawienie rozwiazania do magazynu
				resultWriteChan <- t.result
				solved++
				stat := &workerInfo{
					id:        id,
					impatient: s,
					solved:    solved}
				workerStateChan <- stat
				if dif.LoudMode {
					fmt.Println("Pracownik nr", id, "otrzymał wynik:", t.result)
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

func getWorkerStats(workerStatsChan <-chan *workerInfo, getWorkerStat <-chan chan bool) {
	workerStats := make([]workerInfo, dif.NumberOfWorkers)
	for {
		select {
		case msg := <-workerStatsChan:
			{
				workerStats[msg.id-1] = *msg
			}

		case msg := <-getWorkerStat:
			{
				fmt.Println("hi")

				for i := range workerStats {
					aWorker := workerStats[i]
					fmt.Println("Pracownik nr", aWorker.id, " (", aWorker.impatient, ") rozwiazal :", aWorker.solved, " zadan")

				}
				msg <- true
			}
		}
	}
}

// wątek to obsługi typu "spokojnego"
func prompt(getTaskListState chan<- chan bool, getWarehouseState chan<- chan bool, getWorkerStat chan<- chan bool) {
	getReturn := make(chan bool)
	scanner := bufio.NewScanner(os.Stdin)
	for {
		// wyswietlenie menu dla trybu cichego
		fmt.Println("Menu")
		fmt.Println("1. Wyswietl stan magazynu")
		fmt.Println("2. Wyswietl liste zadań do wykonania")
		fmt.Println("3. Wyswietl statystyki pracownikow")
		fmt.Print("Wybierz nr czynnosci: ")
		scanner.Scan()
		text := scanner.Text()
		// wykonanie czynnosci wybranej przez uzytkownika
		switch text {
		case "1":
			{
				getWarehouseState <- getReturn
				<-getReturn
			}
		case "2":
			{
				getTaskListState <- getReturn
				<-getReturn
			}
		case "3":
			{
				getWorkerStat <- getReturn
				<-getReturn
			}
		default:
			{
				fmt.Println("Podano niepoprawny nr czynnosci")
			}
		}

	}
}

func addMachine(id int, machineChan chan *info, backdoorMachineChan chan chan bool) {
	broken := false
	for {
		if !broken {
			prob := rand.Intn(101)
			if (prob < dif.MachineBreakProbabilit) && (prob > 0) {
				broken = true
				fmt.Println("Zepsuła sie maszyna dodajaca nr", id)
			}
		}
		select {
		case msg := <-machineChan:
			{
				task := msg.aTask
				if !broken {
					time.Sleep(time.Duration(dif.AddMachineWorkTime) * time.Millisecond)
					task.result = task.firstArg + task.secondArg
					task.notSolved = false
				}

				msg.userChan <- task
			}
		case msg := <-backdoorMachineChan:
			{
				fmt.Println("Naprawiono maszyne dodajaca nr", id)
				broken = false
				msg <- true
			}
		}

	}
}
func multMachine(id int, machineChan chan *info, backdoorMachineChan chan chan bool) {
	broken := false
	for {
		if !broken {
			prob := rand.Intn(101)
			if (prob < dif.MachineBreakProbabilit) && (prob > 0) {
				broken = true
				fmt.Println("Zepsuła sie maszyna mnozaca nr", id)
			}
		}
		select {
		case msg := <-machineChan:
			{
				task := msg.aTask
				if !broken {
					time.Sleep(time.Duration(dif.AddMachineWorkTime) * time.Millisecond)
					task.result = task.firstArg * task.secondArg
					task.notSolved = false
				}

				msg.userChan <- task
			}
		case msg := <-backdoorMachineChan:
			{
				fmt.Println("Naprawiono maszyne mnozaca nr", id)
				broken = false
				msg <- true
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
	getWorkerStat := make(chan chan bool)

	reportMachineChan := make(chan *machine)

	getMachineChan := make(chan *machine)

	machineFixedChan := make(chan *machine)

	backdoorAddMachineChans := make([]chan chan bool, dif.NumberOfAddMachines)
	for i := range backdoorAddMachineChans {
		backdoorAddMachineChans[i] = make(chan chan bool)
	}
	backdoorMultMachineChans := make([]chan chan bool, dif.NumberOfMultMachines)
	for i := range backdoorMultMachineChans {
		backdoorMultMachineChans[i] = make(chan chan bool)
	}

	workerChans := make([]chan *task, dif.NumberOfWorkers)
	for i := range workerChans {
		workerChans[i] = make(chan *task)
	}

	workerStateChans := make(chan *workerInfo)

	addMachineChans := make([]chan *info, dif.NumberOfAddMachines)
	for i := range addMachineChans {
		addMachineChans[i] = make(chan *info)
	}

	multMachineChans := make([]chan *info, dif.NumberOfMultMachines)
	for i := range multMachineChans {
		multMachineChans[i] = make(chan *info)
	}

	// wywołanie wątku prezesa
	go ceo(taskWriteChan)
	// wywołanie wątku listy zadań
	go taskMag(taskWriteChan, taskGetChan, getTaskListState)

	go service(reportMachineChan, getMachineChan, machineFixedChan)

	for w := 1; w <= dif.NumberOfServiceWorkers; w++ {
		go serviceWorker(w, getMachineChan, machineFixedChan, backdoorAddMachineChans, backdoorMultMachineChans)
	}
	// wywołanie wątków pracowników
	for w := 1; w <= dif.NumberOfAddMachines; w++ {
		go addMachine(w-1, addMachineChans[w-1], backdoorAddMachineChans[w-1])
	}
	for w := 1; w <= dif.NumberOfMultMachines; w++ {
		go multMachine(w-1, multMachineChans[w-1], backdoorMultMachineChans[w-1])
	}
	for w := 1; w <= dif.NumberOfWorkers; w++ {
		go worker(w, taskGetChan, resultWriteChan, workerChans[w-1], addMachineChans, multMachineChans, workerStateChans, reportMachineChan)
	}
	// wywołanie wątku magazynu
	go resultWarehouse(resultWriteChan, resultGetChan, getWarehouseState)
	// wywołanie wątku klienta
	go client(resultGetChan)
	// wywołanie wątku dla trybu "spokojnego"
	go getWorkerStats(workerStateChans, getWorkerStat)
	if !dif.LoudMode {
		go prompt(getTaskListState, getWarehouseState, getWorkerStat)
	}
	for {
	}

}
