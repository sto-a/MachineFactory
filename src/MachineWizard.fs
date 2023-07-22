module MachineWizard
    
    open Akka.Actor
    open Akka.FSharp

    open Config
    open MessageTypes

    let rnd = new System.Random()
    let rndlh low hi = (hi-low) * rnd.NextDouble() + low

    type Machine(id:int) =
        //описание показателей матмодели
        let teps = (rndlh TEMPERATURE_CHANGE_ENGINE_MIN TEMPERATURE_CHANGE_ENGINE_MAX) / 10.0
        let tfps = (rndlh TEMPERATURE_CHANGE_FAN_MIN TEMPERATURE_CHANGE_FAN_MAX) / 10.0
        let tenvps = (rndlh TEMPERATURE_CHANGE_ENV_MIN TEMPERATURE_CHANGE_ENV_MAX) / 10.0
        let pDropCommand = rnd.NextDouble() * PDROP_COMMANG_MESSAGE

        //переменные модели
        let mutable temp = rnd.NextDouble() * MACHINE_START_TEMPERATURE_MAX
        [<DefaultValue>]
        val mutable engineOn: bool
        [<DefaultValue>]
        val mutable fanOn:bool

        member this.getId = id
        member this.getTemp = temp

        member _.dropCommand = rnd.NextDouble() < pDropCommand

        member this.next () =
                match this.engineOn, this.fanOn with
                | false, false -> temp <- max (temp - tenvps) 0.0
                | false, true -> temp <- max (temp - tfps) 0.0
                | true, false -> temp <- temp + teps
                | true, true -> temp <- max (temp + teps - tfps) 0.0
        
        member this.getState () ={id = id; t = temp; engineOn = this.engineOn; fanOn = this.fanOn}

    type MachineWizard() =
        [<DefaultValue>]
        val mutable private timer : System.Timers.Timer
        [<DefaultValue>]
        val mutable private machines : Machine[]
        let mutable dropMachine = -1
        let mutable dropCount = 0

        member this.init(stateContorlActorRef:IActorRef) =
            this.timer <- new System.Timers.Timer(TIME_INTERVAL)
            this.machines <- Array.init MACHINE_COUNT (fun i -> new Machine(i))
            this.timer.AutoReset <- true
            this.timer.Elapsed.Add (fun t ->
                this.machines
                |> Array.iteri (fun i m -> 

                    m.next()
                    if dropCount <= 0 then 
                        if rnd.NextDouble() < PDROP_MACHINE_STATE_MESSAGE then 
                            dropMachine <- rnd.Next(MACHINE_COUNT)
                            dropCount <- rnd.Next(DROP_MACHINE_INTERVAL_MIN, DROP_MACHINE_INTERVAL_MAX)
                    else dropCount <- dropCount - 1

                    if i <> dropMachine then
                        
                        stateContorlActorRef <! m.getState()
                )
            )

        member this.GetMachineList () = this.machines |> Array.toList

        member this.Stop() = this.timer.Stop()
        member this.Start() = this.timer.Start()

        member this.command cmd =
            match cmd with
            | EngineStart i -> if not this.machines.[i].dropCommand then this.machines.[i].engineOn <- true
            | EngineStop i -> if not this.machines.[i].dropCommand then this.machines.[i].engineOn <- false
            | FanStart i -> if not this.machines.[i].dropCommand then this.machines.[i].fanOn <- true
            | FanStop i -> if not this.machines.[i].dropCommand then this.machines.[i].fanOn <- false

        [<DefaultValue>]
        static val mutable private instance : MachineWizard option

        static member Instance
            with get() =
                match MachineWizard.instance with
                | Some mw -> mw
                | None -> 
                    MachineWizard.instance <- Some (new MachineWizard())
                    MachineWizard.instance.Value

            