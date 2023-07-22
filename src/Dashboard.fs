module Dashboard

    open Config
    open MessageTypes

    type DashboardStore() =
        
        let _timer = new System.Timers.Timer(DASHBOARD_TIME_INTERVAL)
        let mutable _time = 0

        let mutable _ticket : Map<int,int*bool> = Map.empty
        let mutable _states : Map<int,int*MachineStateValue> = Map.empty

        do
            _timer.Elapsed.Add (fun t ->
                _time <- _time + 1
            )
            _timer.Start()

        member _.Push msg =
            match msg with
                | Ticket id -> 
                    _ticket <- Map.add id (_time, true) _ticket
                | State (id, ef) ->
                    _states <- Map.add id (_time, ef) _states

        member _.Time with get() = _time

        member _.States with get() = _states

        member _.Tickets with get() = _ticket

        member _.TicketsClear() =_ticket <- Map.empty

        [<DefaultValue>]
        static val mutable private instance : DashboardStore option

        static member Instance
            with get() =
                match DashboardStore.instance with
                | Some mw -> mw
                | None -> 
                    DashboardStore.instance <- Some (new DashboardStore())
                    DashboardStore.instance.Value