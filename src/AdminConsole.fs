module AdminConsole
    
    open Akka.Actor
    open Akka.FSharp
    open MessageTypes


    type AdminConsolePrivateMessage = {
            a : IActorRef
            msg : AdminMessage
        }

    type AdminConsole() as this =
        let _timerPrintStatus = new System.Timers.Timer(1000)
        let mutable _actorList : IActorRef list = []
        let mutable _status : Map<string,AdminMessage> = Map.empty 
        let mutable _time = None

        [<DefaultValue>]
        val mutable private _board : IActorRef

        do
            _timerPrintStatus.Stop()

            _timerPrintStatus.AutoReset <- true
            _timerPrintStatus.Elapsed.Add (fun t-> 
                this.sendMessage
                let timeNew = System.DateTime.Now
                match _time with
                | None ->
                    _time <- Some timeNew
                | Some timeOld ->
                    this.printStatus (1000.0 / (timeNew - timeOld).TotalMilliseconds)
                    _time <- Some timeNew
            )
            
        member this.Start() = 
            _timerPrintStatus.Start()

        member this.Stop() =
            _timerPrintStatus.Stop()

        member this.sendMessage = 
            _actorList
            |> List.iter (fun a -> 
               this._board <! {a=a; msg=Request(this._board)}
            ) 

        member this.printStatus tc = 
            System.Console.Clear()
            System.Console.SetCursorPosition(0,0)
            _status 
            |> Map.iter (fun k v -> 
                match v with
                | Request _ -> ()
                | Responce ( id, mc) ->
                    printfn "%s %s сообщ/с" id (System.String.Format("{0,10:0.00}", (float)mc*tc))
            )

        member this.info msg =
            match msg with
            | Request _ -> ()
            | Responce (id, mc) ->
                _status <- Map.change id (fun v -> Some msg) _status
        
        member this.initActorStatisticList (system:ActorSystem, actorPathList: IActorRef list, ?name:string) =
            let _name =
                match name with 
                | None ->  "admin-console"
                | Some n -> n

            this._board <- system.ActorOf(Props.Create(fun () -> new AdminConsoleActorType()), _name)
            _actorList <- actorPathList

        member this.Board with get() = this._board

        [<DefaultValue>]
        static val mutable private instance : AdminConsole option

        static member Instance
            with get() =
                match AdminConsole.instance with
                | Some a -> a
                | None ->
                    AdminConsole.instance <- Some (new AdminConsole())
                    AdminConsole.instance.Value

    and AdminConsoleActorType() as this =
        inherit ReceiveActor()

        do
            this.Receive<AdminMessage> (fun msg -> 
                match msg with
                | Request _ -> ()
                | Responce _ -> AdminConsole.Instance.info msg
            )
            this.Receive<AdminConsolePrivateMessage> (fun msg ->
                msg.a <! msg.msg
            )


    type MessageStatistics(name) as this =
        inherit ReceiveActor()

        [<DefaultValue>]
        val mutable _countMsg : int
        let mutable _name = name

        member this.MenadgeAction =
            this._countMsg <- this._countMsg + 1

        do  
            this.Receive<AdminMessage> (fun msg ->
                this.MenadgeAction
                match msg with
                | Request (sender) ->
                    let r = Responce(_name, this._countMsg)
                    this._countMsg <- 0
                    sender <! r        
                | Responce _ -> ()
                    
            )
