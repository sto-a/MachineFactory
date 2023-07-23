module Actors
   
    open Akka.Actor
    open Akka.FSharp

    open Config
    open MessageTypes
    open MachineWizard
    open Dashboard

    let printMsgFun (msg:obj) = 
        match msg with
        | :? MachineState as st ->
            if st.id < System.Console.BufferHeight then
                System.Console.SetCursorPosition(0, st.id)
                printf "                                           "
                System.Console.SetCursorPosition(0, st.id)
                printfn "%i %f %b %b" st.id st.t st.engineOn st.fanOn
        | :? MachineMessage as mm ->
            if mm.id < System.Console.BufferHeight then
                System.Console.SetCursorPosition(0, mm.id)
                printf "                                           "
                System.Console.SetCursorPosition(0, mm.id)
                let engineOn = 
                    match mm.ef with
                    | EngineOnFanOff | EngineOnFanOn -> true
                    | _ -> false
                let fanOn =
                    match mm.ef with
                    | EngineOnFanOn | EngineOffFanOn -> true
                    | _ -> false
                let t = 
                    match mm.t with
                    | Less50 -> "<50"
                    | Less75 -> "<75"
                    | Less95 -> "<95"
                    | Greater95 -> "---"
                printfn "%i %s %b %b" mm.id t engineOn fanOn
        | _ -> printfn "%A" msg

    let machineActorFun machineControlActorRef dashboardActorRef (mailbox: Actor<MachineMessage>) =

        let execAction actNew id name = 
            match actNew with
                | ADoNothing         -> ()
                | AEngineDNFanOff    -> machineControlActorRef <! FanStop(id)
                | AEngineDNFanOn     -> machineControlActorRef <! FanStart(id)
                | AEngineOffFanDN    -> machineControlActorRef <! EngineStop(id)
                | AEngineOnFanDN     -> machineControlActorRef <! EngineStart(id)
                | AEngineOffFanOff   -> 
                    machineControlActorRef <! EngineStop(id)
                    machineControlActorRef <! FanStop(id)
                | AEngineOffFanOn    -> 
                    machineControlActorRef <! EngineStop(id)
                    machineControlActorRef <! FanStart(id)
                | AEngineOnFanOff    -> 
                    machineControlActorRef <! EngineStart(id)
                    machineControlActorRef <! FanStop(id)
                | AEngineOnFanOn     -> 
                    machineControlActorRef <! EngineStart(id)
                    machineControlActorRef <! FanStart(id)


        let isTrable actOld ef=
            match actOld, ef with
            | ADoNothing, EngineOffFanOff -> false
            | ADoNothing, EngineOffFanOn -> false
            | ADoNothing, EngineOnFanOff -> false
            | ADoNothing, EngineOnFanOn -> false

            | AEngineDNFanOff, EngineOffFanOff -> false
            | AEngineDNFanOff, EngineOffFanOn -> true
            | AEngineDNFanOff, EngineOnFanOff -> false
            | AEngineDNFanOff, EngineOnFanOn -> true

            | AEngineDNFanOn, EngineOffFanOff -> true
            | AEngineDNFanOn, EngineOffFanOn -> false
            | AEngineDNFanOn, EngineOnFanOff -> true
            | AEngineDNFanOn, EngineOnFanOn -> false

            | AEngineOffFanDN, EngineOffFanOff -> false
            | AEngineOffFanDN, EngineOffFanOn -> false
            | AEngineOffFanDN, EngineOnFanOff -> true
            | AEngineOffFanDN, EngineOnFanOn -> true

            | AEngineOnFanDN, EngineOffFanOff -> true
            | AEngineOnFanDN, EngineOffFanOn -> true
            | AEngineOnFanDN, EngineOnFanOff -> false
            | AEngineOnFanDN, EngineOnFanOn -> false

            | AEngineOffFanOff, EngineOffFanOff -> false
            | AEngineOffFanOff, EngineOffFanOn -> true
            | AEngineOffFanOff, EngineOnFanOff -> true
            | AEngineOffFanOff, EngineOnFanOn -> true

            | AEngineOffFanOn, EngineOffFanOff -> true
            | AEngineOffFanOn, EngineOffFanOn -> false
            | AEngineOffFanOn, EngineOnFanOff -> true
            | AEngineOffFanOn, EngineOnFanOn -> true

            | AEngineOnFanOff, EngineOffFanOff -> true
            | AEngineOnFanOff, EngineOffFanOn -> true
            | AEngineOnFanOff, EngineOnFanOff -> false
            | AEngineOnFanOff, EngineOnFanOn -> true

            | AEngineOnFanOn, EngineOffFanOff -> true
            | AEngineOnFanOn, EngineOffFanOn -> true
            | AEngineOnFanOn, EngineOnFanOff -> true
            | AEngineOnFanOn, EngineOnFanOn -> false

        let rec state0 (actOld, stateOld:MachineMessage option, cmsg) = actor {

                let! stateNew = mailbox.Receive()

                if isTrable actOld stateNew.ef then dashboardActorRef <! Ticket(stateNew.id)
                let cmsgNew =
                    if cmsg > MAX_SILENCE_INTERVAL || stateOld.IsSome && stateOld.Value.ef <> stateNew.ef then 
                        dashboardActorRef <! State(stateNew.id, stateNew.ef)
                        0
                    else 
                        cmsg + 1

                let actNew, isHold =
                    match stateNew.t, stateNew.ef with
                    | Less50, EngineOffFanOff -> AEngineOnFanDN, false
                    | Less50, EngineOffFanOn -> AEngineOnFanOff, false
                    | Less50, EngineOnFanOff -> ADoNothing, false
                    | Less50, EngineOnFanOn -> AEngineDNFanOff, false

                    | Less75, EngineOffFanOff -> AEngineOnFanDN, false
                    | Less75, EngineOffFanOn -> AEngineOnFanOff, false
                    | Less75, EngineOnFanOff -> ADoNothing, false 
                    | Less75, EngineOnFanOn -> AEngineDNFanOff, false 

                    | Less95, EngineOffFanOff -> AEngineOnFanOn, false
                    | Less95, EngineOffFanOn -> AEngineOnFanDN, false
                    | Less95, EngineOnFanOff -> AEngineDNFanOn, false
                    | Less95, EngineOnFanOn -> ADoNothing, false

                    | Greater95, EngineOffFanOff -> ADoNothing, true
                    | Greater95, EngineOffFanOn -> AEngineDNFanOff, true
                    | Greater95, EngineOnFanOff -> AEngineOffFanDN, true
                    | Greater95, EngineOnFanOn -> AEngineOffFanOff, true

                execAction actNew stateNew.id mailbox.Self.Path.Name

                if isHold then return! state1 (actNew,(Some stateNew), cmsgNew)
                else return! state0 (actNew,(Some stateNew), cmsgNew)
            }
        and state1 (actOld, stateOld, cmsg) = actor{

                let! stateNew = mailbox.Receive()

                if isTrable actOld stateNew.ef then dashboardActorRef <! Ticket(stateNew.id)
                let cmsgNew =
                    if cmsg > MAX_SILENCE_INTERVAL || stateOld.IsSome && stateOld.Value.ef <> stateNew.ef then 
                        dashboardActorRef <! State(stateNew.id, stateNew.ef)
                        0
                    else 
                        cmsg + 1
                
                let actNew, isHold =
                    match stateNew.t, stateNew.ef with
                    | Less50, EngineOffFanOff -> AEngineOnFanDN, true
                    | Less50, EngineOffFanOn -> AEngineOnFanOff, true
                    | Less50, EngineOnFanOff -> ADoNothing, true
                    | Less50, EngineOnFanOn -> AEngineDNFanOff, true

                    | Less75, EngineOffFanOff -> ADoNothing, false
                    | Less75, EngineOffFanOn -> AEngineDNFanOff, false
                    | Less75, EngineOnFanOff -> AEngineOffFanDN, false
                    | Less75, EngineOnFanOn -> AEngineOffFanOff, false

                    | Less95, EngineOffFanOff -> ADoNothing, false
                    | Less95, EngineOffFanOn -> AEngineDNFanOff, false
                    | Less95, EngineOnFanOff -> AEngineOffFanDN, false
                    | Less95, EngineOnFanOn -> AEngineOffFanOff, false

                    | Greater95, EngineOffFanOff -> ADoNothing, false
                    | Greater95, EngineOffFanOn -> AEngineDNFanOff, false
                    | Greater95, EngineOnFanOff -> AEngineOffFanDN, false
                    | Greater95, EngineOnFanOn -> AEngineOffFanOff, false
                
                execAction actNew stateNew.id mailbox.Self.Path.Name

                if not isHold then return! state1 (actNew,(Some stateNew), cmsgNew)
                else return! state0 (actNew,(Some stateNew), cmsgNew)
            }

        state0 (ADoNothing, None, 0)

    let mapRouterActorFun spawnChild (cmpf:'a->'b) (mailbox: Actor<'a>) =
        let rec imp map = actor {
                let! msg = mailbox.Receive()

                let key = cmpf msg
                let mapNew = 
                    match Map.tryFind key map with
                    | None -> 
                        let a = spawnChild (key.ToString()) mailbox
                        a <! msg
                        Map.add key a map
                    | Some a -> 
                        a <! msg
                        map
                
                return! imp mapNew
            }
        imp Map.empty

    open AdminConsole

    type MachineControlActorType(name) as this =
        inherit MessageStatistics(name)
        do 
            this.Receive<MachineCommand> (fun msg -> this.MenadgeAction; MachineWizard.Instance.command msg)

    type StateControlActorType(name, spawnChild) as this =
        inherit MessageStatistics(name)

        do 
            this.Receive<MachineState> (fun (msg:MachineState)-> 
                this.MenadgeAction
                let tn = 
                    match msg.t with
                    | t when t < 50.0 -> Less50
                    | t when t < 75.0 -> Less75
                    | t when t < 95.0 -> Less95
                    | _ -> Greater95
                let ef: MachineStateValue =
                        match msg.engineOn, msg.fanOn with
                        | false, false ->   EngineOffFanOff
                        | false, true ->    EngineOffFanOn
                        | true, false ->    EngineOnFanOff
                        | true, true ->     EngineOnFanOn
                let child = spawnChild
                let messageChild : MachineMessage = {id = msg.id; t = tn; ef = ef }
                child <! messageChild
            )

    type DashboardActorType(name) as this =
        inherit MessageStatistics(name)

        do
            this.Receive<DashboardMessage> (fun msg ->
                this.MenadgeAction
                DashboardStore.Instance.Push msg
            )
        