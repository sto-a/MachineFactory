module ActorSchema

    open Akka.FSharp
    open Akka.Actor

    open Actors
    open MessageTypes

    let system = ActorSystem.Create "MachineFactory"

    //------------------------------------------------------------
    let echoActorRef = 
        actorOf printMsgFun
        |> spawn system "print-actor" 

    //------------------------------------------------------------
    let machineControlActorRef = 
        let name = "machine-control"
        system.ActorOf(Props.Create(fun () -> new MachineControlActorType(name)), name)

    //------------------------------------------------------------
    let dashboardActorRef =
        let name = "dashboard"
        system.ActorOf(Props.Create(fun () -> new DashboardActorType(name)), name)

    //------------------------------------------------------------
    let spawnChildMachine name mailbox=
        spawn mailbox name (machineActorFun machineControlActorRef dashboardActorRef)

    let machineMapRouterActorRef =
        mapRouterActorFun spawnChildMachine (fun cmd -> cmd.id) 
        |> spawn system "machine-map-router"

    //------------------------------------------------------------
    let stateControlActorRef = 
        let name = "state-control"
        system.ActorOf(Props.Create(fun () -> new StateControlActorType(name, machineMapRouterActorRef)), name)

