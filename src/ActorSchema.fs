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
        actorOf machineControlMsgFun
        |> spawn system "machine-control"

    //------------------------------------------------------------
    let dashboardActorRef =
        actorOf dashboardMsgFun
        |> spawn system "dashboard"

    //------------------------------------------------------------
    let spawnChildMachine name mailbox=
        spawn mailbox name (machineActorFun machineControlActorRef dashboardActorRef)

    let machineMapRouterActorRef =
        mapRouterActorFun spawnChildMachine (fun cmd -> cmd.id) 
        |> spawn system "machine-map-router"

    //------------------------------------------------------------
    let stateControlActorRef = 
        actorOf (stateControlMsgFun machineMapRouterActorRef)
        |> spawn system "state-control"

