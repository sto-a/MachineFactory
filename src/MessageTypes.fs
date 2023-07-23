module MessageTypes

    type TemperaturaStateValue =
        | Less50
        | Less75
        | Less95
        | Greater95

    type MachineStateValue =
        | EngineOffFanOff
        | EngineOffFanOn
        | EngineOnFanOff
        | EngineOnFanOn

    type ActionStateValue =
        | ADoNothing
        | AEngineDNFanOff
        | AEngineDNFanOn
        | AEngineOffFanDN
        | AEngineOnFanDN
        | AEngineOffFanOff
        | AEngineOffFanOn
        | AEngineOnFanOff
        | AEngineOnFanOn


    type MachineCommand =
        | EngineStart of int
        | EngineStop of int
        | FanStart of int
        | FanStop of int

    type MachineState ={
        id:int
        t:float
        engineOn:bool
        fanOn:bool
    }

    type MachineMessage = {
        id : int
        t : TemperaturaStateValue
        ef : MachineStateValue
    }

    type DashboardMessage =
        | Ticket of int     
        | State of int*MachineStateValue  

    open Akka.Actor

    type AdminMessage =
        | Request of IActorRef
        | Responce of string*int