﻿
//==========AKKA.NET==============
open ActorSchema
//==========MACHNE WIZARD==============
open MachineWizard
MachineWizard.Instance.init(stateControlActorRef)
MachineWizard.Instance.Start()
//==========ADMIN CONSOLE==============
open AdminConsole
AdminConsole.Instance.initActorStatisticList(system, [stateControlActorRef; dashboardActorRef; machineControlActorRef])
AdminConsole.Instance.Start()
//===========VIEWER====================
open System
open System.Windows.Forms
open Viewer 

Application.EnableVisualStyles()
Application.SetCompatibleTextRenderingDefault(false)

[<STAThread>]
Application.Run(dashboardForm)

