module Viewer

    open System.Windows.Forms
    open System.Drawing

    open Config
    open MessageTypes
    open Dashboard
    open MachineWizard

    type DashboardForm() as this =
        inherit Form()
        
        let _timerRefresh = new System.Windows.Forms.Timer()

        do
            this.DoubleBuffered <- true
            _timerRefresh.Interval <- VIEWER_REFRESH_INTERVAL
            _timerRefresh.Tick.Add (fun e -> this.Refresh())

            base.Text <- "Machine factory. Для квитирования сообщений кликните по форме левой кнопкой мыши."
            base.StartPosition <- FormStartPosition.CenterScreen
            base.Size <- new Size(600, 480)
            base.TopMost <- true

        override this.OnLoad(e) =
            base.OnLoad(e)
            _timerRefresh.Start()
         

    let dashboardForm = new DashboardForm()

    //Form resources

    let imageList = new ImageList()
    imageList.ImageSize <- new Size(64, 64)
    imageList.Images.Add(Image.FromFile(".\\Picts\\engineOn.png"))
    imageList.Images.Add(Image.FromFile(".\\Picts\\engineOff.png"))
    imageList.Images.Add(Image.FromFile(".\\Picts\\engineOut.png"))
    imageList.Images.Add(Image.FromFile(".\\Picts\\fanOn.png"))
    imageList.Images.Add(Image.FromFile(".\\Picts\\fanOff.png"))
    imageList.Images.Add(Image.FromFile(".\\Picts\\fanOut.png"))
    imageList.TransparentColor <- Color.White

    let warningImageList = 
        let il = new ImageList()
        il.Images.Add(Image.FromFile(".\\Picts\\warning.png"))
        il.ImageSize <- new Size(32, 32)
        il.TransparentColor <- Color.White
        il

    let drawTempLine (gr:Graphics) x y t=
        let ti =  (min t 100.0) * (128.0 / 100.0)
        gr.DrawRectangle (Pens.Black, x, y + 62, (int)(t), 2)

    let drawWarning (gr:Graphics) x y =
        warningImageList.Draw(gr, new Point(x+48, y+16), 0)

    let drawMachineDashboard (gr:Graphics) x y (ef:MachineStateValue) =
        let e,f =
            match ef with
            | EngineOffFanOff   -> 1, 4
            | EngineOffFanOn    -> 1, 3
            | EngineOnFanOff    -> 0, 4
            | EngineOnFanOn     -> 0, 3
        imageList.Draw(gr, new Point(x, y), e)
        imageList.Draw(gr, new Point(x+64, y), f)

    let drawNotActiveMachine (gr:Graphics) x y = 
        imageList.Draw(gr, new Point(x, y), 2)
        imageList.Draw(gr, new Point(x+64, y), 5)
        
 
    let getDrawPosition (id:int) =
        let width = dashboardForm.Width - 3
        let height = dashboardForm.Height
        let controlWidth = 128 + 5
        let controlHeight = 64 + 3
        let countControlInWidth = width / controlWidth
        let y = id / countControlInWidth
        let x = id % countControlInWidth
        x*controlWidth + 3 , y*controlHeight

    //Form evants

    dashboardForm.MouseClick.Add (fun e ->
        if e.Button = MouseButtons.Left then
            DashboardStore.Instance.TicketsClear()
            dashboardForm.Refresh()
    )

    dashboardForm.Paint.Add (fun e ->
        let machines = MachineWizard.Instance.GetMachineList()
        let time =  DashboardStore.Instance.Time

        DashboardStore.Instance.States
        |> Map.iter (fun k v ->
            let x, y = getDrawPosition(k)
            if (fst v) + FREEZ_MACHINE_INTERVAL > time then
                drawMachineDashboard e.Graphics x y (snd v)
            else 
                drawNotActiveMachine e.Graphics x y
            let t = (machines |> List.find (fun s -> s.getId = k)).getTemp
            drawTempLine e.Graphics x y t
        )
        DashboardStore.Instance.Tickets
        |> Map.iter (fun k v ->
            let x, y = getDrawPosition(k)
            drawWarning e.Graphics x y
        )

    )

    dashboardForm.FormClosed.Add (fun e ->
        exit(0)
    )
