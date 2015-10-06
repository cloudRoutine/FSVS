namespace EventSample
open FSharp.Control
open System

module Alarm =


    let alarmEvent : (bool*int) Event = Event<_>()

    let alarmStream = alarmEvent.Publish

    let alarmMessage (pressed:bool) =
        if pressed then "Wake Up!!! Snooze time is over" else "Wake up!"


    type AlarmEventArgs (snoozePressed:bool,nrings:int) =
        inherit EventArgs()
        member val NumRings = nrings with get
        member val SnoozePressed = snoozePressed with get
        member __.AlarmTest 
            with get() = if snoozePressed then "Wake Up!!! Snooze time is over" else "Wake up!"
        
    type AlarmEventHandler = delegate of obj * AlarmEventArgs -> unit

    type AlarmClock(args) as self =
     
//        let Alarm = new AlarmEventHandler(self.OnAlarm)
        member val Stop = false with get, set
        member val SnoozePressed= false with get, set

//        member self.OnAlarm(_) =
//            self

        


    //alarmStream.Subscribe()


    ()
