' Whoa-Nellie (Stern 2015)
 
 
' leftover code for desktop and 2 player that are not used
' 1.4 reduce rollover scores to 1pt as per youtube
' 1.5 add attract mode, reverse top lane lights, initially on then go off as you get them
' 1.6 - 10pts for top lanes when light is on or off, flash lights when nudged hard
'     - kicker cup +10pts per lit bumper points
' 1.7 - pf image changes, buzz sounds, BG for sounds, yellow light rules, kicker flashes bumpers, 5pt bumper is a dead bumper - no force.
'     - yellow lane skill shot - 200 when flashing, 50 when not - at start of ball.
'     - lane lights one of the other red rollovers as well, ditto for green rollover
'     - gobble gobble
' 1.8 - add highscore postit - not great for desktop :-(
' 1.8d - DOF by arngrim
' 2.0 - new actual ROM sounds, more corrected rules
Option Explicit
Randomize
 
Const cGameName = "whoa_nellie_em"
Const ballsize = "50"
 
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0
 
Dim HiScore
Dim GameOn
Dim Players
Dim UpBall
Dim Ball
Dim UpLight(2)
Dim Score(2)
Dim TempScore(2)
Dim Credit
Dim Credit1(2)
Dim Credit2(2)
Dim Tilted
Dim Tilts
Dim Up, pauseloop
Dim x, i, lno
Dim y, attractmodeflag, LFlag, RFlag
Dim BonusLights
 
Dim BallsPerGame, StandardMode, MusicFlag
 
Dim Controller
Dim DOFs
Dim B2SOn
Dim Req(220)
Dim rpos
Dim MusicFile, SVol,CVol,TVol  
 
Dim HiSc, Object
Dim Initial(4)
Dim HSInitial0, HSInitial1, HSInitial2
Dim EnableInitialEntry
Dim HSi,HSx, HSArray, HSiArray
Dim RoundHS, RoundHSPlayer
Dim ScoreMil, Score100K, Score10K, ScoreK, Score100, Score10, ScoreUnit
HSArray = Array("HS_0","HS_1","HS_2","HS_3","HS_4","HS_5","HS_6","HS_7","HS_8","HS_9","HS_Space","HS_Comma")
HSiArray = Array("HSi_0","HSi_1","HSi_2","HSi_3","HSi_4","HSi_5","HSi_6","HSi_7","HSi_8","HSi_9","HSi_10","HSi_11","HSi_12","HSi_13","HSi_14","HSi_15","HSi_16","HSi_17","HSi_18","HSi_19","HSi_20","HSi_21","HSi_22","HSi_23","HSi_24","HSi_25","HSi_26")
Dim MatchNumber, Balls, Shift
 
Req(26)="26 27 28 29 1e 2b 2c 2d 2e 2f " ' background music
Req(28)="1f " ' intro
Req(30)="c28" ' major award
Req(31)="cb2" ' take a picture
Req(32)="d52" ' high score
Req(36)="8d58d68dd8f98fe90590690990b91091491b92392593693793994895695995e95f"
Req(38)="8e08e18ec8f78f890a90c90d90f91691791891c92b93a"
Req(39)="8da8dc8de8e28e48e68e78e88e98ea8ee8ef8f28f68fc90090e91292492a92c92d92f93b93c93e940"
Req(40)="8fd90490391e91f920"
'54 bell
'55 cowbell
'56 high bell
Req(62)="9ee9f09f3a44afdb2fc4ec8ec92"
Req(78)="aa1aa4c1bc55c9fd2c"
Req(80)="d1b"
Req(81)="d40"
Req(82)="d2e"
Req(83)="bdf"
Req(84)="c14"
Req(85)="bce"
Req(86)="cb5"
Req(87)="cf5"
Req(88)="d5b"
Req(90)="7f1"
Req(91)="acfac7ac5acdad5ab6ab8ad1ab7ad4ab9acf9d8a43b0ec03c06cf8"
Req(93)="98f9909ba9d7ad2d01"
Req(97)="af2af5aff9c79c8"
Req(100)="b69bb6bd0c1faac"
Req(104)="d1b"
Req(114)="98f990991992993994995996"
Req(117)="9dd9deb09b0a9e69e79e8b87b88c71c72"
Req(120)="83083783e845"
Req(121)="c3fd33d34"
Req(126)="cdeb3eb45"
Req(127)="b5bc49c48"
Req(135)="7b973473b757" 'splat
Req(143)="ae69849879899f1a01a00aa5aa6b54b55b59b5acb0cff"
Req(147)="966abcac2"
Req(154)="7c070a72d75076576c77377a788703"
Req(166)="4d04d74de22722e23523c24a2512a52ac2b32ba2c12c82d62dd2e42eb2f22f930036b3723793803873953f73fe40544444b4524594fa50150850f51651d53254054762f63663d64464b8518ac"
Req(195)="86186886f876"
Req(197)="9ff"
Req(198)="04aa4b"
Req(199)="aeaaec"
Req(200)="ae7ae8"
Req(62)="9ee9f09f3a44a45afdb2fc4ec8ec92"
 
' Configurable Items
MusicFlag= TRUE      ' Set to TRUE to play background music
BallsPerGame = 5     ' 3 balls turns melon lights on for every ball
StandardMode = False  ' True means you have to get top rollovers twice -- On-blink-off.
SVol = .8   ' Speech Volume
TVol = .2   ' Background Track Volume
CVol = 1.0  ' Chime Volume
 
Set UpLight(1) = Up1
Set UpLight(2) = Up2
 
Sub table1_init()
  LoadEM
  Score(1) = 0
  Score(2) = 0
  Up = 1
  lno = 0 ' lamp seq flashing while in trough
   
  Hisc =100
  Initial(1) = 19: Initial(2) = 5: Initial(3) = 13
  LoadHighScore
  UpdatePostIt
  CreditReel.SetValue Credit
 
    If ShowDT = True Then
        For each object in backdropstuff
        Object.visible = 1  
        Next
    End If
   
    If ShowDt = False Then
        For each object in backdropstuff
        Object.visible = 0  
        Next
    End If
 
  If B2SOn Then
    Controller.B2SSetCredits Credit
    Controller.B2SSetGameOver 35,1
  End If
 
  TriggerLightsOff()
 
  AttractMode.Interval=500
  AttractMode.Enabled = True
  attractmodeflag=0
  AttractModeSounds.Interval=7000
  AttractModeSounds.Enabled = True
 
  If Credit > 0 Then DOF 121,1
End Sub
 
Sub Table1_KeyDown(ByVal keycode)
 
    If EnableInitialEntry = True Then EnterIntitals(keycode)
 
    If keycode = PlungerKey Then
        Plunger.PullBack:PlaySoundAt "fx_plungerpull",Plunger
    End If
 
    If keycode = LeftFlipperKey Then
      If GameOn = TRUE then
        If Tilted = FALSE then
        LeftFlipper.RotateToEnd
        flipperLtimer.interval=10
        flipperLtimer.Enabled = TRUE
        PlaySound SoundFX("FlipUpL", DOFFlippers), 0, .3, Pan(LeftFlipper), 0, 2000, 0, 1, AudioFade(LeftFlipper)
        DOF 101, 1
        End If
      End If
    End If
   
    If keycode = RightFlipperKey Then
      If GameOn = TRUE then
        If Tilted = FALSE then
           RightFlipper.RotateToEnd
          flipperLtimer.interval=10
          flipperRtimer.Enabled = TRUE
          PlaySound SoundFX("FlipUpR", DOFFlippers), 0,.3, Pan(RightFlipper), 0, -2000, 0, 1, AudioFade(RightFlipper)
          DOF 102, 1
         ' PlayAllReq(117)
        End If
      End If
    End If
   
    If keycode = LeftTiltKey Then
        Nudge 90, 2
        TiltCheck
    End If
   
    If keycode = RightTiltKey Then
        Nudge 270, 2
        TiltCheck
    End If
   
    If keycode = CenterTiltKey Then
        Nudge 0, 2
        TiltCheck
    End If
 
    if keycode = 6 then   ' Add coin
          if Credit < 9 then
            Credit = Credit + 1
            DOF 121, 1
            PlaysoundAt "coin3",Drain
        end if
        CreditReel.SetValue Credit
        If B2SOn Then
            Controller.B2SSetCredits Credit
        End If
        savehs
    end if
 
if keycode = 30 then TestHs
 
  '  if keycode = 205 then target1c_hit ' test functions   right arrow
  '  if keycode = 156 then target2c_hit ' test functions   left arrow
  '  if keycode = 208 then LeftInLaneTrigger_hit ' test functions   down arrow
  '  if keycode = 200 then RightInLaneTrigger_hit ' test functions   up arrow
  '  if keycode = 204 then trigger5_hit' test functions   enter keypad
 
    if keycode = 2 then
        If EnableInitialEntry = False Then
' Do not add players if Credit = 0
        if Credit > 0 then
          AttractMode.Enabled = FALSE
          AttractModeSounds.Enabled=FALSE
          StopSound MusicFile
          MyLightSeq.StopPlay()
' Do Reset Sequence if Players = 0
           If GameOn = FALSE and Players = 0 then
              StartGame
              Exit Sub
           End If  
' Add Player if Game not started yet
             If Players < 1 and Score(1) = 0 then
                Players = Players + 1
                Credit = Credit - 1
                If Credit < 1 Then DOF 121, 0
                CreditReel.SetValue Credit
                If B2SOn Then
                    Controller.B2SSetCredits Credit
                End If
                savehs
                End If
           End If  
        end if
        End If
End Sub
 
'Sub flipperLtimer_timer()
'   PlaySound "buzz", 0, .05, -0.2, 0.05,0,0,0,.7
'End sub
'
'Sub flipperRtimer_timer()
'   PlaySound "buzz1", 0, .15, 0.2, 0.05,0,0,0,.7
'End sub
 
Sub BallInLane_timer()
   PlayReq(78)
   BallInLane.Interval=10000+INT(5000*RND)
End Sub
 
Sub Table1_KeyUp(ByVal keycode)
 
    If keycode = PlungerKey Then
        Plunger.Fire
    PlaySoundAt "Plunger",Plunger
    End If
   
    If keycode = LeftFlipperKey Then
      If GameOn = TRUE then
        LeftFlipper.RotateToStart
        PlaySoundAtVol SoundFX("FlipDownL", DOFFlippers),LeftFlipper,.3
        StopSound "FlipUpL"
        DOF 101, 0
        flipperLtimer.Enabled = FALSE
      End If
    End If
   
    If keycode = RightFlipperKey Then
      If GameOn = TRUE then
        RightFlipper.RotateToStart
        PlaySoundAtVol SoundFX("FlipDownR", DOFFlippers),RightFlipper,.3
        StopSound "FlipUpR"
        DOF 102, 0
        flipperRtimer.Enabled = FALSE
      End If
    End If
End Sub
 
Sub TriggerLightsOff
  For each i in BumperLights: i.State = LightStateOff: Next
  For each i in TableLights: i.State = LightStateOff: Next
End Sub
 
'GI Lights'
Sub GI_On
  For each i in GI: i.State = LightStateOn: Next
End Sub
 
Sub GI_Off
  For each i in GI: i.State = LightStateOff: Next
End Sub
 
Sub Bumper1_On
  b1=1:b1l1.state = LightStateOn
  b1l2.state = LightStateOn
  b1l3.state = LightStateOn
End Sub
Sub Bumper1_Off
  b1=0:b1l1.state = LightStateOff
  b1l2.state = LightStateOff
  b1l3.state = LightStateOff
End Sub
Sub Bumper2_On
  b2=1:b2l1.state = LightStateOn
  b2l2.state = LightStateOn
  b2l3.state = LightStateOn
End Sub
Sub Bumper2_Off
  b2=0:b2l1.state = LightStateOff
  b2l2.state = LightStateOff
  b2l3.state = LightStateOff
End Sub
Sub Bumper3_On
  b3=1:b3l1.state = LightStateOn
  b3l2.state = LightStateOn
  b3l3.state = LightStateOn
End Sub
Sub Bumper3_Off
  b3=0:b3l1.state = LightStateOff
  b3l2.state = LightStateOff
  b3l3.state = LightStateOff
End Sub
Sub Bumper4_On
  b4=1:b4l1.state = LightStateOn
  b4l2.state = LightStateOn
  b4l3.state = LightStateOn
End Sub
Sub Bumper4_Off
  b4=0:b4l1.state = LightStateOff
  b4l2.state = LightStateOff
  b4l3.state = LightStateOff
End Sub
Sub Bumper5_On
  b5=1:b5l1.state = LightStateOn
  b5l2.state = LightStateOn
  b5l3.state = LightStateOn
End Sub
Sub Bumper5_Off
  b5=0:b5l1.state = LightStateOff
  b5l2.state = LightStateOff
  b5l3.state = LightStateOff
End Sub
 
' Tilt Routine
Sub TiltCheck
   If Tilted = FALSE then
      If Tilts = 0 then
         Tilts = Tilts + int(rnd*100)
         TiltTimer.Enabled = TRUE
      Else  
         Tilts = Tilts + int(rnd*100)
      End If  
      If Tilts >= 225 and Tilted = FALSE then
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper1.Force=0
        Bumper2.Force=0
        Bumper3.Force=0
        Bumper4.Force=0
        Bumper5.Force=0
 
        SkillShotOff()
        If LampSplash.enabled=True then ' if you tilt while in plunger lane
          l1.state=s1:l2.state=s2:l3.state=s3:l4.state=s4
          LampSplash.enabled = False  'Turn off the 4 light attract mode
          BallInLane.Enabled=False
        End If
        Tilted = TRUE
        TiltTimer.Enabled = FALSE
        TiltBox.Text = "TILT"
        TiltBoxa.Text = "TILT"
        PlayReq(100)
        If B2SOn Then
            Controller.B2SSetTilt 33,1
            Controller.B2SSetdata 1,0
        End If
      Else
        if Tilts > 80 then   'flash gi
          TiltGI.interval=1000
          TiltGI.enabled=TRUE
          PlayReq(97)
          GI_Off()
        End if
      End If  
   End If
End Sub
 
Sub TiltGI_timer()
  TiltGI.enabled=FALSE
  GI_On()
End Sub
 
Sub TiltTimer_Timer()
    If Tilts > 0 then
       Tilts = Tilts - 1
    Else
       TiltTimer.Enabled = FALSE
    End If
End Sub
 
' Game Control
Sub StartGame
    PlayReq(62) '  Start of Game
    Players = 1
    Up = 1
    UpLight(1).State = LightStateOn
    Credit = Credit - 1
    If Credit < 1 Then DOF 121, 0
    CreditReel.SetValue Credit
    If B2SOn Then
        Controller.B2SSetCredits Credit
    End If
    savehs
 
    Bumper1.Force=5  ' reset Bumpers after a tilt
    Bumper2.Force=5
    Bumper3.Force=5
    Bumper4.Force=5
    Bumper5.Force=0
 
    LFlag=0:RFlag=0
 
    GI_On
    Tilted = FALSE
    Tilts = 0
    'TiltBox.Text = ""
    'TiltBoxa.Text = ""
    If B2SOn Then
        Controller.B2SSetTilt 33,0
        Controller.B2SSetdata 1,1
    End If
    GameOn = TRUE
    BallReel.SetValue 1
    If B2SOn Then
        Controller.B2SSetBallInPlay 32, Ball
    End If
    For x = 1 to 2
      Score(x) = 0
      Credit1(x) = FALSE
      Credit2(x) = FALSE
      If B2SOn Then
        Controller.B2SSetGameOver 35,0
      End If
    NEXT
    EMReel1.ResettoZero
    'EMReel2.ResettoZero
    PlaySound "initialize"
    ResetTimer.Enabled = TRUE
    If B2SOn Then
        Controller.B2SSetScore 1,score(1)
    End If
End Sub
 
Sub ResetTimer_Timer()
    ResetTimer.Enabled = FALSE
    Ball = 1
    Bumper1_Off:Bumper2_Off:Bumper3_Off:Bumper4_Off:Bumper5_On
 
      ' turn on Melon lights
      L5.State = LightStateOn   'Yellow Light
 
    L11.State=LightStateOn:L12.State=LightStateOn:L13.State=LightStateOn:L14.State=LightStateOn 'Upper Lane Lights
    If B2SOn Then
        Controller.B2SSetBallInPlay 32, Ball
    End If
 
    PlayReq(36) ' SOB
    BallReleaseTimer.Enabled = TRUE
End Sub  
 
Sub BallReleaseTimer_Timer()
  BallReleaseTimer.Enabled = FALSE
 
  MusicFile=PlayTrack(26) ' Background Music
  BallRelease.createball
  PlaySoundAt SoundFX("ballrelease", DOFContactors),Ballrelease
  DOF 119, 2
  BallRelease.kick 135,4,0
End Sub
 
Sub Drain_Hit()
    Drain.DestroyBall
    DOF 133, 2
    TriggerLightsOff()
    MyLightSeq.StopPlay()  ' reset if Tilted lights
    Tilts = 0
    PlayerDelayTimer.Enabled = TRUE
    PlaySoundAt "drain", Drain
End Sub
 
Sub PlayerDelayTimer_Timer()
    Tilted = FALSE
    PlayerDelayTimer.Enabled = FALSE
 
    BonusLights = False
    BumperTimersOff()
    Bumper5_On()
    Bumper1.Force=8  ' reset Bumpers after a tilt
    Bumper2.Force=8
    Bumper3.Force=8
    Bumper4.Force=8
    Bumper5.Force=5
    StopSound MusicFile
 
    Tilts = 0
    If B2SOn Then
        Controller.B2SSetTilt 33,0
        Controller.B2SSetdata 1,1
    End If
 
    UpLight(Up).State = LightStateOff
    Up = Up + 1
    IF Up> Players then
        Ball = Ball + 1
        If B2SOn Then
            Controller.B2SSetBallInPlay 32, Ball
        End If
        Up = 1
    End If  
    If Ball > BallsPerGame then
       GameOn = FALSE
       EndOfGame()
    Else
       UpLight(Up).State = LightStateOn
       BallReel.SetValue Ball
       Bumper1_Off:Bumper2_Off:Bumper3_Off:Bumper4_Off:Bumper5_On
       L11.State=LightStateOn:L12.State=LightStateOn:L13.State=LightStateOn:L14.State=LightStateOn 'Upper Lane Lights
       LFlag=0:RFlag=0   ' speech you hear on the inlanes
         ' turn on Melon lights
         L5.State = LightStateOn   'Yellow Light
       if ball=2 then PlayReq(38) ' SOB
       if ball=3 or ball=4 then PlayReq(39)
       if ball=5 then PlayReq(40)
       BallReleaseTimer.Enabled = TRUE
       If B2SOn Then
         Controller.B2SSetBallInPlay 32, Ball
       End If
    End If
End Sub
 
Sub EndofGame()
    LeftFlipper.RotateToStart
    RightFlipper.RotateToStart
    PlayReq(90) ' boing
    If B2SOn Then
        Controller.B2SSetBallInPlay 32, 0
    End If
    RoundHS = Score(1)
    If RoundHS > HiSc Then
        HiSc = RoundHS
        HSi = 1
        HSx = 1
        y = 1
        Initial(1) = 1
        For x = 2 to 3
            Initial(x) = 0
        Next
        UpdatePostIt
        InitialTimer1.enabled = 1
        EnableInitialEntry = True
    End If
    UpdatePostIt
    SaveHS
    Players = 0
    Bumper1_Off:Bumper2_Off:Bumper3_Off:Bumper4_Off:Bumper5_Off
    GI_Off
    If B2SOn Then
        Controller.B2SSetGameOver 35,1
    End If
    EndOfGameTimer.Interval=4000
    EndOfGameTimer.Enabled=True
End Sub
 
Sub EndOfGameTimer_Timer()
    EndOfGameTimer.Enabled=False
    PlayReq(91) ' End of Game Talk
    AttractMode.Interval=2500
    AttractMode.Enabled = True
    attractmodeflag=0
    AttractModeSounds.Interval= 15000
    AttractModeSounds.Enabled = True
End Sub
 
'flipper primitives'
 
Sub UpdateFlipperLogos
    leftflipprim.RotAndTra8 = LeftFlipper.CurrentAngle + 235
    rightflipprim.RotAndTra8 = RightFlipper.CurrentAngle + 126
    FlipperLSh.RotZ = LeftFlipper.currentangle
    FlipperRSh.RotZ = RightFlipper.currentangle
End Sub
 
Sub FlippersTimer_Timer()
    UpdateFlipperLogos
End Sub
 
' Game Logic
Sub TestHS
            HSi = 1
            HSx = 1
            y = 1
            Initial(1) = 1
            For x = 2 to 3
                Initial(x) = 0
            Next
            UpdatePostIt
            InitialTimer1.enabled = 1
            EnableInitialEntry = True
End Sub
' Playfield Star Rollovers
 
Sub Trigger1_Hit()
  PlaysoundAt "trigger",Trigger1
  DOF 127, 2
  If L1.State = LightStateOn and  L3.State = LightStateOn then
    AddScore (10)
  End if
  If L1.State = LightStateOn and L3.State = LightStateOn and L2.State = LightStateOn and L4.State = LightStateOn then
    AddScore (100)
  End if
  If L1.State = LightStateOn and  L3.State <> LightStateOn then
    AddScore(10)
  End if
  If L1.State = LightStateOff then
    L1.State=LightStateOn 'Quick Flash
    L1.uservalue=0
    L1.timerinterval=50
    Me.TimerEnabled = TRUE
    AddScore(1)
  Else
    L1.State=LightStateOff 'Quick Flash
    L1.uservalue=1
    Me.timerinterval=50
    Me.TimerEnabled = TRUE
  End if
End Sub
 
Sub Trigger1_Timer()
  Me.TimerEnabled = False
  if L1.uservalue=0 then
    L1.State=LightStateOff
  else
    if L1.uservalue=1 then
      L1.State=LightStateOn
    else
      L1.State=LightStateBlinking
    end if
  end if
End Sub
 
Sub Trigger2_Timer()
  Me.TimerEnabled = False
  if L2.uservalue=0 then
    L2.State=LightStateOff
  else
    if L2.uservalue=1 then
      L2.State=LightStateOn
    else
      L2.State=LightStateBlinking
    end if
  end if
End Sub
 
Sub Trigger3_Timer()
  Me.TimerEnabled = False
  if L3.uservalue=0 then
    L3.State=LightStateOff
  else
    if L3.uservalue=1 then
      L3.State=LightStateOn
    else
      L3.State=LightStateBlinking
    end if
  end if
End Sub
 
Sub Trigger4_Timer()
  Me.TimerEnabled = False
  if L4.uservalue=0 then
    L4.State=LightStateOff
  else
    if L4.uservalue=1 then
      L4.State=LightStateOn
    else
      L4.State=LightStateBlinking
    end if
  end if
End Sub
 
Sub Trigger6_Timer()
  Me.TimerEnabled = False
  if L9.uservalue=0 then
    L9.State=LightStateOff
  else
    if L9.uservalue=1 then
      L9.State=LightStateOn
    else
      L9.State=LightStateBlinking
    end if
  end if
End Sub
 
Sub Trigger14_Timer()
  Me.TimerEnabled = False
  if L15.uservalue=0 then
    L15.State=LightStateOff
  else
    if L15.uservalue=1 then
      L15.State=LightStateOn
    else
      L15.State=LightStateBlinking
    end if
  end if
End Sub
 
Sub Trigger2_Hit()
  PlaysoundAt "trigger",Trigger2
  DOF 128, 2
  If L2.State = LightStateOn and  L4.State = LightStateOn then
    AddScore (10)
  End if
  If L2.State = LightStateOn and L4.State = LightStateOn and L1.State = LightStateOn and L3.State = LightStateOn then
    AddScore (100)
  End if
  If L2.State = LightStateOn and L4.State <> LightStateOn then
    AddScore(10)
  End if
  If L2.State = LightStateOff then
    L2.State=LightStateOn 'Quick Flash
    L2.uservalue=0
    Me.timerinterval=50
    Me.TimerEnabled = TRUE
    AddScore(1)
  Else
    L2.State=LightStateOff 'Quick Flash
    L2.uservalue=1
    Me.timerinterval=50
    Me.TimerEnabled = TRUE
  End if
End Sub
 
Sub Trigger3_Hit()
  PlaysoundAt "trigger",Trigger3
  DOF 129, 2
  If L3.State = LightStateOn and  L1.State = LightStateOn then
    AddScore (10)
  End if
  If L1.State = LightStateOn and L3.State = LightStateOn and L2.State = LightStateOn and L4.State = LightStateOn then
    AddScore (100)
  End if
  If L4.State = LightStateOn and L1.State <> LightStateOn then
    AddScore(10)
  End if
  If L3.State = LightStateOff then
    L3.State=LightStateOn 'Quick Flash
    L3.uservalue=0
    Me.timerinterval=50
    Me.TimerEnabled = TRUE
    AddScore(1)
  Else
    L3.State=LightStateOff 'Quick Flash
    L3.uservalue=1
    Me.timerinterval=50
    Me.TimerEnabled = TRUE
  End if
End Sub
 
Sub Trigger4_Hit()
  PlaysoundAt "trigger",Trigger4
  DOF 130, 2
  If L4.State = LightStateOn and  L2.State = LightStateOn then
    AddScore (10)
  End if
  If L1.State = LightStateOn and L2.State = LightStateOn and L3.State = LightStateOn and L4.State = LightStateOn then
    AddScore (100)
  End if
  If L4.State = LightStateOn and L2.State <> LightStateOn then
    AddScore(10)
  End if
  If L4.State = LightStateOff then
    L4.State=LightStateOn 'Quick Flash
    L4.uservalue=0
    Me.timerinterval=50
    Me.TimerEnabled = TRUE
    AddScore(1)
  Else
    L4.State=LightStateOff 'Quick Flash
    L4.uservalue=1
    Me.timerinterval=50
    Me.TimerEnabled = TRUE
  End if
End Sub
 
Sub Trigger5_Hit()
  PlaysoundAt "trigger",Trigger5
  PlayReq(154) ' splat
  DOF 126, 2
  If L8.State = LightStateOn then
     L8.State=LightStateOff 'Quick Flash
     L8.uservalue=1
     Trigger5.timerinterval=50
     Trigger5.TimerEnabled = TRUE
     AddScore(10)
  End if
  If L8.State = LightStateOff then
    L8.State=LightStateOn 'Quick Flash
    L8.uservalue=0
    Trigger5.timerinterval=50
    Trigger5.TimerEnabled = TRUE
    AddScore(1)
  End if
End Sub
 
Sub Trigger5_Timer()
  Trigger5.TimerEnabled = False
  if L8.uservalue=0 then
    L8.State=LightStateOff
  else
    if L8.uservalue=1 then
      L8.State=LightStateOn
    else
      L8.State=LightStateBlinking
    end if
  end if
End Sub
 
Sub Trigger6_Hit()
  If L9.State = LightStateOff then
    AddScore(1)
  End If
  If L9.State = LightStateOn then
    L9.State=LightStateOff 'Quick Flash
    L9.uservalue=1
    Me.timerinterval=50
    Me.TimerEnabled = TRUE
    AddScore(10)
  End if
  PlayReq(135)
End Sub
 
Sub Trigger7_Hit()
  PlaysoundAt "trigger",Trigger7
  PlayReq(154) ' splat
  DOF 131, 2
  If L10.State = LightStateOff then
    L10.State=LightStateOn 'Quick Flash
    L10.uservalue=0
    L10.timerinterval=50
    Me.TimerEnabled = TRUE
    AddScore(1)
    Exit Sub
  End if
 
  If L10.State = LightStateOn then
    L10.State=LightStateOff 'Quick Flash
    L10.uservalue=1  
    Me.timerinterval=50
    Me.TimerEnabled = TRUE
    AddScore(10)
  End if
End Sub
 
Sub Trigger7_Timer()
  Me.TimerEnabled = False
  if L10.uservalue=0 then
    L10.State=LightStateOff
  else
    if L10.uservalue=1 then
      L10.State=LightStateOn
    else
      L10.State=LightStateBlinking
    end if
  end if
End Sub
 
Sub Trigger8_Hit() ' Center Exit lane
If Tilted = FALSE then
    PlaySoundAt "rollover", Trigger8
    AddScore(10)
End if
End Sub
 
dim b1,b2,b3,b4,b5
 
Sub Bumper1_Hit()
  PlaySoundAtVol SoundFX("fx_bumper1", DOFContactors),Bumper1,4
  DOF 105, 2
  if b1=0 then
    AddScore 1
    Bumper1_On:b1=0 ' flash
  else
    AddScore 10
    Bumper1_Off:b1=1 ' flash
  end if
  Me.TimerInterval=200:Me.TimerEnabled=1
End Sub
 
Sub Bumper2_Hit()
  PlaySoundAtVol SoundFX("fx_bumper2", DOFContactors),Bumper2,4
  DOF 107, 2
  if b2=0 then
    AddScore 1
    Bumper2_On:b2=0 ' flash
  else
    AddScore 10
    Bumper2_Off:b2=1 ' flash
  end if
  Me.TimerInterval=200:Me.TimerEnabled=1
End Sub
 
Sub Bumper3_Hit()
  PlaySoundAtVol SoundFX("fx_bumper3", DOFContactors),Bumper3,4
  DOF 106, 2
  if b3=0 then
    AddScore 1
    Bumper3_On:b3=0 ' flash
  else
    AddScore 10
    Bumper3_Off:b3=1 ' flash
  end if
  Me.TimerInterval=200:Me.TimerEnabled=1
End Sub
Sub Bumper4_Hit()
  PlaySoundAtVol SoundFX("fx_bumper4", DOFContactors),Bumper4,4
  DOF 108, 2
  if b4=0 then
    AddScore 1
    Bumper4_On:b4=0 ' flash
  else
    AddScore 10
    Bumper4_Off:b4=1 ' flash
  end if
  Me.TimerInterval=200:Me.TimerEnabled=1
End Sub
 
Sub Bumper5_Hit()
  PlaySoundAtVol SoundFX("fx_bumper1", DOFContactors),Bumper5,4
  DOF 109, 2
  PlayReq(166)
  if b5=0 then
    AddScore 1
    Bumper5_On:b5=0 ' flash
  else
    AddScore 5
    Bumper5_Off:b5=1 ' flash
  end if
  Me.TimerInterval=200:Me.TimerEnabled=1
 
  RotateBonusLight()
End Sub
 
Sub Bumper1_Timer()
  Me.TimerEnabled=FALSE
  if b1=0  then ' restore light
     Bumper1_Off
  else
    Bumper1_On
  end if
End Sub
Sub Bumper2_Timer()
  Me.TimerEnabled=FALSE
  if b2=0  then
    Bumper2_Off
  else
    Bumper2_On
  end if
End Sub
Sub Bumper3_Timer()
  Me.TimerEnabled=FALSE
  if b3=0  then
    Bumper3_Off
  else
    Bumper3_On
  end if
End Sub
Sub Bumper4_Timer()
  Me.TimerEnabled=FALSE
  if b4=0  then
    Bumper4_Off
  else
    Bumper4_On
  end if
End Sub
Sub Bumper5_Timer()
  Me.TimerEnabled=FALSE
  if b5=0  then
    Bumper5_Off
  else
    Bumper5_On
  end if
End Sub
 
Sub t1_hit()
    t1p.transx = -10
    Me.TimerEnabled = 1
    addscore 50
End sub
Sub t1_Timer
    t1p.transx = 0
    Me.TimerEnabled = 0
End Sub
 
'Girls
 
Sub target1L_hit()
  target1_hit(5)
End Sub
Sub targer1R_hit()
  target1_hit(5)
End Sub
Sub target1C_hit()
  target1_hit(50)
End Sub
 
Sub target1_hit(pts)
    target1p.transx = -10
    target1.TimerEnabled = 1
    PlaysoundAt SoundFX("trigger", DOFTargets),Light12
    DOF 124, 2
    ' add some points
    If L6.State = LightStateOff then
      AddScore(pts)
      if pts = 50 then
        L6.State=LightStateBlinking:L6.uservalue=0
        L6.timerinterval=1000:L6.TimerEnabled=True
        PlayReq(143)
      end if      
    end if
    If L6.State = LightStateOn then
      AddScore(200)
      L6.State=LightStateBlinking:L6.uservalue=0 ' Turn Off
      L6.timerinterval=1000:L6.TimerEnabled=True
      PlayReq(117)
      L5.State=LightStateOn   ' Next Light
    End if
End sub
 
Sub L6_timer()
  L6.TimerEnabled=False
  if L6.uservalue = 0 then L6.State=LightStateOff
  if L6.uservalue = 1 then L6.State=LightStateOn
End Sub
 
Sub target1_Timer
    target1p.transx = 0
    Me.TimerEnabled = 0
End Sub
 
'Ellen
Sub target2L_hit()
  target2_hit(5)
End Sub
Sub targer2R_hit()
  target2_hit(5)
End Sub
Sub target2C_hit()
  target2_hit(50)
End Sub
 
Sub target2_hit(pts)
    target2p.transx = -10
    target2.TimerEnabled = 1
    PlaysoundAt SoundFX("trigger", DOFTargets),Light11
    DOF 125, 2
    ' add some points
    If L5.State = LightStateOff then
      AddScore(pts)
      if pts = 50 then
        L5.State=LightStateBlinking:L5.uservalue=0
        L5.timerinterval=500:L5.TimerEnabled=True
        PlayReq(143)
      end if      
    end if
    If L5.State = LightStateOn then
      AddScore(200)
      L5.State=LightStateBlinking:L5.uservalue=0
      L5.timerinterval=500:L5.TimerEnabled=True
      PlayReq(117)
      L7.State=LightStateOn
    End if
End sub
 
Sub target2_Timer
    target2p.transx = 0
    Me.TimerEnabled = 0
End Sub
 
Sub L5_timer()
  L5.TimerEnabled=False
  if L5.uservalue = 0 then L5.State=LightStateOff
  if L5.uservalue = 1 then L5.State=LightStateOn
End Sub
 
'Meloney
Sub Kicker1_Hit()
    PlaysoundAt "kicker_enter", Kicker1
  If L7.State = LightStateoff then 'light below cup
    L7.State=LightStateBlinking:L7.uservalue=0
    L7.timerinterval=500:L7.TimerEnabled=True
    AddScore(50)
  End if
  If L7.State = LightStateOn then  
    L7.State=LightStateBlinking:L7.uservalue=0
    L7.timerinterval=500:L7.TimerEnabled=True
    AddScore(200)
    L6.State = LightStateOn ' Turn Next Light On
  End if
  pauseloop=0
  PauseTimer.Interval=250
  PauseTimer.Enabled=TRUE
End Sub
 
Sub PauseTimer_Timer()
  pauseloop=pauseloop+1
  if pauseloop=10 then
    PlayReq(195)
  end if
  if pauseloop=1 and (b1+b2+b3+b4)=4 then PlayReq(127)
  if pauseloop=1 and (b1+b2+b3+b4)=3 then PlayReq(126)
 
  if pauseloop=2 then
    if b1=1 then  ' flash lit bumper and award points
      Bumper1_Off:b1=1 ' flash - need to restore the value of b1 here
      Bumper1.PlayHit():DOF 105, 2:PlaySoundAt SoundFX("fx_bumper1", DOFContactors),Bumper1
      Bumper1.TimerInterval=500:Bumper1.TimerEnabled=1
      AddScore(10)
    else
      pauseloop=pauseloop+1 ' skip forward
    end if
  end if
  if pauseloop=4 then
    if b2=1  then
      Bumper2_Off:b2=1 ' flash
      Bumper2.PlayHit():DOF 107, 2:PlaySoundAt SoundFX("fx_bumper2", DOFContactors),Bumper2
      Bumper2.TimerInterval=500:Bumper2.TimerEnabled=1
      AddScore(10)
    else
      pauseloop=pauseloop+1
    end if
  end if
  if pauseloop=6 then
    if b3=1 then
      Bumper3_Off:b3=1 ' flash
      Bumper3.PlayHit():DOF 106, 2:PlaySoundAt SoundFX("fx_bumper3", DOFContactors),Bumper3
      Bumper3.TimerInterval=500:Bumper3.TimerEnabled=1
      AddScore(10)
    else
      pauseloop=pauseloop+1
    end if
  end if
  if pauseloop=8 then
    if b4=1 then
      Bumper4_Off:b4=1 ' flash
      Bumper4.PlayHit():DOF 108, 2:PlaySoundAt SoundFX("fx_bumper4", DOFContactors),Bumper4
      Bumper4.TimerInterval=500:Bumper4.TimerEnabled=1
      AddScore(10)
    else
      pauseloop=pauseloop+1
    end if
  end if
  if pauseloop > 12 then
    PauseTimer.Enabled=FALSE
    Kicker1.TimerInterval = 500
    Kicker1.TimerEnabled = TRUE
  End if
End Sub
 
Sub Kicker1_Timer()
    PlaySoundAt "kicker_release", Kicker1
	kicker1.TimerEnabled = FALSE
	Kicker1.kick 260,15, .1
	kicker1Rod.transy=15	
	kickrodtimer.uservalue=1
	kickrodtimer.enabled=1
	DOF 123, 2
End Sub

Sub kickrodtimer_timer
	if me.uservalue=2 then kicker1rod.transy=7.5
	if me.uservalue=3 then
		kicker1rod.transy=0
		me.enabled=0
	end if
	me.uservalue=me.uservalue+1
end sub
 
Sub L7_timer()
  L7.TimerEnabled=False
  if L7.uservalue = 0 then L7.State=LightStateOff
  if L7.uservalue = 1 then L7.State=LightStateOn
End Sub
 
Sub RotateBonusLight
  if L5.state = LightStateOn then
    ' Move to L7
      L5.state = LightStateOff
      L7.state=LightStateOn
  else
    if L7.state = LightStateOn then
      ' move to L6
        L7.state = LightStateOff
        L6.state=LightStateOn
    else
      if L6.state = LightStateOn then
        ' move to L5
          L6.state = LightStateOff
          L5.state=LightStateOn
      end if
    end if
  end if
End Sub
 
Sub LeftInLaneTrigger_Hit()
 If Tilted = FALSE then
    PlaySoundAt "rollover", LeftInLaneTrigger
    AddScore(5)
    DOF 116, 2
    ' add some points
    LFlag=LFlag+1
    if LFlag>10 then LFlag=1
 
    if LFlag = 1 then
      if RFlag=0 or RFlag=3 then
        RFlag=1  ' skip over the intro speech
      end if
      PlayReq(197)
    end if
    if LFlag = 2 then PlayReq(198)
    if LFlag = 3 then PlayReq(199)
    if LFlag > 3 then PlayReq(117)
 
    RotateBonusLight()
End if
End Sub
 
Sub RightInLaneTrigger_Hit()
 If Tilted = FALSE then
    PlaySoundAt "rollover", RightInLaneTrigger
    AddScore(5)
    DOF 117, 2
    ' add some points
    RFlag=RFlag+1
    if RFlag>10 then RFlag=1
 
    if RFlag = 1 then
      if LFlag=0 or LFlag=3 then
        LFlag=1  ' skip over the intro speech
      end if
      PlayReq(197)
    end if
    if RFlag = 2 then PlayReq(200)
    if RFlag = 3 then PlayReq(199)
    if RFlag > 3 then PlayReq(117)
 
    RotateBonusLight()
End If
End Sub
 
Sub LeftOutLaneTrigger_Hit()
 If Tilted = FALSE then
    PlaySoundAt "rollover", LeftOutLaneTrigger
    DOF 115, 2
    ' add some points
    AddScore(50)
    PlayReq(117)
End If
End Sub
 
Sub RightOutLaneTrigger_Hit()
 If Tilted = FALSE then
    PlaySoundAt "rollover", RightOutLaneTrigger
    DOF 118, 2
    ' add some points
    AddScore(50)
    PlayReq(117)
End If
End Sub
 
Sub RightSlingShot_Slingshot()
    PlaySoundAt SoundFX("sling", DOFContactors),SLING9
    DOF 104, 2
    Addscore(1)
End Sub
Sub RSlingshot1R_hit()
    PlaySoundAt "sling",Light36
    Addscore(1)
End Sub
Sub RSlingshot2R_hit()
    PlaySoundAt "sling",L15
    Addscore(1)
End Sub
Sub RSlingshot3R_hit()
    PlaySoundAt "sling",L15
    Addscore(1)
End Sub
Sub LeftSlingShot_Slingshot()
    PlaySoundAt SoundFX("sling", DOFContactors),SLING8
    DOF 103, 2
    Addscore(1)
End Sub
Sub LSlingshot1R_hit()
    PlaySoundAt "sling",light1
    Addscore(1)
End Sub
Sub LSlingshot2R_hit()
    PlaySoundAt "sling",L10
    Addscore(1)
End Sub
Sub LSlingshot3R_hit()
    PlaySoundAt "sling",gi55
    Addscore(1)
End Sub
Sub LSlingshot4R_hit()
    PlaySoundAt "sling",gi55
    Addscore(1)
End Sub
Sub LSlingshot5R_hit()
    PlaySoundAt "sling",gi55
    Addscore(1)
End Sub
 
Sub t9_Hit()  'lane 1
 If Tilted = FALSE then
    bumper1_on
    PlaysoundAt "rollover",T9
    PlayReq(120) ' top lanes
    DOF 110, 2
    If StandardMode = True then
        If L14.State = LightStateOn then
            AddScore(10)
            L14.State = LightStateBlinking
            Exit Sub
        End if
   
        If L14.State = LightStateBlinking then  ' Upper Left Light
            AddScore(10)
            L14.State = LightStateOff
            L1.State = LightStateOn   ' Bottom Rollover Light
            L8.State = LightStateOn   ' Right Red Rollover
        End if
    Else
        If L14.State = LightStateOn then ' Upper Left Light
            AddScore(10)
            L14.State = LightStateOff
            L1.State = LightStateOn   ' Bottom Rollover Light
            L8.State = LightStateOn   ' Right Red Rollover
        Else
           AddScore(1) ' Get 10pts when light is off
        End if
    End if
    If L14.State = LightStateOff and L13.State = LightStateOff and L12.State = LightStateOff and L11.State=LightStateOff  then ' all top lights
        Addscore(250)
        PlayReq(121)
    End if
End if
End Sub
 
Sub t10_Hit() ' lane2  - should light L15
 If Tilted = FALSE then
    bumper2_on
    PlaysoundAt "rollover",T10
    PlayReq(120) ' top lanes
    DOF 111, 2
    If StandardMode = True then
        If L13.State = LightStateOn then
            AddScore(10)
            L13.State = LightStateBlinking
            Exit Sub
        End if
   
        If L13.State = LightStateBlinking then  ' Upper Left Light
            AddScore(10)
            L13.State = LightStateOff
            L2.State = LightStateOn   ' Bottom Rollover Light
            L15.State = LightStateOn  ' Green Rolloever
        End if
    Else
        If L13.State = LightStateOn then ' Upper Left Light
            AddScore(10)
            L13.State = LightStateOff
            L2.State = LightStateOn   ' Bottom Rollover Light
            L15.State = LightStateOn  ' Green Rolloever
        Else
           AddScore(1) ' Get 10pts when light is off
        End if
    End if
 
 
    If L14.State = LightStateOff and L13.State = LightStateOff and L12.State = LightStateOff and L11.State=LightStateOff  then ' all top lights
        Addscore(250)
        PlayReq(121)
    End if
End if
End Sub
 
Sub t11_Hit()
 If Tilted = FALSE then
    bumper4_on
    PlaysoundAt "rollover",T11
    PlayReq(120) ' top lanes
    DOF 112, 2
    If StandardMode = True then
        If L12.State = LightStateOn then
            AddScore(10)
            L12.State = LightStateBlinking
            Exit Sub
        End if
   
        If L12.State = LightStateBlinking then  '
            AddScore(10)
            L12.State = LightStateOff
            L3.State = LightStateOn   '
            L9.State = LightStateOn  
        End if
    Else
        If L12.State = LightStateOn then '
            AddScore(10)
            L12.State = LightStateOff
            L3.State = LightStateOn   ' Mid Playfield rollovers
            L9.State = LightStateOn   ' 2nd rollover
        Else
           AddScore(1) ' Get 10pts when light is off
        End if
    End if
    If L14.State = LightStateOff and L13.State = LightStateOff and L12.State = LightStateOff and L11.State=LightStateOff  then ' all top lights
        Addscore(250)
        PlayReq(121)
    End if
End If
End Sub
 
Sub T12_Hit()  ' should light  L10
 If Tilted = FALSE then
    bumper3_on
    PlaysoundAt "rollover",T12
    PlayReq(120) ' top lanes
    DOF 113, 2
    If StandardMode = True then
        If L11.State = LightStateOn then
            AddScore(10)
            L11.State = LightStateBlinking
            Exit Sub
        End if
   
        If L11.State = LightStateBlinking then  ' Upper Left Light
            AddScore(10)
            L11.State = LightStateOff
            L4.State = LightStateOn   ' Bottom Rollover Light
            L10.State = LightStateOn  ' 2nd rollover
        End if
    Else
        If L11.State = LightStateOn then ' Upper Left Light
            AddScore(10)
            L11.State = LightStateOff
            L4.State = LightStateOn   ' Bottom Rollover Light
            L10.State = LightStateOn  ' 2nd rollover
        Else
           AddScore(1) ' Get 1pts when light is off
        End if
    End if
 
    If L14.State = LightStateOff and L13.State = LightStateOff and L12.State = LightStateOff and L11.State=LightStateOff  then ' all top lights
        Addscore(250)
        PlayReq(121)
    End if
End If
End Sub
 
Sub T13_Hit()
 If Tilted = FALSE then
    PlaysoundAt "rollover",T13
    DOF 114, 2
 
    if LampSplash2.enabled then '200 pts for skill shot - gets turned off in addscore routine
      AddScore(200)
      PlayReq(147) ' top lanes
    else
      AddScore(50)
      PlayReq(143)
    End if
End If
End Sub
 
Sub Trigger14_Hit()
 If Tilted = FALSE then
    DOF 134, 2
    PlayReq(117)
   
    If L15.State = LightStateOff then
      L15.State=LightStateOn 'Quick Flash
      L15.uservalue=0
      L15.timerinterval=50
      Me.TimerEnabled = TRUE
      AddScore(1)
    Else
      If L15.State = LightStateOn then
        L15.State=LightStateOff 'Quick Flash
        L15.uservalue=1
        L15.timerinterval=50
        Me.TimerEnabled = TRUE
        AddScore(10)
      End if
    End if
End If
End Sub
 
 
Sub BumperTimersOff()
  L5.State = LightStateOff
  L6.State = LightStateOff
  L7.State = LightStateOff
End Sub
 
'====================
 
Sub Resetaftersuccess()
  BumperTimersOff()
  L5.State = LightStateOn  'reenable the melon lights
End Sub
 
'Score Handling
 
Sub AddScore(val)
  skillshotoff()
  If Tilted = FALSE then
    if int(Score(Up)/1000) <> int((Score(Up)+val)/1000) then
      PlaySound "Sounds-0x32C",0, CVol     ' Cow Bell
    else
      if int(Score(Up)/100) <> int((Score(Up)+val)/100) then
        PlaySound "Sounds-0xE9",0, CVol    ' bell sounds
      else
        if int(Score(Up)/10) <> int((Score(Up)+val)/10) then
          PlaySound "Sounds-0x1F0",0, CVol ' Simple Bell
        end if
      end if
    end if
    if Score(Up)+val > 1000 and Score(Up)<1000 then
      PlayReq(88)
    end if
    if Score(Up)+val > 1500 and Score(Up)<1500 then
      PlayReq(87)
    end if
    if Score(Up)+val > 3500 and Score(Up)<3500 then
      PlayReq(86)
    end if
    if Score(Up)+val > 4000 and Score(Up)<4000 then
      PlayReq(85)
    end if
    if Score(Up)+val > 5000 and Score(Up)<5000 then
      PlayReq(84)
    end if
    if Score(Up)+val > 6000 and Score(Up)<6000 then
      PlayReq(83)
    end if
    if Score(Up)+val > 7000 and Score(Up)<7000 then
      PlayReq(82)
    end if
    if Score(Up)+val > 8000 and Score(Up)<8000 then
      PlayReq(80)
    end if
    if Score(Up)+val >10000 and Score(Up)<10000 then
      PlayReq(81)
    end if
 
    Score(Up) = Score(Up) + val
    DisplayScore
    AddReel val
 
    If B2SOn Then
        Controller.B2SSetScore 1,Score(1)
    End If
  End If  
End Sub    
 
Sub AddReel(Pts)
   Select Case Up
   Case 1
   EmReel1.AddValue Pts
   Case 2
   EmReel2.AddValue Pts
   End Select  
End Sub
 
' Score Displayer
Sub DisplayScore()
            If Score(Up) >= 2000 And Credit1(Up)=false Then
            PlayReq(30)
            AwardSpecial
            Credit1(Up)=true
        End If
         If Score(Up) >= 3000 And Credit2(Up)=false Then
            PlayReq(31)
            AwardSpecial
            Credit2(Up)=true
        End If
End Sub
 
Sub AwardSpecial()
    PlaysoundAt SoundFX("knocke", DOFKnocker),L11
    DOF 122, 2
    Credit = Credit + 1
    DOF 121, 1
    CreditReel.SetValue Credit
    If B2SOn Then
        Controller.B2SSetCredits Credit
    End If
    savehs
End Sub
 
'************Save Scores
Sub SaveHS
    savevalue "nellie", "Credit", Credit
    savevalue "nellie", "HiScore", HiSc
    savevalue "nellie", "Match", MatchNumber
    savevalue "nellie", "Initial1", Initial(1)
    savevalue "nellie", "Initial2", Initial(2)
    savevalue "nellie", "Initial3", Initial(3)
    savevalue "nellie", "Score4", Score(1)
    savevalue "nellie", "Balls", Balls
End sub
 
'*************Load Scores
Sub LoadHighScore
    dim temp
    temp = LoadValue("nellie", "credit")
    If (temp <> "") then Credit = CDbl(temp)
    temp = LoadValue("nellie", "HiScore")
    If (temp <> "") then HiSc = CDbl(temp)
    temp = LoadValue("nellie", "match")
    If (temp <> "") then MatchNumber = CDbl(temp)
    temp = LoadValue("nellie", "Initial1")
    If (temp <> "") then Initial(1) = CDbl(temp)
    temp = LoadValue("nellie", "Initial2")
    If (temp <> "") then Initial(2) = CDbl(temp)
    temp = LoadValue("nellie", "Initial3")
    If (temp <> "") then Initial(3) = CDbl(temp)
    temp = LoadValue("nellie", "score4")
    If (temp <> "") then score(1) = CDbl(temp)
    temp = LoadValue("nellie", "balls")
    If (temp <> "") then Balls = CDbl(temp)
End Sub
 
 
Dim s26,s27,s1,s2,s3,s4,s6,s7
 
Sub LampSplashOn_Hit
  if Tilted = FALSE then
    PlaySoundAt "rollover", LampSplashOn
    s26=Light26.state:s27=Light27.state:s6=Light6.state:s7=Light7.state
    s1=l1.state:s2=l2.state:s3=l3.state:s4=l4.state
    l1.state=LightStateOff
    l2.state=LightStateOff
    l3.state=LightStateOff
    l4.state=LightStateOff
    LampSplash.interval = 300
    LampSplash2.interval = 300
    LampSplash.enabled = True
    LampSplash2.enabled = True
    BallInLane.Enabled=True
  End if
End Sub
 
Sub LampSplashOn_UnHit
  if Tilted = FALSE then
    DOF 120, 2
    l1.state=s1:l2.state=s2:l3.state=s3:l4.state=s4
    LampSplash.enabled = False  'Turn off the 4 light attract mode
    BallInLane.Enabled=False
  End If
End Sub
 
Sub SkillshotOff   'turn off when points are scored
  if LampSplash2.enabled = True then
    LampSplash2.enabled = False
    Light26.state=s26:Light27.state=s27
    Light6.state=s6:Light7.state=s7
  End if
End Sub
 
Sub LampSplash_Timer() ' These lights no longer flash once the trigger is unhit
  Select Case lno
        Case 1
            l1.state = LightStateOn:l2.state = LightStateOff
        Case 2
            l2.state = LightStateOn:l1.state = LightStateOff
        Case 3
            l3.state = LightStateOn:l2.state = LightStateOff
        Case 4
            l4.state = LightStateOn:l3.state = LightStateOff
        Case 5
            l3.state = LightStateOn:l4.state = LightStateOff
        Case 6
            l2.state = LightStateOn:l3.state = LightStateOff
    End Select  
End Sub
 
Sub LampSplash2_Timer() ' flash the lights while ball is in the trough
  lno = lno + 1
  if lno > 6 then lno = 1
  Select Case lno
        Case 1
            Light27.state = LightStateOff:Light26.state = LightStateOff:Light7.state=LightStateOff:Light6.state=LightStateOff
        Case 2
            Light27.state = LightStateOff:Light26.state = LightStateOff:Light7.state=LightStateOff:Light6.state=LightStateOff
        Case 3
            Light27.state = LightStateOff:Light26.state = LightStateOff:Light7.state=LightStateOff:Light6.state=LightStateOff
        Case 4
            Light27.state = LightStateOn:Light26.state = LightStateOn:Light7.state=LightStateOn:Light6.state=LightStateOn
        Case 5
            Light27.state = LightStateOn:Light26.state = LightStateOn:Light7.state=LightStateOn:Light6.state=LightStateOn
        Case 6
            Light27.state = LightStateOn:Light26.state = LightStateOn:Light7.state=LightStateOn:Light6.state=LightStateOn
    End Select  
End Sub
 
' Base Vp10 Routines
' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************
 
Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng((BallVel(ball)*1 + 4)/10)
End Function
 
Function Vol2(ball1, ball2) ' Calculates the Volume of the sound based on the speed of two balls
    Vol2 = (Vol(ball1) + Vol(ball2) ) / 2
End Function
 
Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp> 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function
 
Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function
 
function AudioFade(ball)
    Dim tmp
    tmp = ball.y * 2 / Table1.height-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function
 
Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function
 
'*****************************************
'    JP's VP10 Collision & Rolling Sounds
'*****************************************
 
Const tnob = 2 ' total number of balls
ReDim rolling(tnob)
ReDim collision(tnob)
Initcollision
 
Sub Initcollision
    Dim i
    For i = 0 to tnob
        collision(i) = -1
        rolling(i) = False
    Next
End Sub
 
Sub CollisionTimer_Timer()
    Dim BOT, B, B1, B2, dx, dy, dz, distance, radii
    BOT = GetBalls
 
    ' rolling
   
    For B = UBound(BOT) +1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next
    If UBound(BOT) = -1 Then Exit Sub
    For b = 0 to UBound(BOT)
    If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
                    PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
            Else   
                    PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0
            End If
        Else
            If rolling(b) = True Then
                    StopSound("fx_ballrolling" & b)
                    rolling(b) = False
            End If
        End If
    Next
 
    'collision
 
    If UBound(BOT) < 1 Then Exit Sub
 
    For B1 = 0 to UBound(BOT)
        For B2 = B1 + 1 to UBound(BOT)
            dz = INT(ABS((BOT(b1).z - BOT(b2).z) ) )
            radii = BOT(b1).radius + BOT(b2).radius
            If dz <= radii Then
 
            dx = INT(ABS((BOT(b1).x - BOT(b2).x) ) )
            dy = INT(ABS((BOT(b1).y - BOT(b2).y) ) )
            distance = INT(SQR(dx ^2 + dy ^2) )
 
            If distance <= radii AND (collision(b1) = -1 OR collision(b2) = -1) Then
                collision(b1) = b2
                collision(b2) = b1
                PlaySound("fx_collide"), 0, Vol2(BOT(b1), BOT(b2)), Pan(BOT(b1)), 0, Pitch(BOT(b1)), 0, 0, AudioFade(BOT(b1))
            Else
                If distance > (radii + 10)  Then
                    If collision(b1) = b2 Then collision(b1) = -1
                    If collision(b2) = b1 Then collision(b2) = -1
                End If
            End If
            End If
        Next
    Next
End Sub
 
Sub Posts_Hit(idx)
    If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
        RandomSoundRubber()
    Else
        PlaySound "fx_rubber2", 0, Vol(ActiveBall)*.1, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1
    End If
End Sub
 
 
Sub Rubbers_Hit(idx)
    If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
        RandomSoundRubber()
    Else
        PlaySound "fx_rubber2", 0, Vol(ActiveBall)*.1, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1
    End If
End Sub
 
Sub RandomSoundRubber()
    Select Case Int(Rnd*3)+1
        Case 1 : PlaySoundAtBallVol "rubber_hit_1",.1
        Case 2 : PlaySoundAtBallVol "rubber_hit_2",.1
        Case 3 : PlaySoundAtBallVol "rubber_hit_3",.1
    End Select
End Sub
 
Sub LeftFlipper_Collide(parm)
    RandomSoundFlipper()
End Sub
 
Sub RightFlipper_Collide(parm)
    RandomSoundFlipper()
End Sub
 
Sub RandomSoundFlipper()
    Select Case Int(Rnd*3)+1
        Case 1 : PlaySoundAtBall "flip_hit_1"
        Case 2 : PlaySoundAtBall "flip_hit_2"
        Case 3 : PlaySoundAtBall "flip_hit_3"
    End Select
End Sub
 
Sub Gates_Hit (idx)
    PlaySoundAt "gate4",ActiveBall
End Sub
 
 
Sub AttractMode_Timer()
  AttractMode.Enabled=False
  MyLightSeq.UpdateInterval=15
  MyLightSeq.Play SeqLeftOn, 30, 3
  MyLightSeq.Play SeqUpOn, 30, 3
  MyLightSeq.Play SeqDownOn, 30, 3
  MyLightSeq.Play SeqRightOn, 30, 3
  MyLightSeq.Play SeqLeftOn, 30, 3
  MyLightSeq.Play SeqUpOn, 30, 3
  MyLightSeq.Play SeqDownOn, 30, 3
  MyLightSeq.Play SeqRightOn, 30, 3
  MyLightSeq.Play SeqLeftOn, 30, 3
  MyLightSeq.Play SeqUpOn, 30, 3
  MyLightSeq.Play SeqDownOn, 30, 3
  MyLightSeq.Play SeqRightOn, 30, 3
  MyLightSeq.Play SeqLeftOn, 30, 3
  MyLightSeq.Play SeqUpOn, 30, 3
  MyLightSeq.Play SeqDownOn, 30, 3
  MyLightSeq.Play SeqRightOn, 30, 3
End Sub
 
Sub MyLightSeq_PlayDone() ' play the lights again until key is pressed
  AttractMode.Interval=100
  AttractMode.Enabled=True
End Sub
 
Sub AttractModeSounds_Timer() ' play random attract speech
  attractmodeflag=attractmodeflag+1
  if attractmodeflag = 5 then
    MusicFile=Playreq(28) ' longer piano entrance
  else
    if attractmodeflag > 15 then
      attractmodeflag=1
      MusicFile=Playreq(93) ' : msgbox "Attract Mode MusicFile=" &MusicFile
    else
      if attractmodeflag <> 6 then ' skip one round
        MusicFile=Playreq(93) ' : msgbox "Attract Mode MusicFile=" &MusicFile
      end if
    end if
  end if
  AttractModeSounds.Interval=7000+(INT(RND*5)*1000)
End Sub
 
Sub LastScoreReels
        HSScorex = LastScore
        HSScore100K=Int (HSScorex/100000)'Calculate the value for the 100,000's digit
        HSScore10K=Int ((HSScorex-(HSScore100k*100000))/10000) 'Calculate the value for the 10,000's digit
        HSScoreK=Int((HSScorex-(HSScore100k*100000)-(HSScore10K*10000))/1000) 'Calculate the value for the 1000's digit
        HSScore100=Int((HSScorex-(HSScore100k*100000)-(HSScore10K*10000)-(HSScoreK*1000))/100) 'Calculate the value for the 100's digit
        HSScore10=Int((HSScorex-(HSScore100k*100000)-(HSScore10K*10000)-(HSScoreK*1000)-(HSScore100*100))/10) 'Calculate the value for the 10's digit
        HSScore1=Int(HSScorex-(HSScore100k*100000)-(HSScore10K*10000)-(HSScoreK*1000)-(HSScore100*100)-(HSScore10*10)) 'Calculate the value for the 1's digit
 
End Sub
 
'************Enter Initials
Sub EnterIntitals(keycode)
    If KeyCode = LeftFlipperKey Then
        HSx = HSx - 1
        if HSx < 0 Then HSx = 26
        If HSi < 4 Then EVAL("Initial" & HSi).image = HSiArray(HSx)
    End If
    If keycode = RightFlipperKey Then
        HSx = HSx + 1
        If HSx > 26 Then HSx = 0
        If HSi < 4 Then EVAL("Initial"& HSi).image = HSiArray(HSx)
    End If
        If keycode = StartGameKey Then
            If HSi < 3 Then
                EVAL("Initial" & HSi).image = HSiArray(HSx)
                Initial(HSi) = HSx
                EVAL("InitialTimer" & HSi).enabled = 0
                EVAL("Initial" & HSi).visible = 1
                Initial(HSi + 1) = HSx
                EVAL("Initial" & HSi +1).image = HSiArray(HSx)
                y = 1
                EVAL("InitialTimer" & HSi + 1).enabled = 1
                HSi = HSi + 1
            Else
                Initial3.visible = 1
                InitialTimer3.enabled = 0
                Initial(3) = HSx
                InitialEntry.enabled = 1
                HSi = HSi + 1
            End If
        End If
End Sub
 
Sub InitialEntry_timer
    SaveHS
    HSi = HSi + 1
    EnableInitialEntry = False
    InitialEntry.enabled = 0
    Players = 0
End Sub
 
'************Flash Initials Timers
Sub InitialTimer1_Timer
    y = y + 1
    If y > 1 Then y = 0
    If y = 0 Then
        Initial1.visible = 1
    Else
        Initial1.visible = 0   
    End If
End Sub
 
Sub InitialTimer2_Timer
    y = y + 1
    If y > 1 Then y = 0
    If y = 0 Then
        Initial2.visible = 1
    Else
        Initial2.visible = 0   
    End If
End Sub
 
Sub InitialTimer3_Timer
    y = y + 1
    If y > 1 Then y = 0
    If y = 0 Then
        Initial3.visible = 1
    Else
        Initial3.visible = 0   
    End If
End Sub
 
'**************Update Desktop Text
Sub UpdateText
    BallReel.SetValue Ball
End Sub
 
 
'***************Post It Note Update
Sub UpdatePostIt
    ScoreMil = Int(HiSc/1000000)
    Score100K = Int( (HiSc - (ScoreMil*1000000) ) / 100000)
    Score10K = Int( (HiSc - (ScoreMil*1000000) - (Score100K*100000) ) / 10000)                                                     
    ScoreK = Int( (HiSc - (ScoreMil*1000000) - (Score100K*100000) - (Score10K*10000) ) / 1000)                                     
    Score100 = Int( (HiSc - (ScoreMil*1000000) - (Score100K*100000) - (Score10K*10000) - (ScoreK*1000) ) / 100)                    
    Score10 = Int( (HiSc - (ScoreMil*1000000) - (Score100K*100000) - (Score10K*10000) - (ScoreK*1000) - (Score100*100) ) / 10)         
    ScoreUnit = (HiSc - (ScoreMil*1000000) - (Score100K*100000) - (Score10K*10000) - (ScoreK*1000) - (Score100*100) - (Score10*10) )
 
    Pscore6.image = HSArray(ScoreMil):If HiSc < 1000000 Then PScore6.image = HSArray(10)
    Pscore5.image = HSArray(Score100K):If HiSc < 100000 Then PScore5.image = HSArray(10)
    PScore4.image = HSArray(Score10K):If HiSc < 10000 Then PScore4.image = HSArray(10)
    PScore3.image = HSArray(ScoreK):If HiSc < 1000 Then PScore3.image = HSArray(10)
    PScore2.image = HSArray(Score100):If HiSc < 100 Then PScore2.image = HSArray(10)
    PScore1.image = HSArray(Score10):If HiSc < 10 Then PScore1.image = HSArray(10)
    PScore0.image = HSArray(ScoreUnit):If HiSc < 1 Then PScore0.image = HSArray(10)
    If HiSc < 1000 Then
        PComma.image = HSArray(10)
    Else
        PComma.image = HSArray(11)
    End If
    If HiSc < 1000000 Then
        PComma1.image = HSArray(10)
    Else
        PComma1.image = HSArray(11)
    End If
    If HiSc < 1000000 Then Shift = 1:PComma.transx = -10
    If HiSc < 100000 Then Shift = 2:PComma.transx = -20
    If HiSc < 10000 Then Shift = 3:PComma.transx = -30
    For x = 0 to 6
        EVAL("Pscore" & x).transx = (-10 * Shift)
    Next
    Initial1.image = HSiArray(Initial(1))
    Initial2.image = HSiArray(Initial(2))
    Initial3.image = HSiArray(Initial(3))
End Sub
 
Sub table1_Exit
'   SaveLMEMConfig
    If B2SOn Then Controller.stop
End Sub
 
function PlayReq(num)
  rpos=INT(RND*len(Req(num))/3)
  PlaySound "Sounds-0x" & rtrim(Ucase(mid(Req(num),3*rpos+1,3))),0, SVol
  PlayReq="Sounds-0x" & Ucase(mid(Req(num),3*rpos+1,3))
End function
 
function PlayTrack(num)  ' softer and looping
  rpos=INT(RND*len(Req(num))/3)
  PlaySound "Sounds-0x" & rtrim(Ucase(mid(Req(num),3*rpos+1,3))), 5, TVol
  PlayTrack = "Sounds-0x" & rtrim(Ucase(mid(Req(num),3*rpos+1,3)))
End function
 
' Just used for testing
Sub PlayallReq(num)
  for i = 1 to len(Req(num))/3
    PlaySound "Sounds-0x" & Ucase(mid(Req(num),3*(i-1)+1,3))
    msgbox num & " playing Sounds-0x" & Ucase(mid(Req(num),3*(i-1)+1,3))
  Next
End Sub
 
 
'**************************************************************************
'                 Positional Sound Playback Functions by DJRobX
' VPX version check only required for 10.3 backwards compatibility.
' Version check and Else statement may be removed if table is > 10.4 only.
'**************************************************************************
 
'Set position as table object (Use object or light but NOT wall) and Vol to 1
 
Sub PlaySoundAt(sound, tableobj)
    If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
        PlaySound sound, 1, 1, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
    Else
        PlaySound sound, 1, 1, Pan(tableobj)
    End If
End Sub
 
 
'Set all as per ball position & speed.
 
Sub PlaySoundAtBall(sound)
    If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
        PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
    Else
        PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1
    End If
End Sub
 
 
'Set position as table object and Vol manually.
 
Sub PlaySoundAtVol(sound, tableobj, Vol)
    If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
        PlaySound sound, 1, Vol, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
    Else
        PlaySound sound, 1, Vol, Pan(tableobj)
    End If
End Sub
 
 
'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3
 
Sub PlaySoundAtBallVol(sound, VolMult)
    If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
        PlaySound sound, 0, Vol(ActiveBall) * VolMult, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
    Else
        PlaySound sound, 0, Vol(ActiveBall) * VolMult, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1
    End If
End Sub
 
 
' Notes: To be left in script so others can learn & understand new VPX Surround Code
'
' PlaySoundAtBall "sound",ActiveBall
'   * Sets position as ball and Vol to 1
 
' PlaySoundAtBallVol "sound",x
'   * Same as PlaySounAtBall but sets x as a volume multiplier (1-10) or partial multiplier (.01-.99)
'
' PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
'   * May us used as shown, or with any manual setting, in place of any above Sound Playback Function.
'
' PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1
'   * May us used as shown, or with any manual setting, to maintain 10.3 backwards compatability.
 
'**********************************************************************
 
'**********************************************************************
 
 
'*****************************************
'           BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)
 
Sub BallShadowUpdate_timer()
    Dim BOT, b
    BOT = GetBalls
    ' hide shadow of deleted balls
    If UBound(BOT)<(tnob-1) Then
        For b = (UBound(BOT) + 1) to (tnob-1)
            BallShadow(b).visible = 5
        Next
    End If
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow for each ball
    For b = 0 to UBound(BOT)
        If BOT(b).X < Table1.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/16) + ((BOT(b).X - (Table1.Width/2))/7)) + 13
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/16) + ((BOT(b).X - (Table1.Width/2))/7)) - 13
        End If
        ballShadow(b).Y = BOT(b).Y + 10
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub