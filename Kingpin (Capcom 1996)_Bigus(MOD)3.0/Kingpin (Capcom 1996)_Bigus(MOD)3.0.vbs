
  Option Explicit
   Randomize

'******************* Options *********************
' DMD/BAckglass Controller Setting

' Thalamus 2018-08-09
' Added/Updated "Positional Sound Playback Functions" and "Supporting Ball & Sound Functions"
' Changed UseSolenoids=1 to 2 - reverted back, this table probably needs more script updates to work correctly
' Thalamus 2018-10-23 : Improved directional sounds

' !! NOTE : Table not verified yet !!

' Options
' Volume devided by - lower gets higher sound

Const VolDiv = 2000    ' Lower number, louder ballrolling/collition sound
Const VolCol = 20      ' Ball collition divider ( voldiv/volcol )

' The rest of the values are multipliers
'
'  .5 = lower volume
' 1.5 = higher volume

Const VolBump   = 2    ' Bumpers volume.
Const VolRol    = 1    ' Rollovers volume.
Const VolGates  = 1    ' Gates volume.
Const VolMetal  = 1    ' Metals volume.
Const VolRH     = 1    ' Rubber hits volume.
Const VolPo     = 1    ' Rubber posts volume.
Const VolPi     = 1    ' Rubber pins volume.
Const VolTarg   = 1    ' Targets volume.
Const VolKick   = 1    ' Kicker volume.
Const VolSpin   = 1.5  ' Spinners volume.
Const VolFlip   = 1    ' Flipper volume.


Const cController         = 3         'Set to 1=VPinMAME, 2=UVP backglass server, 3=B2S backglass server, 4=B2S backglass server with no VP mech sounds

Const LampShader          = 0         'Set to 1 to use high performance lamp shaders, set to 0 if you see poor performance

Const HighPerformance     = 0         'Set to 1 for high resolution texture swapping, set to 0 if you see stutter and don't need this feature

Sub LoadCoreVBS
     'On Error Resume Next
     ExecuteGlobal GetTextFile("core.vbs")
     If Err Then MsgBox "Can't open core.vbs"
     On Error Goto 0
End Sub

  Dim VarHidden, UseVPMDMD
If Table.ShowDT = true then
  UseVPMDMD = True
  VarHidden = 1
  l40a.Visible = false
  l40b.Visible = false
  l40a1.Visible = true
  l40b1.Visible = true
else
  UseVPMDMD = False
  VarHidden = 0
  l40a1.Visible = false
  l40b1.Visible = false
  l40a.Visible = true
  l40b.Visible = true
end if

LoadVPM "01130000","CAPCOM.VBS",3.10

Sub LoadVPM(VPMver, VBSfile, VBSver)
  On Error Resume Next
  If ScriptEngineMajorVersion < 5 Then MsgBox "VB Script Engine 5.0 or higher required"
  ExecuteGlobal GetTextFile(VBSfile)
  If Err Then MsgBox "Unable to open " & VBSfile & ". Ensure that it is in the same folder as this table. " & vbNewLine & Err.Description
  Select Case cController
  Case 1
    Set Controller = CreateObject("VPinMAME.Controller")
  If Err Then MsgBox "Can't Load VPinMAME." & vbNewLine & Err.Description
  If VPMver>"" Then If Controller.Version < VPMver Or Err Then MsgBox "VPinMAME ver " & VPMver & " required."
  If VPinMAMEDriverVer < VBSver Or Err Then MsgBox VBSFile & " ver " & VBSver & " or higher required."
  Case 2
  Set Controller = CreateObject("UltraVP.BackglassServ")
  Case else
    Set Controller = CreateObject("B2S.Server")
  End Select
  On Error Goto 0
End Sub

'********************
'Standard definitions
'********************

  Const cGameName = "kpb105" 'change the romname here

     Const UseSolenoids = 1
     Const UseLamps = 0
     Const UseSync = 1
     Const HandleMech = 0

     'Standard Sounds
     Const SSolenoidOn = "Solenoid"
     Const SSolenoidOff = ""
     Const SCoin = "coin"

'*DOF method for rom controller tables by Arngrim********************
'*************************use Tabletype rom or em********************
'*******Use DOF 1**, 1 to activate a ledwiz output*******************
'*******Use DOF 1**, 0 to deactivate a ledwiz output*****************
'*******Use DOF 1**, 2 to pulse a ledwiz output**********************
Sub DOF(dofevent, dofstate)
  If cController >= 3 Then
    If dofstate = 2 Then
      Controller.B2SSetData dofevent, 1:Controller.B2SSetData dofevent, 0
    Else
      Controller.B2SSetData dofevent, dofstate
    End If
  End If
End Sub
'********************************************************************


'************
' Table init.
'************
   'Variables
    'Dim xx
    Dim Bump1,Bump2,Bump3,Mech3bank,bsTrough,bsVUK,visibleLock,bsTEject,bsSVUK,bsRScoop,bsLock
  Dim dtDropL, dtDropR
  Dim PlungerIM
  Dim PMag
  Dim mag2
  Dim bsRHole
  Dim FireButtonFlag:FireButtonFlag = 0
  Dim IsStarted:IsStarted = 0

  Sub Table_Init

'*****

Kicker1.CreateBall
Kicker1.Kick 0,0
Kicker1.Enabled = false

Kicker2.CreateBall
Kicker2.Kick 0,0
Kicker2.Enabled = false

Controller.Switch(47) = 1 'ramp down active

  With Controller
    .GameName = cGameName
    If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
    .SplashInfoLine = "Kingpin"
    .HandleKeyboard = 0
    .ShowTitle = 0
    .ShowDMDOnly = 1
    .ShowFrame = 0
    .HandleMechanics = 1
    .Hidden = VarHidden
        .Games(cGameName).Settings.Value("sound") = 1
    On Error Resume Next
    .Run GetPlayerHWnd
    If Err Then MsgBox Err.Description
  End With


    On Error Goto 0

    Const IMPowerSetting = 52
    Const IMTime = 0.6
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1
    .Switch 43
        .InitExitSnd "plunger2", "plunger"
        .CreateEvents "plungerIM"
    End With

'**Trough
    Set bsTrough = New cvpmBallStack
    bsTrough.InitSw 35,36,37,38,39,0,0,0
    bsTrough.InitKick BallRelease, 50, 10
    bsTrough.InitExitSnd SoundFX("ballrel"), SoundFX("Solenoid")
    bsTrough.Balls = 4

  set dtDropL=new cvpmDropTarget
  dtDropL.InitDrop Array(sw25,sw26,sw27,sw28),Array(25, 26, 27, 28)
  dtDropL.InitSnd "DTR","DTResetL"

  set dtDropR=new cvpmDropTarget
  dtDropR.InitDrop Array(sw29,sw30,sw31),Array(29, 30, 31)
  dtDropR.InitSnd "DTR","DTResetR"

  Set bsVUK=New cvpmBallStack
  bsVUK.InitSw 0,51,0,0,0,0,0,0
  bsVUK.InitKick sw51b,173,28
  bsVUK.KickZ = 55
  bsVUK.KickBalls = 1
  bsVUK.InitExitSnd SoundFX("scoopexit"), SoundFX("rail")

  Set bsLock=New cvpmBallStack
  bsLock.InitSw 0,44,45,46,0,0,0,0
  bsLock.InitKick GunKicker,170,20
  bsLock.InitExitSnd SoundFX("scoopexit"), SoundFX("rail")

'**Nudging
      vpmNudge.TiltSwitch=-7
    vpmNudge.Sensitivity=1
    vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)

      '**Main Timer init
  PinMAMETimer.Interval = PinMAMEInterval
  PinMAMETimer.Enabled = 1



  End Sub

'*****Keys
Sub Table_KeyDown(ByVal Keycode)

  If Keycode = LeftFlipperKey then
    Controller.Switch(5)=1
    If bsTrough.balls < 4 and FlipperDisabled < 4 Then
      PlaySoundAtVol "flipperupleft", LeftFlipper, VolFlip
      LeftFlipper.RotateToEnd
      FlipperDisabled = FlipperDisabled + 1
    End If
    Exit Sub
  End If
  If Keycode = RightFlipperKey then
    Controller.Switch(6)=1
    If bsTrough.balls < 4 and FlipperDisabled < 4 Then
      PlaySoundAtVol "flipperupright", RightFlipper, VolFlip
      RightFlipper.RotateToEnd
      FlipperDisabled = FlipperDisabled + 1
    End If
    Exit Sub
  End If

    If keycode = PlungerKey Then vpmTimer.PulseSw 14
    If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Table_KeyUp(ByVal keycode)
  If vpmKeyUp(keycode) Then Exit Sub

  If Keycode = LeftFlipperKey then
    Controller.Switch(5)=0
    PlaySoundAtVol "flipperdown", LeftFlipper, VolFlip
    LeftFlipper.RotateToStart
    Exit Sub
  End If
  If Keycode = RightFlipperKey then
    Controller.Switch(6)=0
    PlaySoundAtVol "flipperdown", RightFlipper, VolFlip
    RightFlipper.RotateToStart
    Exit Sub
  End If

  If Keycode = StartGameKey Then Controller.Switch(7) = 0 and IsStarted = 1
End Sub

   'Solenoids
SolCallback(1)  = "bsTrough.SolIn"
SolCallback(2)  = "bsTrough.SolOut"

SolCallback(6)    = "dtDropL.SolDropUp"
SolCallback(7)    = "dtDropR.SolDropUp"
SolCallback(8) = "bsLock.SolOut" 'gun eject
SolCallback(9) = "SolLFlipper"
SolCallback(10) = "SolRFlipper"
SolCallback(11) = "bsVUK.SolOut" 'slot machine eject
SolCallback(12) = "SlotMachineMotor"
SolCallBack(13) = "LoopGate"
SolCallback(14) = "SolLeftRamp"

SolCallback(18) = "LeftRampFlash"
SolCallback(19) = "f19.State="
SolCallback(21) = "f21.State="
SolCallback(22) = "f22.State="
SolCallback(23) = "RightRampFlash"
SolCallback(24) = "f24.State="
SolCallback(25) = "f25.State="
SolCallback(26) = "f26.State="
SolCallback(27) = "SetLamp 131,"
SolCallback(28) = "f28.State="
SolCallback(29) = "f29.State="
SolCallback(30) = "f30.State="
SolCallback(31) = "f31.State="
SolCallback(32) = "SolAutoFire"

' SolCallback(sLRFlipper) = "SolRFlipper"
' SolCallback(sLLFlipper) = "SolLFlipper"

Sub start2_timer
  IsStarted = 1
  Controller.Switch(9) = 0
  start2.enabled = 0
End Sub

Sub SlotMachineMotor(Enabled)
  If Enabled then
    SlotTimer.Enabled = 1
    If IsStarted = 0 then Controller.Switch(9) = 1:start2.enabled = 1:End If '***ENABLE THIS IS LAMPS FLICKER (AND YOU REFUSE TO UPDATE VPINMAME)
  Else
    SlotTimer.Enabled = 0
  End If
End Sub

Sub SMCTimer_Timer()
' Debug.print "cylinder " & SLOTmachineCylinder.RotX & " slotpos " & SlotPos
  SLOTmachineCylinder.RotX = SlotPos
End Sub

'************* SLOT MACHINE *************
'SLOT MACHINE BASED ON SCRIPT BY DESTRUK AND UNCLE REAMUS
'Row 1=Money 320
'Row 2=Goods 0
'Row 3=Sevens 40
'Row 4=Gangsters 80
'Row 5=Bars 120
'Row 6=Power 160
'Row 7=Guns 200
'Row 8=Crazy Cash 240
'Row 9=Cherries 280

Dim SlotPos
SlotPos=0

Sub SlotTimer_Timer
SlotPos=SlotPos+1

Select Case SlotPos
  Case 0:Controller.Switch(52)=1 'goods
  Case 39:Controller.Switch(52)=0
  Case 40:Controller.Switch(52)=1 'sevens
  Case 79:Controller.Switch(52)=0
  Case 80:Controller.Switch(52)=1 'gangsters
  Case 119:Controller.Switch(52)=0
  Case 120:Controller.Switch(52)=1 'bars
  Case 159:Controller.Switch(52)=0
  Case 160:Controller.Switch(52)=1 'power
  Case 199:Controller.Switch(52)=0
  Case 200:Controller.Switch(52)=1 'guns
  Case 239:Controller.Switch(52)=0
  Case 240:Controller.Switch(52)=1 'crazy cash
  Case 279:Controller.Switch(52)=0
  Case 280:Controller.Switch(52)=1 'cherries
  Case 319:Controller.Switch(52)=0
  Case 320:Controller.Switch(52)=1 'money
  Case 359:Controller.Switch(52)=0
  Case 360:SlotPos = 0
End Select
End Sub

'''Flashers'''

Dim RRFON:RRFON = 0
Dim LRFON:LRFON = 0
Sub LeftRampFlash(Enabled)
  If Enabled Then
    SetFlash 130, 1
    SetLamp 140, 1
    SetLamp 132, 1
    SetLamp 133, 1
    SetLamp 134, 1
    SetLamp 136, 1
    SetLamp 130, 1
    SetLamp 141, 1
    SetLamp 142, 1
    SetLamp 143, 1
    p122r.image = "Wire2R_ON3"
    If LRFON = 1 then
      SetLamp139, 1
    End If
    If LRFON = 0 then
      SetLamp 137, 1
    End If
  Else
    SetFlash 130, 0
    SetLamp 140, 0
    SetLamp 132, 0
    SetLamp 133, 0
    SetLamp 134, 0
    SetLamp 136, 0
    SetLamp 130, 0
    SetLamp 141, 0
    SetLamp 142, 0
    SetLamp 143, 0
    p122r.image = "Off"
    If LRFON = 1 then
      SetLamp138, 1
    End If
    If LRFON = 0 then
      SetLamp 137, 0
    End If
  End If
End Sub

Sub RightRampFlash(Enabled)
  If Enabled Then
    SetFlash 129, 1
    SetLamp 129, 1
    SetLamp 135, 1
    SetLamp 144, 1
    If RRFON = 1 then
      SetLamp139, 1
    End If
    If RRFON = 0 then
      SetLamp 138, 1
    End If
  Else
    SetFlash 129, 0
    SetLamp 129, 0
    SetLamp 135, 0
    SetLamp 144, 0
    If RRFON = 1 then
      SetLamp137, 1
    End If
    If RRFON = 0 then
      SetLamp 138, 0
    End If
  End If
End Sub

Dim LoopOpen:LoopOpen = 0
Sub LoopGate(Enabled)
  If Enabled then
    If LoopOpen = 1 then
      Gate2.Collidable = 1
      LoopOpen = 0
    Else
      Gate2.Collidable = 0
      LoopOpen = 1
    End If
  End If
End Sub

dim RampUp:RampUp = 0
Sub SolLeftRamp(Enabled)
  If Enabled Then
    If RampUp = 0 Then
      LeftRampFlipper.RotateToEnd
      Controller.Switch(47) = 0
      RampUp = 1
    Else
      LeftRampFlipper.RotateToStart
      Controller.Switch(47) = 1
      RampUp = 0
    End If
  End If

End Sub



Dim KickNow:KickNow = 0
'Sub Kicker3_Hit:Kicker3.DestroyBall:GunEject.Enabled = 1:GunEjectFlipper.RotateToEnd:End Sub
Sub Kicker4_Hit:GunEject.Enabled = 1:GunEjectFlipper.RotateToEnd:End Sub

Sub GunEject_Timer
  'Kicker4.CreateBall
  Kicker4.KickZ 170, 20, 0, 55
  GunEjectFlipper.RotateToStart
  GunEject.Enabled = 0
End Sub



Sub solTrough(Enabled)
  If Enabled Then
    bsTrough.ExitSol_On
  End If
 End Sub

Sub solAutofire(Enabled)
  If Enabled Then
    PlungerIM.AutoFire
  End If
 End Sub


'primitive flippers!
dim MotorCallback
Set MotorCallback = GetRef("GameTimer")
Sub GameTimer
    UpdateFlipperLogos
End Sub

Sub UpdateFlipperLogo_Timer
    LFLogo.RotY = LeftFlipper.CurrentAngle
    RFlogo.RotY = RightFlipper.CurrentAngle
  Primitive_Metalramp1.ObjRotX = LeftRampFlipper.CurrentAngle
  gunejectprim.ObjRotX = GunEjectFlipper.CurrentAngle
End Sub



'******************************************
' Use FlipperTimers to call div subs
'******************************************




Dim FlipperDisabled
       Sub SolLFlipper(Enabled)
           If Enabled Then
        FlipperDisabled = 0
       Else
        If bsTrough.balls = 4 Then
          Debug.print "soloff"
            'PlaySound "flipperdown"
          LeftFlipper.RotateToStart
        end if
           End If
       End Sub

       Sub SolRFlipper(Enabled)
           If Enabled Then
        FlipperDisabled = 0
       Else
        If bsTrough.balls = 4 Then
          Debug.print "solof"
            'PlaySound "flipperdown"
          RightFlipper.RotateToStart
        end if

           End If
       End Sub



'***Slings and rubbers

 Sub LeftSlingShot_Slingshot
  Leftsling = True
  Controller.Switch(41) = 1
  PlaySoundAtVol Soundfx("slingshotleft"), ActiveBall, 1:LeftSlingshot.TimerEnabled = 1
  End Sub

Dim Leftsling:Leftsling = False

Sub LS_Timer()
  If Leftsling = True and Left1.ObjRotZ < -7 then Left1.ObjRotZ = Left1.ObjRotZ + 2
  If Leftsling = False and Left1.ObjRotZ > -20 then Left1.ObjRotZ = Left1.ObjRotZ - 2
  If Left1.ObjRotZ >= -7 then Leftsling = False
  If Leftsling = True and Left2.ObjRotZ > -212.5 then Left2.ObjRotZ = Left2.ObjRotZ - 2
  If Leftsling = False and Left2.ObjRotZ < -199.5 then Left2.ObjRotZ = Left2.ObjRotZ + 2
  If Left2.ObjRotZ <= -212.5 then Leftsling = False
  If Leftsling = True and Left3.TransZ > -23 then Left3.TransZ = Left3.TransZ - 4
  If Leftsling = False and Left3.TransZ < -0 then Left3.TransZ = Left3.TransZ + 4
  If Left3.TransZ <= -23 then Leftsling = False
End Sub

 Sub LeftSlingShot_Timer:Me.TimerEnabled = 0:Controller.Switch(41) = 0:End Sub

 Sub RightSlingShot_Slingshot
  Rightsling = True
  Controller.Switch(42) = 1
  PlaySoundAtVol Soundfx("slingshotright"), ActiveBall, 1:RightSlingshot.TimerEnabled = 1
  End Sub

 Dim Rightsling:Rightsling = False

Sub RS_Timer()
  If Rightsling = True and Right1.ObjRotZ > 7 then Right1.ObjRotZ = Right1.ObjRotZ - 2
  If Rightsling = False and Right1.ObjRotZ < 20 then Right1.ObjRotZ = Right1.ObjRotZ + 2
  If Right1.ObjRotZ <= 7 then Rightsling = False
  If Rightsling = True and Right2.ObjRotZ < 212.5 then Right2.ObjRotZ = Right2.ObjRotZ + 2
  If Rightsling = False and Right2.ObjRotZ > 199.5 then Right2.ObjRotZ = Right2.ObjRotZ - 2
  If Right2.ObjRotZ >= 212.5 then Rightsling = False
  If Rightsling = True and Right3.TransZ > -23 then Right3.TransZ = Right3.TransZ - 4
  If Rightsling = False and Right3.TransZ < -0 then Right3.TransZ = Right3.TransZ + 4
  If Right3.TransZ <= -23 then Rightsling = False
End Sub

 Sub RightSlingShot_Timer:Me.TimerEnabled = 0:Controller.Switch(42) = 0:End Sub

Sub Bumper1_Hit:vpmTimer.PulseSw 57:PlaySoundAtVol "bumper", Bumper1, VolBump:Bumper1.TimerEnabled = 1:End Sub
Sub Bumper1_Timer:Bumper1.TimerEnabled = 0:End Sub

Sub Bumper2_Hit:vpmTimer.PulseSw 58:PlaySoundAtVol "bumper", Bumper2, VolBump:Bumper2.TimerEnabled = 1:End Sub
Sub Bumper2_Timer:Bumper2.TimerEnabled = 0:End Sub

Sub Bumper3_Hit:vpmTimer.PulseSw 59:PlaySoundAtVol "bumper", Bumper3, VolBump:Bumper3.TimerEnabled = 1:End Sub
Sub Bumper3_Timer:Bumper3.TimerEnabled = 0:End Sub

 'Drains and Kickers
Dim BallInPlay:BallInPlay = 0

Sub Drain_Hit
  PlaySoundAtVol "Drain", Drain, 1
  bsTrough.AddBall Me
  BallInPlay = BallInPlay - 1
End Sub



Sub BallRelease_UnHit(): BallInPlay = BallInPlay + 1:End Sub


Sub RLS_Timer()
              Primitive_RXRspinner1.RotX = -(sw61.currentangle) +90
              Primitive_RXRspinner2.RotX = -(sw17.currentangle) +90
  If RampUp = 1 then Ramp5.Collidable = 0
  If RampUp = 0 then Ramp5.Collidable = 1
End Sub


'Sub sw44_Hit:bsLock.AddBall Me:End Sub

Sub sw44_Hit
  Set aBall = ActiveBall
  aZpos = 50
  Me.TimerInterval = 2
  Me.TimerEnabled = 1
End Sub



Sub sw44_Timer
  aBall.Z = aZpos
  aZpos = aZpos-2
  If aZpos <40 Then
    Me.TimerEnabled = 0
    Me.DestroyBall
    bsLock.AddBall Me
  End If
End Sub

Sub sw51a_Hit:sw51.Enabled = 1:End Sub
'Sub sw51_Hit:bsVUK.AddBall Me:sw51.Enabled = 0:End Sub
Dim aBall, aZpos



Sub sw51_Hit
  Set aBall = ActiveBall
  aZpos = 50
  Me.TimerInterval = 2
  Me.TimerEnabled = 1
  playsoundAtVol "scoopleft", ActiveBall, 1
End Sub



Sub sw51_Timer
  aBall.Z = aZpos
  aZpos = aZpos-2
  If aZpos <40 Then
    Me.TimerEnabled = 0
    Me.DestroyBall
    bsVUK.AddBall Me
    sw51.Enabled = 0
  End If
End Sub

Sub sw19_Hit:Me.TimerEnabled = 1:Controller.Switch(19) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub 'left inlane
Sub sw19_Timer:Me.TimerEnabled = 0:Controller.Switch(19) = 0:End Sub
Sub sw20_Hit:Me.TimerEnabled = 1:Controller.Switch(20) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub 'right inlane
Sub sw20_Timer:Me.TimerEnabled = 0:Controller.Switch(20) = 0:End Sub
Sub sw21_Hit:Me.TimerEnabled = 1:Controller.Switch(21) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub 'left outlane
Sub sw21_Timer:Me.TimerEnabled = 0:Controller.Switch(21) = 0:End Sub
Sub sw22_Hit:Me.TimerEnabled = 1:Controller.Switch(22) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub 'right outlane
Sub sw22_Timer:Me.TimerEnabled = 0:Controller.Switch(22) = 0:End Sub
Sub sw23_Hit:Me.TimerEnabled = 1:Controller.Switch(23) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub 'left orbit
Sub sw23_Timer:Me.TimerEnabled = 0:Controller.Switch(23) = 0:End Sub
Sub sw24_Hit:Me.TimerEnabled = 1:Controller.Switch(24) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub 'right orbit
Sub sw24_Timer:Me.TimerEnabled = 0:Controller.Switch(24) = 0:End Sub

Sub sw25_Hit:dtDropL.Hit 1:End Sub  'K
Sub sw26_Hit:dtDropL.Hit 2:End Sub  'I
Sub sw27_Hit:dtDropL.Hit 3:End Sub  'N
Sub sw28_Hit:dtDropL.Hit 4:End Sub  'G
Sub sw29_Hit:dtDropR.Hit 1:End Sub  'P
Sub sw30_Hit:dtDropR.Hit 2:End Sub  'I
Sub sw31_Hit:dtDropR.Hit 3:End Sub  'N

Sub sw32_Hit  : vpmTimer.PulseSw 49:sw32.TimerEnabled = 1:sw32p.TransX = -4: playsoundAtVol SoundFX("target"), Primitive241, 1: End Sub 'captive ball
Sub sw32_Timer:Me.TimerEnabled = 0:sw32p.TransX = 0:End Sub

Sub swPlunger_Hit:Me.TimerEnabled = 1:Controller.Switch(43) = 1:PlaySoundAtVol "rollover", swPlunger, 1:End Sub 'plunger lane
Sub swPlunger_Timer:Me.TimerEnabled = 0:Controller.Switch(43) = 0:End Sub

Sub sw53_Hit:Me.TimerEnabled = 1:Controller.Switch(53) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub 'k top rollover
Sub sw53_Timer:Me.TimerEnabled = 0:Controller.Switch(53) = 0:End Sub
Sub sw54_Hit:Me.TimerEnabled = 1:Controller.Switch(54) = 1:PlaySoundAtVol "rollover", ActiveBall, VolRol:End Sub 'i top rollover
Sub sw54_Timer:Me.TimerEnabled = 0:Controller.Switch(54) = 0:End Sub
Sub sw55_Hit:Me.TimerEnabled = 1:Controller.Switch(55) = 1:PlaySoundAtVol "rollover", Activeball, VolRol:End Sub 'd top rollover
Sub sw55_Timer:Me.TimerEnabled = 0:Controller.Switch(55) = 0:End Sub

Sub sw49_Hit  : vpmTimer.PulseSw 49:sw49.TimerEnabled = 1:sw49p.TransX = -4: playsoundAtVol SoundFX("target"), Activeball, VolTarg: End Sub
Sub sw49_Timer:Me.TimerEnabled = 0:sw49p.TransX = 0:End Sub
Sub sw50_Hit  : vpmTimer.PulseSw 50:sw50.TimerEnabled = 1:sw50p.TransX = -4: playsoundAtVol SoundFX("target"), ActiveBall, VolTarg: End Sub
Sub sw50_Timer:Me.TimerEnabled = 0:sw50p.TransX = 0:End Sub
Sub sw60_Hit  : vpmTimer.PulseSw 60:sw60.TimerEnabled = 1:sw60p.TransX = -4: playsoundAtVol SoundFX("target"), ActiveBall, VolTarg: End Sub
Sub sw60_Timer:Me.TimerEnabled = 0:sw60p.TransX = 0:End Sub
Sub sw63_Hit  : vpmTimer.PulseSw 63:sw63.TimerEnabled = 1:sw63p.TransX = -4: playsoundAtVol SoundFX("target"), ActiveBall, VolTarg: End Sub
Sub sw63_Timer:Me.TimerEnabled = 0:sw63p.TransX = 0:End Sub

Sub sw61_Spin:vpmTimer.PulseSw 61:PlaySoundAtVol SoundFX("spinner"), sw61, VolSpin:End Sub 'left ramp spinner

Sub sw17_Spin:vpmTimer.PulseSw 17:PlaySoundAtVol SoundFX("spinner"), sw18, VolSpin:End Sub 'right ramp spinner
Sub sw18_Hit:Me.TimerEnabled = 1:Controller.Switch(18) = 1:End Sub
Sub sw18_Timer:Me.TimerEnabled = 0:Controller.Switch(18) = 0:End Sub

Sub sw61_Spin:vpmTimer.PulseSw 61:PlaySoundAtVol "spinner", sw61, VolSpin:End Sub 'left ramp spinner
Sub sw62_Hit:Me.TimerEnabled = 1:Controller.Switch(62) = 1:End Sub
Sub sw62_Timer:Me.TimerEnabled = 0:Controller.Switch(62) = 0:End Sub

 '****************************************
 ' SetLamp 0 is Off
 ' SetLamp 1 is On
 ' LampState(x) current state
 '****************************************

'Dim RefreshARlight
Dim LampState(200), FadingLevel(200), FadingState(200)
Dim FlashState(200), FlashLevel(200)
Dim FlashSpeedUp, FlashSpeedDown
Dim x


AllLampsOff()
LampTimer.Interval = 40 'lamp fading speed
LampTimer.Enabled = 1
'
FlashInit()
FlasherTimer.Interval = 10 'flash fading speed
FlasherTimer.Enabled = 1
'
'' Lamp & Flasher Timers
'
Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4
      FlashState(chgLamp(ii, 0) ) = chgLamp(ii, 1)

        Next
    End If

    UpdateLamps
End Sub

GISetDefaultColorTimer.Interval = 1000
Sub GISetDefaultColorTimer_Timer  'If timer expires, no mode is running so set defaultGIcolor
  If GI_Light.State = 1 Then
    red = 0:green = 0:blue = 255
  End If
  GISetDefaultColorTimer.Enabled = 0
End Sub

Sub FlashInit
    Dim i
    For i = 0 to 200
        FlashState(i) = 0
        FlashLevel(i) = 0
    Next

    FlashSpeedUp = 50   ' fast speed when turning on the flasher
    FlashSpeedDown = 50 ' slow speed when turning off the flasher, gives a smooth fading
    AllFlashOff()
End Sub

Sub AllFlashOff
    Dim i
    For i = 0 to 200
        FlashState(i) = 0
    Next
End Sub

Sub Reflections_Timer()

End Sub

Sub UpdateLamps()

    NFadeLm 40, l40a
    NFadeLm 40, l40b
    NFadeLm 40, l40a1
    NFadeL 40, l40b1

    NFadeL 81, l81
    NFadeL 82, l82
    NFadeL 83, l83
    NFadeLm 84, l84a
    NFadeL 84, GIWhite 'overall color
    'NFadeL 84, l84b
    NFadeLm 87, l87a
    NFadeL 87, GIRed 'overall color
    'NFadeL 87, l87b
    NFadeL 88, l88a
    'NFadeL 88, l88b
    NFadeL 89, l89a
    'NFadeL 89, l89b
    NFadeL 90, l90a
    'NFadeL 90, l90b
    NFadeL 91, l91
    NFadeL 92, l92a
    'NFadeL 92, l92b
    NFadeL 93, l93a
    'NFadeL 93, l93b
    NFadeL 94, l94a
    'NFadeL 94, l94b
    NFadeLm 85, l85a 'left flipper return
    NFadeLm 85, l85b 'left flipper return
    'NFadeLm 85, l85c 'left flipper return
    NFadeL 85, l85f 'left flipper return
    'NFadeL 85, l85d 'left flipper return
    NFadeLm 95, l95e 'left slingshot
    NFadeL 95, l95f 'left slingshot
    'NFadeLm 95, l95g 'left slingshot
    'NFadeL 95, l95h 'left slingshot
    NFadeLm 86, l86a 'right flipper return
    NFadeLm 86, l86b 'right flipper return
    'NFadeLm 86, l86c 'right flipper return
    NFadeL 86, l86f 'right flipper return
    'NFadeL 86, l86d 'right flipper return
    NFadeLm 96, l96e 'right slingshot
    NFadeL 96, l96f 'right slingshot
    'NFadeLm 96, l96g 'right slingshot
    'NFadeL 96, l96h 'right slingshot
    NFadeLm 97, l97a 'upper lane guide k-i
    NFadeL 97, l97b 'upper lane guide k-i
    NFadeLm 98, l98a 'upper lane guide i-d
    NFadeL 98, l98b 'upper lane guide i-d
    NFadeL 99, l99a
    'NFadeL 99, l99b
    NFadeL 100, l100a
    'NFadeL 100, l100b
    NFadeL 101, l101a
    'NFadeL 101, l101b
    NFadeL 102, l102a
    'NFadeL 102, l102b
    NFadeL 103, l103a
    'NFadeL 103, l103b
    NFadeL 104, l104a
    'NFadeL 104, l104b
    NFadeL 105, l105a
    'NFadeL 105, l105b
    NFadeL 106, l106a
    'NFadeL 106, l106b
    NFadeL 107, l107a
    'NFadeL 107, l107b
    NFadeL 108, l108a
    'NFadeL 108, l108b
    NFadeL 109, l109
    NFadeL 110, l110
    NFadeL 111, l111a
    'NFadeL 111, l111b
    NFadeL 112, l112a
    'NFadeL 112, l112b
    NFadeL 113, l113
    NFadeL 114, l114
    NFadeL 115, l115a
    'NFadeL 115, l115b
    NFadeL 116, l116
    'NFadeL 117, l117a
    'NFadeL 117, l117b
    NFadeL 118, l118a
    'NFadeL 118, l118b
    NFadeL 119, l119
    NFadeL 120, l120
    NFadeL 121, l121a 'left spinner flash
    Flash 121, l121b 'left spinner flash
    NFadeL 122, l122a 'right spinner flash
    Flash 122, l122b 'right spinner flash
    NFadeL 127, l127a
    'NFadeL 127, l127b
    'NFadeL 127, l127c
    'NFadeL 127, l127d
    NFadeLm 128, l128a
    'NFadeLm 128, l128b
    NFadeL 128, l128c
    'NFadeL 128, l128d
    NFadeLm 129, f23a 'hotel lex flash
    NFadeL 129, f23b 'hotel lex flash
    'Flash 129, f23c 'hotel lex flash
    NFadeLm 130, f22a 'right ramp flash
    NFadeL 130, f22b 'right ramp flash
    'Flash 130, f22c 'right ramp flash
    NFadeLm 131, f27a
    NFadeL 131, f27b

  If HighPerformance = 1 then
    FadePrim 140, Backwall, "backwall-kingpinON3", "backwall-kingpinON2", "backwall-kingpinON1","backwall-kingpin"
    FadePrim 132, Primitive_ramp1, "Ramp1RF_ON3", "Ramp1RF_ON2", "Ramp1RF_ON1","Ramp1map_OFF80"
    FadePrim 141, Primitive_ramp1decal, "Rampdecalredraw2ON3", "Rampdecalredraw2ON2", "Rampdecalredraw2ON1","Rampdecalredraw2"
    FadePrim 133, Primitive_ramp2, "Ramp2map_RFON3", "Ramp2map_RFON2", "Ramp2map_RFON1", "Ramp2map_OFF80"
    FadePrim 134, Primitive_rampcover, "PlasticCover_RFON3", "PlasticCover_RFON2", "PlasticCover_RFON1", "PlasticCoverMap"
    FadePrim 135, Primitive_LWireReflect, "LwireReflect_ON3", "LwireReflect_ON2", "LwireReflect_ON1", "Off"
    FadePrim 136, Primitive_Reflect2, "RwireReflect_ON3", "RwireReflect_ON2", "RwireReflect_ON1", "Off"
    FadePrim 137, HotelLEX, "HotelLex_RRFON3", "HotelLex_RRFON2", "HotelLex_RRFON1", "HotelLex"
    FadePrim 138, HotelLEX, "HotelLex_LRFON3", "HotelLex_LRFON2", "HotelLex_LRFON1", "HotelLex"
    FadePrim 139, HotelLEX, "HotelLex_2XRFON3", "HotelLex_2XRFON2", "HotelLex_2XRFON1", "HotelLex"
    FadePrim 142, centerwirelower, "WireS_ON3", "WireS_ON2", "WireS_ON1", "Off"
    FadePrim 143, p122b2, "wirebendR_ON3", "wirebendR_ON2", "wirebendR_ON1", "Off" 'right to left wire
    FadePrim 144, p122b1, "wirebendL_ON3", "wirebendL_ON2", "wirebendL_ON1", "Off" 'left to right wire
  End If
End Sub

Sub SpinCityR_Timer()
  If HighPerformance = 1 then
    If l122b.opacity = 0 then p122a.image = "off":p122b.image = "off":end if
    If l122b.opacity >= 1 and l122b.opacity <= 33 then p122a.image = "Wire2_ON1":p122b.image = "wirebendY_ON1":end if
    If l122b.opacity >= 34 and l122b.opacity <= 66 then p122a.image = "Wire2_ON2":p122b.image = "wirebendY_ON2":end if
    If l122b.opacity >= 67 and l122b.opacity <= 100 then p122a.image = "Wire2_ON3":p122b.image = "wirebendY_ON3":end if
  End If
End Sub

Sub FadePrim(nr, pri, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:pri.image = d:FadingLevel(nr) = 0
        Case 3:pri.image = c:FadingLevel(nr) = 2
        Case 4:pri.image = b:FadingLevel(nr) = 3
        Case 5:pri.image = a:FadingLevel(nr) = 1
    End Select
End Sub

Sub FadeL(nr, a, b)
    Select Case FadingLevel(nr)
        Case 2:b.state = 0:FadingLevel(nr) = 0
        Case 3:b.state = 1:FadingLevel(nr) = 2
        Case 4:a.state = 0:FadingLevel(nr) = 3
        Case 5:a.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeL(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.state = 0:FadingLevel(nr) = 0
        Case 5:a.State = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeLm(nr, a)
    Select Case FadingLevel(nr)
        Case 4:a.state = 0
        Case 5:a.State = 1
    End Select
End Sub

Sub Flash(nr, object)
    Select Case FlashState(nr)
        Case 0 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown
            If FlashLevel(nr) < 0 Then
                FlashLevel(nr) = 0
                FlashState(nr) = -1 'completely off
            End if
            Object.opacity = FlashLevel(nr)
        Case 1 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp
            If FlashLevel(nr) > 100 Then
                FlashLevel(nr) = 100
                FlashState(nr) = -2 'completely on
            End if
            Object.opacity = FlashLevel(nr)
    End Select
End Sub

 Sub AllLampsOff():For x = 1 to 200:LampState(x) = 4:FadingLevel(x) = 4:Next:UpdateLamps:UpdateLamps:Updatelamps:End Sub


Sub SetLamp(nr, value)
    If value = 0 AND LampState(nr) = 0 Then Exit Sub
    If value = 1 AND LampState(nr) = 1 Then Exit Sub
    LampState(nr) = abs(value) + 4
FadingLevel(nr ) = abs(value) + 4: FadingState(nr ) = abs(value) + 4
End Sub

Sub SetFlash(nr, stat)
    FlashState(nr) = ABS(stat)
End Sub


Sub FlashAR(nr, ramp, a, b, c, r)                                                          'used for reflections when there is no off ramp
    Select Case FadingState(nr)
        Case 2:ramp.image = 0:r.State = ABS(r.state -1):FadingState(nr) = 0                'Off
        Case 3:ramp.image = c:r.State = ABS(r.state -1):FadingState(nr) = 2                'fading...
        Case 4:ramp.image = b:r.State = ABS(r.state -1):FadingState(nr) = 3                'fading...
        Case 5:ramp.image = a:ramp.alpha = 1:r.State = ABS(r.state -1):FadingState(nr) = 1 'ON
    End Select
End Sub


Sub FlashARm(nr, ramp, a, b, c, r)
    Select Case FadingState(nr)
        Case 2:ramp.alpha = 0:r.State = ABS(r.state -1)
        Case 3:ramp.image = c:r.State = ABS(r.state -1)
        Case 4:ramp.image = b:r.State = ABS(r.state -1)
        Case 5:ramp.image = a:ramp.alpha = 1:r.State = ABS(r.state -1)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it doesn't change the flashstate
    Select Case FlashState(nr)
        Case 0         'off
            Object.alpha = FlashLevel(nr)
        Case 1         ' on
            Object.alpha = FlashLevel(nr)
    End Select
End Sub

'SOUNDS
Dim leftdrop:leftdrop = 0
Sub leftdrop1_Hit:leftdrop = 1:End Sub
Sub leftdrop2_Hit:
  If leftdrop = 1 then
    PlaySoundAtVol "drop_left", leftdrop2, 1
  End If
  StopSound "fx_metalrolling"
  leftdrop = 0
End Sub

Dim rightdrop:rightdrop = 0
Sub rightdrop1_Hit:rightdrop = 1:End Sub
Sub rightdrop2_Hit
  If rightdrop = 1 then
    PlaySoundAtVol "drop_Right", rightdrop2, 1
  End If
  StopSound "fx_metalrolling"
  rightdrop = 0
End Sub


Dim gistep
Dim gistep2

gistep = 255 / 8
gistep2 = 180 / 8

Sub RampsOff

End Sub

Sub RampsOn

End Sub

Sub FlippersOn

End Sub

Sub FlippersOff

End Sub

Sub FlippersRedOn

End Sub

'drop targets using flippers
Sub PrimT_Timer
  If sw25.isdropped = true then sw25f.rotatetoend
  if sw25.isdropped = false then sw25f.rotatetostart
  If sw26.isdropped = true then sw26f.rotatetoend
  if sw26.isdropped = false then sw26f.rotatetostart
  If sw27.isdropped = true then sw27f.rotatetoend
  if sw27.isdropped = false then sw27f.rotatetostart
  If sw28.isdropped = true then sw28f.rotatetoend
  if sw28.isdropped = false then sw28f.rotatetostart
  If sw29.isdropped = true then sw29f.rotatetoend
  if sw29.isdropped = false then sw29f.rotatetostart
  If sw30.isdropped = true then sw30f.rotatetoend
  if sw30.isdropped = false then sw30f.rotatetostart
  If sw31.isdropped = true then sw31f.rotatetoend
  if sw31.isdropped = false then sw31f.rotatetostart
  sw25p.transy = sw25f.currentangle
  sw26p.transy = sw26f.currentangle
  sw27p.transy = sw27f.currentangle
  sw28p.transy = sw28f.currentangle
  sw29p.transy = sw29f.currentangle
  sw30p.transy = sw30f.currentangle
  sw31p.transy = sw31f.currentangle
End Sub


Sub Table_exit()
  Controller.Pause = False
  Controller.Stop
End Sub

Dim ToggleMechSounds
Function SoundFX (sound)
  If cController = 4 and ToggleMechSounds = 0 Then
    SoundFX = ""
  Else
    SoundFX = sound
  End If
End Function

Sub ColorCheck_Timer
slotgi.State = l101a.State
slotgired.State = l102a.State
End Sub

'Dim LED(128)
' LED(0)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(1)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(2)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(3)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
'LED(4)=Array(l5a16,l5a15,l5a14,l5a13,l5a12,l5a11,l5a10,l5a9,l5a8,l5a7,l5a6,l5a5,l5a4,l5a3,l5a2,l5a1)
'LED(5)=Array(l6a16,l6a15,l6a14,l6a13,l6a12,l6a11,l6a10,l6a9,l6a8,l6a7,l6a6,l6a5,l6a4,l6a3,l6a2,l6a1)
'LED(6)=Array(l7a16,l7a15,l7a14,l7a13,l7a12,l7a11,l7a10,l7a9,l7a8,l7a7,l7a6,l7a5,l7a4,l7a3,l7a2,l7a1)
'LED(7)=Array(l8a16,l8a15,l8a14,l8a13,l8a12,l8a11,l8a10,l8a9,l8a8,l8a7,l8a6,l8a5,l8a4,l8a3,l8a2,l8a1)
'LED(8)=Array(l9a16,l9a15,l9a14,l9a13,l9a12,l9a11,l9a10,l9a9,l9a8,l9a7,l9a6,l9a5,l9a4,l9a3,l9a2,l9a1)
'LED(9)=Array(l10a16,l10a15,l10a14,l10a13,l10a12,l10a11,l10a10,l10a9,l10a8,l10a7,l10a6,l10a5,l10a4,l10a3,l10a2,l10a1)
'LED(10)=Array(l11a16,l11a15,l11a14,l11a13,l11a12,l11a11,l11a10,l11a9,l11a8,l11a7,l11a6,l11a5,l11a4,l11a3,l11a2,l11a1)
'LED(11)=Array(l12a16,l12a15,l12a14,l12a13,l12a12,l12a11,l12a10,l12a9,l12a8,l12a7,l12a6,l12a5,l12a4,l12a3,l12a2,l12a1)
'LED(12)=Array(l13a16,l13a15,l13a14,l13a13,l13a12,l13a11,l13a10,l13a9,l13a8,l13a7,l13a6,l13a5,l13a4,l13a3,l13a2,l13a1)
'LED(13)=Array(l14a16,l14a15,l14a14,l14a13,l14a12,l14a11,l14a10,l14a9,l14a8,l14a7,l14a6,l14a5,l14a4,l14a3,l14a2,l14a1)
'LED(14)=Array(l15a16,l15a15,l15a14,l15a13,l15a12,l15a11,l15a10,l15a9,l15a8,l15a7,l15a6,l15a5,l15a4,l15a3,l15a2,l15a1)
'LED(15)=Array(l16a16,l16a15,l16a14,l16a13,l16a12,l16a11,l16a10,l16a9,l16a8,l16a7,l16a6,l16a5,l16a4,l16a3,l16a2,l16a1)
'LED(16)=Array(l17a16,l17a15,l17a14,l17a13,l17a12,l17a11,l17a10,l17a9,l17a8,l17a7,l17a6,l17a5,l17a4,l17a3,l17a2,l17a1)
'LED(17)=Array(l18a16,l18a15,l18a14,l18a13,l18a12,l18a11,l18a10,l18a9,l18a8,l18a7,l18a6,l18a5,l18a4,l18a3,l18a2,l18a1)
'LED(18)=Array(l19a16,l19a15,l19a14,l19a13,l19a12,l19a11,l19a10,l19a9,l19a8,l19a7,l19a6,l19a5,l19a4,l19a3,l19a2,l19a1)
' LED(19)=Array(l20a16,l20a15,l20a14,l20a13,l20a12,l20a11,l20a10,l20a9,l20a8,l20a7,l20a6,l20a5,l20a4,l20a3,l20a2,l20a1)
' LED(20)=Array(l21a16,l21a15,l21a14,l21a13,l21a12,l21a11,l21a10,l21a9,l21a8,l21a7,l21a6,l21a5,l21a4,l21a3,l21a2,l21a1)
' LED(21)=Array(l22a16,l22a15,l22a14,l22a13,l22a12,l22a11,l22a10,l22a9,l22a8,l22a7,l22a6,l22a5,l22a4,l22a3,l22a2,l22a1)
' LED(22)=Array(l23a16,l23a15,l23a14,l23a13,l23a12,l23a11,l23a10,l23a9,l23a8,l23a7,l23a6,l23a5,l23a4,l23a3,l23a2,l23a1)
' LED(23)=Array(l24a16,l24a15,l24a14,l24a13,l24a12,l24a11,l24a10,l24a9,l24a8,l24a7,l24a6,l24a5,l24a4,l24a3,l24a2,l24a1)
' LED(24)=Array(l25a16,l25a15,l25a14,l25a13,l25a12,l25a11,l25a10,l25a9,l25a8,l25a7,l25a6,l25a5,l25a4,l25a3,l25a2,l25a1)
' LED(25)=Array(l26a16,l26a15,l26a14,l26a13,l26a12,l26a11,l26a10,l26a9,l26a8,l26a7,l26a6,l26a5,l26a4,l26a3,l26a2,l26a1)
' LED(26)=Array(l27a16,l27a15,l27a14,l27a13,l27a12,l27a11,l27a10,l27a9,l27a8,l27a7,l27a6,l27a5,l27a4,l27a3,l27a2,l27a1)
' LED(27)=Array(l28a16,l28a15,l28a14,l28a13,l28a12,l28a11,l28a10,l28a9,l28a8,l28a7,l28a6,l28a5,l28a4,l28a3,l28a2,l28a1)
' LED(28)=Array(l29a16,l29a15,l29a14,l29a13,l29a12,l29a11,l29a10,l29a9,l29a8,l29a7,l29a6,l29a5,l29a4,l29a3,l29a2,l29a1)
' LED(29)=Array(l30a16,l30a15,l30a14,l30a13,l30a12,l30a11,l30a10,l30a9,l30a8,l30a7,l30a6,l30a5,l30a4,l30a3,l30a2,l30a1)
' LED(30)=Array(l31a16,l31a15,l31a14,l31a13,l31a12,l31a11,l31a10,l31a9,l31a8,l31a7,l31a6,l31a5,l31a4,l31a3,l31a2,l31a1)
' LED(31)=Array(l32a16,l32a15,l32a14,l32a13,l32a12,l32a11,l32a10,l32a9,l32a8,l32a7,l32a6,l32a5,l32a4,l32a3,l32a2,l32a1)
' LED(32)=Array(l33a16,l33a15,l33a14,l33a13,l33a12,l33a11,l33a10,l33a9,l33a8,l33a7,l33a6,l33a5,l33a4,l33a3,l33a2,l33a1)
' LED(33)=Array(l34a16,l34a15,l34a14,l34a13,l34a12,l34a11,l34a10,l34a9,l34a8,l34a7,l34a6,l34a5,l34a4,l34a3,l34a2,l34a1)
' LED(34)=Array(l35a16,l35a15,l35a14,l35a13,l35a12,l35a11,l35a10,l35a9,l35a8,l35a7,l35a6,l35a5,l35a4,l35a3,l35a2,l35a1)
' LED(35)=Array(l36a16,l36a15,l36a14,l36a13,l36a12,l36a11,l36a10,l36a9,l36a8,l36a7,l36a6,l36a5,l36a4,l36a3,l36a2,l36a1)
' LED(36)=Array(l37a16,l37a15,l37a14,l37a13,l37a12,l37a11,l37a10,l37a9,l37a8,l37a7,l37a6,l37a5,l37a4,l37a3,l37a2,l37a1)
' LED(37)=Array(l38a16,l38a15,l38a14,l38a13,l38a12,l38a11,l38a10,l38a9,l38a8,l38a7,l38a6,l38a5,l38a4,l38a3,l38a2,l38a1)
' LED(38)=Array(l39a16,l39a15,l39a14,l39a13,l39a12,l39a11,l39a10,l39a9,l39a8,l39a7,l39a6,l39a5,l39a4,l39a3,l39a2,l39a1)
' LED(39)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(40)=Array(l41a16,l41a15,l41a14,l41a13,l41a12,l41a11,l41a10,l41a9,l41a8,l41a7,l41a6,l41a5,l41a4,l41a3,l41a2,l41a1)
' LED(41)=Array(l42a16,l42a15,l42a14,l42a13,l42a12,l42a11,l42a10,l42a9,l42a8,l42a7,l42a6,l42a5,l42a4,l42a3,l42a2,l42a1)
' LED(42)=Array(l43a16,l43a15,l43a14,l43a13,l43a12,l43a11,l43a10,l43a9,l43a8,l43a7,l43a6,l43a5,l43a4,l43a3,l43a2,l43a1)
' LED(43)=Array(l44a16,l44a15,l44a14,l44a13,l44a12,l44a11,l44a10,l44a9,l44a8,l44a7,l44a6,l44a5,l44a4,l44a3,l44a2,l44a1)
' LED(44)=Array(l45a16,l45a15,l45a14,l45a13,l45a12,l45a11,l45a10,l45a9,l45a8,l45a7,l45a6,l45a5,l45a4,l45a3,l45a2,l45a1)
' LED(45)=Array(l46a16,l46a15,l46a14,l46a13,l46a12,l46a11,l46a10,l46a9,l46a8,l46a7,l46a6,l46a5,l46a4,l46a3,l46a2,l46a1)
' LED(46)=Array(l47a16,l47a15,l47a14,l47a13,l47a12,l47a11,l47a10,l47a9,l47a8,l47a7,l47a6,l47a5,l47a4,l47a3,l47a2,l47a1)
' LED(47)=Array(l48a16,l48a15,l48a14,l48a13,l48a12,l48a11,l48a10,l48a9,l48a8,l48a7,l48a6,l48a5,l48a4,l48a3,l48a2,l48a1)
' LED(48)=Array(l49a16,l49a15,l49a14,l49a13,l49a12,l49a11,l49a10,l49a9,l49a8,l49a7,l49a6,l49a5,l49a4,l49a3,l49a2,l49a1)
' LED(49)=Array(l50a16,l50a15,l50a14,l50a13,l50a12,l50a11,l50a10,l50a9,l50a8,l50a7,l50a6,l50a5,l50a4,l50a3,l50a2,l50a1)
' LED(50)=Array(l51a16,l51a15,l51a14,l51a13,l51a12,l51a11,l51a10,l51a9,l51a8,l51a7,l51a6,l51a5,l51a4,l51a3,l51a2,l51a1)
' LED(51)=Array(l52a16,l52a15,l52a14,l52a13,l52a12,l52a11,l52a10,l52a9,l52a8,l52a7,l52a6,l52a5,l52a4,l52a3,l52a2,l52a1)
' LED(52)=Array(l53a16,l53a15,l53a14,l53a13,l53a12,l53a11,l53a10,l53a9,l53a8,l53a7,l53a6,l53a5,l53a4,l53a3,l53a2,l53a1)
' LED(53)=Array(l54a16,l54a15,l54a14,l54a13,l54a12,l54a11,l54a10,l54a9,l54a8,l54a7,l54a6,l54a5,l54a4,l54a3,l54a2,l54a1)
' LED(54)=Array(l55a16,l55a15,l55a14,l55a13,l55a12,l55a11,l55a10,l55a9,l55a8,l55a7,l55a6,l55a5,l55a4,l55a3,l55a2,l55a1)
' LED(55)=Array(l56a16,l56a15,l56a14,l56a13,l56a12,l56a11,l56a10,l56a9,l56a8,l56a7,l56a6,l56a5,l56a4,l56a3,l56a2,l56a1)
' LED(56)=Array(l57a16,l57a15,l57a14,l57a13,l57a12,l57a11,l57a10,l57a9,l57a8,l57a7,l57a6,l57a5,l57a4,l57a3,l57a2,l57a1)
' LED(57)=Array(l58a16,l58a15,l58a14,l58a13,l58a12,l58a11,l58a10,l58a9,l58a8,l58a7,l58a6,l58a5,l58a4,l58a3,l58a2,l58a1)
' LED(58)=Array(l59a16,l59a15,l59a14,l59a13,l59a12,l59a11,l59a10,l59a9,l59a8,l59a7,l59a6,l59a5,l59a4,l59a3,l59a2,l59a1)
' LED(59)=Array(l60a16,l60a15,l60a14,l60a13,l60a12,l60a11,l60a10,l60a9,l60a8,l60a7,l60a6,l60a5,l60a4,l60a3,l60a2,l60a1)
' LED(60)=Array(l61a16,l61a15,l61a14,l61a13,l61a12,l61a11,l61a10,l61a9,l61a8,l61a7,l61a6,l61a5,l61a4,l61a3,l61a2,l61a1)
' LED(61)=Array(l62a16,l62a15,l62a14,l62a13,l62a12,l62a11,l62a10,l62a9,l62a8,l62a7,l62a6,l62a5,l62a4,l62a3,l62a2,l62a1)
' LED(62)=Array(l63a16,l63a15,l63a14,l63a13,l63a12,l63a11,l63a10,l63a9,l63a8,l63a7,l63a6,l63a5,l63a4,l63a3,l63a2,l63a1)
' LED(63)=Array(l64a16,l64a15,l64a14,l64a13,l64a12,l64a11,l64a10,l64a9,l64a8,l64a7,l64a6,l64a5,l64a4,l64a3,l64a2,l64a1)
' LED(64)=Array(l65a16,l65a15,l65a14,l65a13,l65a12,l65a11,l65a10,l65a9,l65a8,l65a7,l65a6,l65a5,l65a4,l65a3,l65a2,l65a1)
' LED(65)=Array(l66a16,l66a15,l66a14,l66a13,l66a12,l66a11,l66a10,l66a9,l66a8,l66a7,l66a6,l66a5,l66a4,l66a3,l66a2,l66a1)
' LED(66)=Array(l67a16,l67a15,l67a14,l67a13,l67a12,l67a11,l67a10,l67a9,l67a8,l67a7,l67a6,l67a5,l67a4,l67a3,l67a2,l67a1)
' LED(67)=Array(l68a16,l68a15,l68a14,l68a13,l68a12,l68a11,l68a10,l68a9,l68a8,l68a7,l68a6,l68a5,l68a4,l68a3,l68a2,l68a1)
' LED(68)=Array(l69a16,l69a15,l69a14,l69a13,l69a12,l69a11,l69a10,l69a9,l69a8,l69a7,l69a6,l69a5,l69a4,l69a3,l69a2,l69a1)
' LED(69)=Array(l70a16,l70a15,l70a14,l70a13,l70a12,l70a11,l70a10,l70a9,l70a8,l70a7,l70a6,l70a5,l70a4,l70a3,l70a2,l70a1)
' LED(70)=Array(l71a16,l71a15,l71a14,l71a13,l71a12,l71a11,l71a10,l71a9,l71a8,l71a7,l71a6,l71a5,l71a4,l71a3,l71a2,l71a1)
' LED(71)=Array(l72a16,l72a15,l72a14,l72a13,l72a12,l72a11,l72a10,l72a9,l72a8,l72a7,l72a6,l72a5,l72a4,l72a3,l72a2,l72a1)
' LED(72)=Array(l73a16,l73a15,l73a14,l73a13,l73a12,l73a11,l73a10,l73a9,l73a8,l73a7,l73a6,l73a5,l73a4,l73a3,l73a2,l73a1)
' LED(73)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(74)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(75)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(76)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(77)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(78)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(79)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(80)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(81)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(82)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(83)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(84)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(85)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(86)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(87)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(88)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(89)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(90)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(91)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(92)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(93)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(94)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(95)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(96)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(97)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(98)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(99)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(100)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(101)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(102)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(103)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(104)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(105)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(106)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(107)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(108)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(109)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(110)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(111)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(112)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(113)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(114)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(115)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(116)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(117)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(118)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(119)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(110)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(111)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(112)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(113)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(114)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(115)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(116)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(117)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(118)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(119)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(120)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(121)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(122)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(123)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(124)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(125)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(126)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)
' LED(127)=Array(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A)


Sub DisplayTimer_Timer
  If LampShader = 1 then
  Dim ChgLED,ii,jj,kk,num,chg,stat,obj
  ChgLED=Controller.ChangedLEDs(0,&Hffffffff)
  If Not IsEmpty(ChgLED) Then
    For ii=0 To UBound(ChgLED)
      num=ChgLED(ii,0):chg=ChgLED(ii,1):stat=ChgLED(ii,2)
      For jj=0 To 3
        If chg And &H000f Then
          For Each obj In LED(num*4+jj)
            obj.State=LightStateOff
          Next
          kk=15
          For Each obj In LED(num*4+jj)
            If (stat And &H000f)=kk And kk>0 Then obj.State=LightStateOn
            kk=kk-1
          Next
        End If
        chg=chg\16:stat=stat\16
      Next
    Next
  End If
'***REFLECTION LAMPS (ON STANDUPS AND BACK WALL***
l46ar1.state = l46a1.state
l46ar2.state = l46a2.state
l46ar3.state = l46a3.state
l46ar4.state = l46a4.state
l46ar5.state = l46a5.state
l46ar6.state = l46a6.state
l46ar7.state = l46a7.state
l46ar8.state = l46a8.state
l46ar9.state = l46a9.state
l46ar10.state = l46a10.state
l46ar11.state = l46a11.state
l46ar12.state = l46a12.state
l46ar13.state = l46a13.state
l46ar14.state = l46a14.state
l46ar15.state = l46a15.state

l56ar1.state = l56a1.state
l56ar2.state = l56a2.state
l56ar3.state = l56a3.state
l56ar4.state = l56a4.state
l56ar5.state = l56a5.state
l56ar6.state = l56a6.state
l56ar7.state = l56a7.state
l56ar8.state = l56a8.state
l56ar9.state = l56a9.state
l56ar10.state = l56a10.state
l56ar11.state = l56a11.state
l56ar12.state = l56a12.state
l56ar13.state = l56a13.state
l56ar14.state = l56a14.state
l56ar15.state = l56a15.state

l69ar1.state = l69a1.state
l69ar2.state = l69a2.state
l69ar3.state = l69a3.state
l69ar4.state = l69a4.state
l69ar5.state = l69a5.state
l69ar6.state = l69a6.state
l69ar7.state = l69a7.state
l69ar8.state = l69a8.state
l69ar9.state = l69a9.state
l69ar10.state = l69a10.state
l69ar11.state = l69a11.state
l69ar12.state = l69a12.state
l69ar13.state = l69a13.state
l69ar14.state = l69a14.state
l69ar15.state = l69a15.state

l70ar1.state = l70a1.state
l70ar2.state = l70a2.state
l70ar3.state = l70a3.state
l70ar4.state = l70a4.state
l70ar5.state = l70a5.state
l70ar6.state = l70a6.state
l70ar7.state = l70a7.state
l70ar8.state = l70a8.state
l70ar9.state = l70a9.state
l70ar10.state = l70a10.state
l70ar11.state = l70a11.state
l70ar12.state = l70a12.state
l70ar13.state = l70a13.state
l70ar14.state = l70a14.state
l70ar15.state = l70a15.state

l71ar1.state = l71a1.state
l71ar2.state = l71a2.state
l71ar3.state = l71a3.state
l71ar4.state = l71a4.state
l71ar5.state = l71a5.state
l71ar6.state = l71a6.state
l71ar7.state = l71a7.state
l71ar8.state = l71a8.state
l71ar9.state = l71a9.state
l71ar10.state = l71a10.state
l71ar11.state = l71a11.state
l71ar12.state = l71a12.state
l71ar13.state = l71a13.state
l71ar14.state = l71a14.state
l71ar15.state = l71a15.state

l72ar1.state = l72a1.state
l72ar2.state = l72a2.state
l72ar3.state = l72a3.state
l72ar4.state = l72a4.state
l72ar5.state = l72a5.state
l72ar6.state = l72a6.state
l72ar7.state = l72a7.state
l72ar8.state = l72a8.state
l72ar9.state = l72a9.state
l72ar10.state = l72a10.state
l72ar11.state = l72a11.state
l72ar12.state = l72a12.state
l72ar13.state = l72a13.state
l72ar14.state = l72a14.state
l72ar15.state = l72a15.state

l73ar1.state = l73a1.state
l73ar2.state = l73a2.state
l73ar3.state = l73a3.state
l73ar4.state = l73a4.state
l73ar5.state = l73a5.state
l73ar6.state = l73a6.state
l73ar7.state = l73a7.state
l73ar8.state = l73a8.state
l73ar9.state = l73a9.state
l73ar10.state = l73a10.state
l73ar11.state = l73a11.state
l73ar12.state = l73a12.state
l73ar13.state = l73a13.state
l73ar14.state = l73a14.state
l73ar15.state = l73a15.state

'End If
'If LampShader = 0 then
Else
NFadeL 5, l5a16
NFadeL 6, l6a16
NFadeL 7, l7a16
NFadeL 8, l8a16
NFadeL 9, l9a16
NFadeL 10, l10a16
NFadeL 11, l11a16
NFadeL 12, l12a16
NFadeL 13, l13a16
NFadeL 14, l14a16
NFadeL 15, l15a16
NFadeL 16, l16a16
NFadeL 17, l17a16
NFadeL 18, l18a16
NFadeL 19, l19a16
NFadeL 20, l20a16
NFadeL 21, l21a16
NFadeL 22, l22a16
NFadeL 23, l23a16
NFadeL 24, l24a16
NFadeL 25, l25a16
NFadeL 26, l26a16
NFadeL 27, l27a16
NFadeL 28, l28a16
NFadeL 29, l29a16
NFadeL 30, l30a16
NFadeL 31, l31a16
NFadeL 32, l32a16
NFadeL 33, l33a16
NFadeL 34, l34a16
NFadeL 35, l35a16
NFadeL 36, l36a16
NFadeL 37, l37a16
NFadeL 38, l38a16
NFadeL 39, l39a16
NFadeL 41, l41a16
NFadeL 42, l42a16
NFadeL 43, l43a16
NFadeL 44, l44a16
NFadeL 45, l45a16
NFadeL 46, l46a16
NFadeL 47, l47a16
NFadeL 48, l48a16
NFadeL 49, l49a16
NFadeL 50, l50a16
NFadeL 51, l51a16
NFadeL 52, l52a16
NFadeL 53, l53a16
NFadeL 54, l54a16
NFadeL 55, l55a16
NFadeL 56, l56a16
NFadeL 57, l57a16
NFadeL 58, l58a16
NFadeL 59, l59a16
NFadeL 60, l60a16
NFadeL 61, l61a16
NFadeL 62, l62a16
NFadeL 63, l63a16
NFadeL 64, l64a16
NFadeL 65, l65a16
NFadeL 66, l66a16
NFadeL 67, l67a16
NFadeL 68, l68a16
NFadeL 69, l69a16
NFadeL 70, l70a16
NFadeL 71, l71a16
NFadeL 72, l72a16
NFadeL 73, l73a16
End If
l46ar16.state = l46a16.state
l56ar16.state = l56a16.state
l69ar16.state = l69a16.state
l70ar16.state = l70a16.state
l71ar16.state = l71a16.state
l72ar16.state = l72a16.state
l73ar16.state = l73a16.state
End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Sub Pins_Hit (idx)
  PlaySound "pinhit_low", 0, Vol(ActiveBall)*VolPi, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
  PlaySound "target", 0, Vol(ActiveBall)*VolTarg, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
  PlaySound "metalhit_thin", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
  PlaySound "metalhit_medium", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
  PlaySound "metalhit2", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
  PlaySound "gate4", 0, Vol(ActiveBall)*VolGates, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
  PlaySound "fx_spinner", Spinner, VolSpin
End Sub

Sub Rubbers_Hit(idx)
  dim finalspeed
    finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
  If finalspeed > 20 then
    PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
  End if
  If finalspeed >= 6 AND finalspeed <= 20 then
    RandomSoundRubber()
  End If
End Sub

Sub Posts_Hit(idx)
  dim finalspeed
    finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
  If finalspeed > 16 then
    PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolPo, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
  End if
  If finalspeed >= 6 AND finalspeed <= 16 then
    RandomSoundRubber()
  End If
End Sub

Sub RandomSoundRubber()
  Select Case Int(Rnd*3)+1
    Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
    Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
  End Select
End Sub


Sub LRRail_Hit:PlaySound "fx_metalrolling", 0, 150, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):End Sub
Sub RLRail_Hit:PlaySound "fx_metalrolling", 0, 150, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):End Sub

' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX
' PlaySound sound, 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
' *******************************************************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position

Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
  PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Set position as table object (Use object or light but NOT wall) and Vol to 1

Sub PlaySoundAt(soundname, tableobj)
  PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed.

Sub PlaySoundAtBall(soundname)
  PlaySoundAt soundname, ActiveBall
End Sub

'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Volume)
  PlaySound sound, 1, Volume, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
  PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

'Set position as bumperX and Vol manually.

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table" is the name of the table
  Dim tmp
  tmp = tableobj.y * 2 / table.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table" is the name of the table
  Dim tmp
  tmp = tableobj.x * 2 / table.width-1
  If tmp > 0 Then
    AudioPan = Csng(tmp ^10)
  Else
    AudioPan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2 / VolDiv)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
  Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
  BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

''*****************************************
''      JP's VP10 Rolling Sounds
''*****************************************

Const tnob = 8 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingTimer_Timer()
    Dim BOT, b
    BOT = GetBalls

  ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        'rolling(b) = False
        'StopSound("fx_ballrolling" & b)
    Next

  ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

  ' play the rolling sound for each ball

    For b = 0 to UBound(BOT)
      If BallVel(BOT(b) ) > 1 Then
        'rolling(b) = True
        if BOT(b).z < 30 Then ' Ball on playfield
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.5, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
        End If
      Else
        'If rolling(b) = True Then
          'StopSound("fx_ballrolling" & b)
          'rolling(b) = False
        'End If
      End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

