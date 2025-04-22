Option Explicit
Randomize

' Thalamus 2018-09-09 : Improved directional sounds

' !! NOTE : Table not verified yet !!

Const VolDiv = 400    ' Lower number, louder ballrolling/collition sound
Const VolCol = 10      ' Ball collition divider ( voldiv/volcol )

' The rest of the values are multipliers
'
'  .5 = lower volume
' 1.5 = higher volume

Const VolBump   = 2    ' Bumpers volume.
Const VolBall   = 1    ' Ball volume.
Const VolRol    = 1    ' Rollovers volume.
Const VolGates  = 1    ' Gates volume.
Const VolMetal  = 1    ' Metals volume.
Const VolRB     = 1    ' Rubber bands volume.
Const VolRH     = .5    ' Rubber hits volume.
Const VolPo     = 1    ' Rubber posts volume.
Const VolPi     = 1    ' Rubber pins volume.
Const VolTarg   = .2    ' Targets volume.
Const VolDTarg  = 1    ' Drop Targets volume.
Const VolWood   = 1    ' Woods volume.
Const VolSpin   = 3    ' Spinners volume.
Const VolFlip   = 1    ' Flipper volume.


On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0

LoadVPM "01200001","S6.VBS",3.10

Dim bsSaucer,xx,x,FlipperShadows,bsLeft,bsRight,bsLT,bsTop,bsTrough,dtbank,dtl,dtr
FlipperShadows = 1    ' 1 turns on, 0 turns off flipper shadows.

Const UseSolenoids=2,UseLamps=1,UseSync=0,UseGI=0,HandleMech=0
Const cGameName="cntct_l1"

Const SSolenoidOn = "fx_Solenoid"
Const SSolenoidOff = ""
Const SCoin = "fx_Coin"
Const sBallRelease=1
Const sTopEject=2
Const sLeftReset=3
Const sRightReset=4
Const sLeftEject=5
Const sLeftThrower=6
Const sRightThrower=7
Const sUpPost=8
Const sSound1=9
Const sSound2=10
Const sSound3=11
Const sSound4=12
Const sSound5=13
Const sKnocker=14
Const sDownPost=15
Const sCLo=16
Const sBumper2=17'  - This solenoid is not working in vpinmame
Const sBumper1=18'  - This solenoid is not working in vpinmame
Const sBumper3=19'  - This solenoid is not working in vpinmame
Const sTarget=22'   - This solenoid is not working in vpinmame
Const sEnable=23

SolCallback(sBallRelease)="bsTrough.SolOut"
SolCallback(sTopEject)="bsTop.SolOut"
SolCallback(sLeftReset)="dtL.SolDropUp"
SolCallback(sRightReset)="dtR.SolDropUp"
SolCallback(sLeftEject)="bsLT.SolOut"
SolCallback(sLeftThrower)="bsLeft.SolOut"
SolCallback(sRightThrower)="bsRight.SolOut"
SolCallback(sUpPost)="SolPostUp"
SolCallback(sKnocker)="vpmSolSound ""Knocker"","
SolCallback(sDownPost)="SolPostDown"
SolCallback(23)="SolTaget"
SolCallback(sLLFlipper)="vpmSLLFlipper LeftFlipper,LeftFlipper1,"
SolCallback(sLRFlipper)="vpmSLRFlipper RightFlipper,RightFlipper1,"

Dim DesktopMode: DesktopMode = Table1.ShowDT
If DesktopMode = True Then 'Show Desktop components
  Ramp16.visible=1
  Ramp15.visible=1
  Ramp17.visible=1
  Primitive52.visible=1
  Primitive87.visible=1
  Primitive88.visible=1
  Primitive89.visible=1
  Primitive90.visible=1
  Primitive91.visible=1
  L61.visible=1
  L60.visible=1
  L53.visible=1
  L52.visible=1
  L50.visible=1
  L51.visible=1
  L58.visible=1
  L59.visible=1
  L57.visible=1
Else
  Ramp16.visible=0
  Ramp15.visible=0
  Ramp17.visible=0
  Primitive52.visible=0
  Primitive87.visible=0
  Primitive88.visible=0
  Primitive89.visible=0
  Primitive90.visible=0
  Primitive91.visible=0
  L61.visible=0
  L60.visible=0
  L53.visible=0
  L52.visible=0
  L50.visible=0
  L51.visible=0
  L58.visible=0
  L59.visible=0
  L57.visible=0
End if


'***********************************************************************
'GI Lights On
'***********************************************************************
Sub Encendido_timer
  playsound "encendido"
  For each xx in Ambiente:xx.State = 1: Next
    me.enabled=false
End Sub

BallSize = 50 'Ball radius


'********************************************************************
'Table Init
'********************************************************************

Sub Table1_Init
  vpmInit me
  Post.IsDropped=1
  With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Contact - Williams 1978" & vbNewLine & "VPX Table By Kalavera"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = 0
        .Games(cGameName).Settings.Value("rol") = 0 '1= rotated display, 0= normal
        '.SetDisplayPosition 0,0, GetPlayerHWnd 'restore dmd window position
        On Error Resume Next
        Controller.SolMask(0) = 0
        vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the Timer to renable all the solenoids after 2 seconds
        Controller.Run GetPlayerHWnd
        On Error Goto 0
    End With

  Set dtL=New cvpmDropTarget
  dtL.InitDrop Array(sw27,sw28),Array(27,28)
  dtL.InitSnd SoundFX("fx_droptarget",DOFDropTargets), SoundFX("fx_resetdrop",DOFContactors)

  Set dtR=New cvpmDropTarget
  dtR.InitDrop Array(sw29,sw30),Array(29,30)
  dtR.InitSnd SoundFX("fx_droptarget2",DOFDropTargets),SoundFX("fx_resetdrop2",DOFContactors)

  dtL.AllDownSw=31
  dtR.LinkedTo=Array(dtL)
  dtL.LinkedTo=Array(dtR)

    'Nudging
  vpmNudge.TiltSwitch = 1
  vpmNudge.Sensitivity = 3
  vpmNudge.TiltObj = Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)

    'Trough
  Set bsTrough = New cvpmBallStack
  With bsTrough
        .InitSw 0, 20, 0, 0, 0, 0, 0, 0
        .InitKick ballrelease, 90, 8
        .InitExitSnd SoundFX("fx_ballrel", DOFContactors), SoundFX("fx_Solenoid5", DOFContactors)
		.KickForceVar = 2
		.KIckAngleVar = 2
        .Balls = 1
    End With

    'Saucer1
  Set bstop = New cvpmBallStack
  With bstop
    .InitSaucer sw10, 10, 205, 12
	.KickForceVar = 2
	.KIckAngleVar = 2
    .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_Solenoid", DOFContactors)
  End With

    'Saucer2
  Set bsLT = New cvpmBallStack
  With bsLT
    .InitSaucer sw26, 26, 140, 12
	.KickForceVar = 2
	.KIckAngleVar = 2
    .InitExitSnd SoundFX("fx_kicker2", DOFContactors), SoundFX("fx_Solenoid2", DOFContactors)
  End With

    'Saucer3
  Set bsLeft = New cvpmBallStack
  With bsLeft
    .InitSaucer sw21, 21, 148, 60
	.KickForceVar = 2
	.KIckAngleVar = 2
    .InitExitSnd SoundFX("fx_kicker3", DOFContactors), SoundFX("fx_Solenoid3", DOFContactors)
  End With

    'Saucer4
  Set bsRight = New cvpmBallStack
  With bsRight
    .InitSaucer sw19, 19, 298, 60
	.KickForceVar = 2
	.KIckAngleVar = 2
    .InitExitSnd SoundFX("fx_kicker4", DOFContactors), SoundFX("fx_Solenoid4", DOFContactors)
  End With

    'Drop targets
  set dtBank = new cvpmdroptarget
  With dtBank
    .InitDrop Array(sw27, sw28, sw29, sw30), Array(27, 28, 29, 30)
    .initsnd SoundFX("", DOFDropTargets), SoundFX("fx_resetdrop", DOFContactors)
  End With

    'Main Timer init
  PinMAMETimer.Interval = PinMAMEInterval
  PinMAMETimer.Enabled = 1

End Sub

'Pause
Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub
Sub Table1_exit:Controller.stop:End Sub

'*******************************************************************
' Keys
'*******************************************************************

Sub table1_KeyDown(ByVal Keycode)
  If keycode = StartGameKey Then Controller.Switch(3)=1
    If keycode = LeftTiltKey Then Nudge 90, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 5:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0, 0.25
    If keycode = PlungerKey Then PlaySoundAtVol "fx_PlungerPull", Plunger, 1:Plunger.Pullback
    If vpmKeyDown(keycode)Then Exit Sub
End Sub

Sub table1_KeyUp(ByVal Keycode)
    If keycode = PlungerKey Then PlaySoundAtVol "fx_plunger", Plunger, 1:Plunger.Fire
    If vpmKeyUp(keycode)Then Exit Sub
End Sub

Sub SolPostUp(Enabled)
    If Enabled Then
    playsoundAtVol SoundFX("fx_solenoidon",DOFContactors), Posta, 20
    Posta.Transz = 25
        Post.IsDropped=0
    lp1.state = 1    
  End If
End Sub

Sub SolPostDown(Enabled)
    If Enabled Then
    playsoundAtVol SoundFX("fx_solenoidoff",DOFContactors), Posta, 20
    Posta.Transz = 0
        Post.IsDropped=1
    lp1.state = 0    
  End If
End Sub

'****************************************************************
'Scores
'****************************************************************

' Drain hole
Sub Drain_Hit:bsTrough.addball me:playsoundAtVol "drain", Drain, 1:End Sub

' Saucers
Sub sw10_Hit:bstop.AddBall 0 : playsoundAtVol "popper_ball", sw10, 1: End Sub
Sub sw19_Hit:bsRight.AddBall 0 : playsoundAtVol "popper_ball", sw19, 1: End Sub
Sub sw21_Hit:bsLeft.AddBall 0 : playsoundAtVol "popper_ball", sw21, 1: End Sub
Sub sw26_Hit:bsLT.AddBall 0 : playsoundAtVol "popper_ball", sw26, 1: End Sub

' Moving Target
Sub sw5a_Hit:vpmTimer.PulseSw 37:PlaySoundAtVol SoundFX("fx_target", DOFDropTargets), sw5p, VolTarg:sw5p.Y = 801:vpmtimer.addtimer 200, "sw5p.Y = 805'":End Sub
Sub sw5b_Hit:vpmTimer.PulseSw 37:PlaySoundAtVol SoundFX("fx_target", DOFDropTargets), sw5p, VolTarg:sw5p.Y = 801:vpmtimer.addtimer 200, "sw5p.Y = 805'":End Sub
Sub sw5c_Hit:vpmTimer.PulseSw 37:PlaySoundAtVol SoundFX("fx_target", DOFDropTargets), sw5p, VolTarg:sw5p.Y = 801:vpmtimer.addtimer 200, "sw5p.Y = 805'":End Sub

' Scoring rubbers
Sub sw25_Hit:vpmTimer.PulseSw 25:PlaySoundAtVol "fx_Rubber2", ActiveBall, VolRB:vpmTimer.PulseSw 12:End Sub
Sub sw31_Hit:vpmTimer.PulseSw 31:PlaySoundAtVol "fx_Rubber2", ActiveBall, VolRB:vpmTimer.PulseSw 12:End Sub
Sub sw14_Hit:vpmTimer.PulseSw 14:PlaySoundAtVol "fx_Rubber2", ActiveBall, VoLRB:vpmTimer.PulseSw 12:End Sub
Sub sw16_Hit:vpmTimer.PulseSw 16:PlaySoundAtVol "fx_Rubber2", ActiveBall, VolRB:vpmTimer.PulseSw 12:End Sub

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 34:PlaySoundAtVol SoundFX("fx_bumper1", DOFContactors), Bumper1, VolBump:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 33:PlaySoundAtVol SoundFX("fx_bumper2", DOFContactors), Bumper2, VolBump:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 35:PlaySoundAtVol SoundFX("fx_bumper3", DOFContactors), Bumper3, VolBump:End Sub

' Targets
Sub sw36_Hit:vpmTimer.PulseSw 36:PlaySoundAtVol SoundFX("fx_target", DOFDropTargets), sw36, VolTarg:End Sub
Sub sw38_Hit:vpmTimer.PulseSw 38:PlaySoundAtVol SoundFX("fx_target", DOFDropTargets), sw38, VolTarg:End Sub
Sub sw13_Hit:vpmTimer.PulseSw 13:PlaySoundAtVol SoundFX("fx_target", DOFDropTargets), sw13, VolTarg:End Sub
Sub sw15_Hit:vpmTimer.PulseSw 15:PlaySoundAtVol SoundFX("fx_target", DOFDropTargets), sw15, VolTarg:End Sub

' Droptargets
Sub sw27_Hit:PlaySoundAtVol SoundFX("fx_droptarget", DOFDropTargets), sw27, VolDTarg:End Sub
Sub sw28_Hit:PlaySoundAtVol SoundFX("fx_droptarget", DOFDropTargets), sw28, VolDTarg:End Sub
Sub sw29_Hit:PlaySoundAtVol SoundFX("fx_droptarget", DOFDropTargets), sw29, VolDTarg:End Sub
Sub sw30_Hit:PlaySoundAtVol SoundFX("fx_droptarget", DOFDropTargets), sw30, VolDTarg:End Sub

Sub sw27_Dropped:dtbank.Hit 1:End Sub
Sub sw28_Dropped:dtbank.Hit 2:End Sub
Sub sw29_Dropped:dtbank.Hit 3:End Sub
Sub sw30_Dropped:dtbank.Hit 3:End Sub

' Rollovers
Sub sw17_Hit:Controller.Switch(17) = 1:PlaySoundAtVol "fx_sensor", ActiveBall, VolRol:End Sub
Sub sw17_UnHit:Controller.Switch(17) = 0:End Sub

Sub sw32_Hit:Controller.Switch(32) = 1:PlaySoundAtVol "fx_sensor", ActiveBall, VolRol:End Sub
Sub sw32_UnHit:Controller.Switch(32) = 0:End Sub

Sub sw23_Hit:Controller.Switch(23) = 1:PlaySoundAtVol "fx_sensor", ActiveBall, VolRol:End Sub
Sub sw23_UnHit:Controller.Switch(23) = 0:End Sub

Sub sw9_Hit:Controller.Switch(9) = 1:PlaySoundAtVol "fx_sensor", ActiveBall, VolRol:End Sub
Sub sw9_UnHit:Controller.Switch(9) = 0:End Sub

Sub sw11_Hit:Controller.Switch(11) = 1:PlaySoundAtVol "fx_sensor", ActiveBall, VolRol:End Sub
Sub sw11_UnHit:Controller.Switch(11) = 0:End Sub

Sub sw12_Hit:Controller.Switch(12) = 1:PlaySoundAtVol "fx_sensor", ActiveBall, VolRol:End Sub
Sub sw12_UnHit:Controller.Switch(12) = 0:End Sub

'***********************
' Swing Target animation
'***********************

Sub SolTaget(Enabled)
  If Enabled Then
PlaySoundAtVol "motor_long", sw5p, 0.003
    SwingTimer.enabled = 1
motorsound.enabled = 1
  Else
    SwingTimer.enabled = 0
motorsound.enabled = 0
  End If
End Sub

Dim MyPi, SwingStep, SwingPos
MyPi = Round(4 * Atn(1), 6) / 90
SwingStep = 0

Sub SwingTimer_Timer()
    If Controller.Lamp(55)Then
        SwingPos = SIN(SwingStep * MyPi) * 15
        SwingStep = (SwingStep + 1)MOD 360
        sw5p.Roty = SwingPos
        If SwingPos < -10 Then
            sw5a.Isdropped = 0:sw5b.IsDropped = 1
        ElseIF SwingPos < 10 Then
            sw5b.Isdropped = 0:sw5a.IsDropped = 1:sw5c.IsDropped = 1
        Else
            sw5c.Isdropped = 0:sw5b.IsDropped = 1
        End If
  End If
End Sub

Sub motorsound_timer()
PlaySoundAtVol "motor_long", sw5p, 0.003
End Sub


'***********************************************************************
'Lights
'***********************************************************************

Set Lights(1)=l1
Set Lights(2)=L2
Set Lights(3)=L3
Set Lights(4)=L4
Set Lights(5)=L5
Set Lights(6)=L6
Set Lights(7)=L7
Set Lights(8)=L8
Set Lights(9)=L9
Set Lights(10)=L10
Set Lights(11)=L11
Set Lights(12)=L12
Set Lights(13)=L13
Set Lights(14)=L14
Set Lights(15)=L15
Set Lights(16)=L16
Set Lights(17)=L17
Set Lights(18)=L18
Set Lights(19)=L19
Set Lights(20)=L20
Set Lights(21)=L21
Set Lights(22)=L22
Set Lights(23)=L23
Set Lights(24)=L24
Set Lights(25)=L25
Set Lights(26)=L26
Set Lights(27)=L27
Set Lights(28)=L28
Set Lights(29)=L29
Set Lights(30)=L30
Set Lights(31)=L31
Set Lights(32)=L32
Set Lights(33)=L33
Set Lights(34)=L34
Set Lights(35)=L35
Set Lights(36)=L36
Set Lights(37)=L37
Lights(38)=Array(L38,L38b)
Lights(39)=Array(L39,L39b)
Lights(40)=Array(L40,L40b)
Set Lights(41)=L41
Set Lights(42)=L42
Set Lights(43)=L43
Set Lights(44)=L44
Set Lights(45)=L45
Set Lights(46)=L46
Set Lights(48)=L48
'Lights(49)=Array(L49,Lp1,Lp2,Lp3)
Set Lights(50)=L50    'Light1CanPlay(BACKGLASS)
Set Lights(51)=L51    'Light2CanPlay(BACKGLASS)
Set Lights(52)=L52    'Light3CanPlay(BACKGLASS)
Set Lights(53)=L53    'Light4CanPlay(BACKGLASS)
'Set Lights(54)=L54   MATCH   (BACKGLASS)
'Set Lights(55)=L55   'BALL IN PLAY (BACKGLASS)
Set Lights(56)=L56    'CREDIT     (APRON)
Set Lights(57)=L57    'PLAYER 1 UP  (BACKGLASS)
Set Lights(58)=L58    'PLAYER 2 UP  (BACKGLASS)
Set Lights(59)=L59    'PLAYER 3 UP  (BACKGLASS)
Set Lights(60)=L60    'PLAYER 4 UP  (BACKGLASS)
Set Lights(61)=L61    'TILT     (BACKGLASS)
'Set Lights(62)=L62   GAME OVER   (BACKGLASS)
'Set Lights(63)=L63   EXTRA BALL  (BACKGLASS)
'Set Lights(64)=L64   HIGH SCORE  (BACKGLASS)


'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************

Dim Digits(28)
' 1st Player
Digits(0) = Array(LED10,LED11,LED12,LED13,LED14,LED15,LED16)
Digits(1) = Array(LED20,LED21,LED22,LED23,LED24,LED25,LED26)
Digits(2) = Array(LED30,LED31,LED32,LED33,LED34,LED35,LED36)
Digits(3) = Array(LED40,LED41,LED42,LED43,LED44,LED45,LED46)
Digits(4) = Array(LED50,LED51,LED52,LED53,LED54,LED55,LED56)
Digits(5) = Array(LED60,LED61,LED62,LED63,LED64,LED65,LED66)

' 2nd Player
Digits(6) = Array(LED80,LED81,LED82,LED83,LED84,LED85,LED86)
Digits(7) = Array(LED90,LED91,LED92,LED93,LED94,LED95,LED96)
Digits(8) = Array(LED100,LED101,LED102,LED103,LED104,LED105,LED106)
Digits(9) = Array(LED110,LED111,LED112,LED113,LED114,LED115,LED116)
Digits(10) = Array(LED120,LED121,LED122,LED123,LED124,LED125,LED126)
Digits(11) = Array(LED130,LED131,LED132,LED133,LED134,LED135,LED136)

' 3rd Player
Digits(12) = Array(LED150,LED151,LED152,LED153,LED154,LED155,LED156)
Digits(13) = Array(LED160,LED161,LED162,LED163,LED164,LED165,LED166)
Digits(14) = Array(LED170,LED171,LED172,LED173,LED174,LED175,LED176)
Digits(15) = Array(LED180,LED181,LED182,LED183,LED184,LED185,LED186)
Digits(16) = Array(LED190,LED191,LED192,LED193,LED194,LED195,LED196)
Digits(17) = Array(LED200,LED201,LED202,LED203,LED204,LED205,LED206)

' 4th Player
Digits(18) = Array(LED220,LED221,LED222,LED223,LED224,LED225,LED226)
Digits(19) = Array(LED230,LED231,LED232,LED233,LED234,LED235,LED236)
Digits(20) = Array(LED240,LED241,LED242,LED243,LED244,LED245,LED246)
Digits(21) = Array(LED250,LED251,LED252,LED253,LED254,LED255,LED256)
Digits(22) = Array(LED260,LED261,LED262,LED263,LED264,LED265,LED266)
Digits(23) = Array(LED270,LED271,LED272,LED273,LED274,LED275,LED276)

' Credits
Digits(24) = Array(LED4,LED2,LED6,LED7,LED5,LED1,LED3)
Digits(25) = Array(LED18,LED9,LED27,LED28,LED19,LED8,LED17)
' Balls
Digits(26) = Array(LED39,LED37,LED48,LED49,LED47,LED29,LED38)
Digits(27) = Array(LED67,LED58,LED69,LED77,LED68,LED57,LED59)

Sub DisplayTimer_Timer
  Dim ChgLED,ii,num,chg,stat,obj
  ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
If Not IsEmpty(ChgLED) Then
    If DesktopMode = True Then
    For ii = 0 To UBound(chgLED)
      num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
      if (num < 28) then
        For Each obj In Digits(num)
          If chg And 1 Then obj.State = stat And 1
          chg = chg\2 : stat = stat\2
        Next
      else
      end if
    next
    end if
end if
End Sub

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
  PlaySoundAtVol SoundFX("right_slingshot",DOFContactors), sling1, 1
  RSling.Visible = 0
  RSling1.Visible = 1
  sling1.rotx = 20
  RStep = 0
  vpmTimer.PulseSw 18
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
  Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
  PlaySoundAtVol SoundFX("left_slingshot",DOFContactors), sling2, 1
  LSling.Visible = 0
  LSling1.Visible = 1
  sling2.rotx = 20
  LStep = 0
  vpmTimer.PulseSw 22
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
  Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX
' PlaySoundAtVol sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
' *******************************************************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
  PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed.

Sub PlaySoundAtBall(soundname)
  PlaySoundAt soundname, ActiveBall
End Sub

'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, Pan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
  PlaySound sound, 0, Vol(ActiveBall) * VolMult, Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

'Set position as bumperX and Vol manually.

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, Pan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function RndNum(min,max)
 RndNum = Int(Rnd()*(max-min+1))+min     ' Sets a random number between min and max
End Function

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
  tmp = tableobj.y * 2 / table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
  Dim tmp
  tmp = tableobj.x * 2 / table1.width-1
  If tmp > 0 Then
    AudioPan = Csng(tmp ^10)
  Else
    AudioPan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioFade(ball) ' Can this be together with the above function ?
  Dim tmp
  tmp = ball.y * 2 / Table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
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

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 2 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

'Sub RollingUpdate()
Sub RollingTimer_Timer()
   Dim BOT, b, ballpitch
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

    ' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
      If BallVel(BOT(b) ) > 1 Then
        rolling(b) = True
        if BOT(b).z < 30 Then ' Ball on playfield
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*VolBall, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*(VolBall*.5), Pan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
        End If
      Else
        If rolling(b) = True Then
          StopSound("fx_ballrolling" & b)
          rolling(b) = False
        End If
      End If
    Next
End Sub


'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'*****************************************
' ninuzzu's FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
  FlipperLSh.RotZ = LeftFlipper.currentangle
  FlipperRSh.RotZ = RightFlipper.currentangle
  FlipperLSh1.RotZ = LeftFlipper1.currentangle
  FlipperRSh1.RotZ = RightFlipper1.currentangle

End Sub

'*****************************************
' ninuzzu's BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

Sub BallShadowUpdate_timer()
    Dim BOT, b
    BOT = GetBalls
    ' hide shadow of deleted balls
    If UBound(BOT)<(tnob-1) Then
        For b = (UBound(BOT) + 1) to (tnob-1)
            BallShadow(b).visible = 0
        Next
    End If
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow for each ball
    For b = 0 to UBound(BOT)
		BallShadow(b).X = BOT(b).X
		ballShadow(b).Y = BOT(b).Y + 10                       
        If BOT(b).Z > 20 and BOT(b).Z < 200 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub


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
  PlaySound "fx_gate", 0, Vol(ActiveBall)*VolGates, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Wood_Hit (idx)
  PlaySound "fx_woodhit", 0, Vol(ActiveBall)*VolWood, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
  PlaySoundAtVol "fx_spinner", Spinner, VolSpin
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
    Case 1 : PlaySoundAtBallVol "rubber_hit_1",VolRH*3
    Case 2 : PlaySoundAtBallVol "rubber_hit_2",VolRH*3
    Case 3 : PlaySoundAtBallVol "rubber_hit_3",VolRH*3
  End Select
End Sub

'**************
' Flipper Subs
'**************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAtVol SoundFX("fx_flipperup", DOFFlippers), LeftFlipper, VolFlip
        LeftFlipper.RotateToEnd
        LeftFlipper1.RotateToEnd
    Else
        PlaySoundAtVol SoundFX("fx_flipperdown", DOFFlippers), LeftFlipper, VolFlip
        LeftFlipper.RotateToStart
        LeftFlipper1.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAtVol SoundFX("fx_flipperup", DOFFlippers), RightFlipper, VolFlip
        RightFlipper.RotateToEnd
        RightFlipper1.RotateToEnd
    Else
        PlaySoundAtVol SoundFX("fx_flipperdown", DOFFlippers), RightFlipper, VolFlip
        RightFlipper.RotateToStart
        RightFlipper1.RotateToStart
    End If
End Sub

Sub LeftFlipper_Collide(parm)
	RandomSoundFlipper
    'PlaySound "fx_rubber_flipper", 0, parm / 10, -0.1, 0.25
End Sub

Sub LeftFlipper1_Collide(parm)
	RandomSoundFlipper
    'PlaySound "fx_rubber_flipper", 0, parm / 10, -0.1, 0.25
End Sub

Sub RightFlipper_Collide(parm)
	RandomSoundFlipper
    'PlaySound "fx_rubber_flipper", 0, parm / 10, 0.1, 0.25
End Sub

Sub RightFlipper1_Collide(parm)
	RandomSoundFlipper
    'PlaySound "fx_rubber_flipper", 0, parm / 10, 0.1, 0.25
End Sub

Sub RandomSoundFlipper()
  Select Case RndNum(1,3)
    Case 1 : PlaySoundAtBallVol "flip_hit_1", VolFlip*10
    Case 2 : PlaySoundAtBallVol "flip_hit_2", VolFlip*10
    Case 3 : PlaySoundAtBallVol "flip_hit_3", VolFlip*10
  End Select
End Sub

