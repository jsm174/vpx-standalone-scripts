'
Option Explicit
Randomize
 
  On Error Resume Next
  ExecuteGlobal GetTextFile("Controller.vbs")
  If Err Then MsgBox "Unable to open Controller.vbs. Ensure that it is in the Scripts folder of Visual Pinball."
  On Error Goto 0
 
' Options
' Volume devided by - lower gets higher sound
 
Const VolDiv = 800
 
Dim dtxx
Dim DesktopMode:DesktopMode = Table1.ShowDT
 
' Const cSingleLFlip = 0
' Const cSingleRFlip = 0
 
If DesktopMode = True Then
  Ramp15.visible = 1
  Ramp16.visible = 1
  SideWood.visible = 1
else
  Ramp15.visible = 0
  Ramp16.visible = 0
  SideWood.visible = 0
End If
 
Dim VarHidden, UseVPMDMD
If Table1.ShowDT = true then
    UseVPMDMD = true
    VarHidden = 1
else
    UseVPMDMD = False
    VarHidden = 0
end if
 
LoadVPM "01560000", "WPC.VBS", 3.26
 
' Thalamus - for Fast Flip v2
NoUpperRightFlipper
NoUpperLeftFlipper
 
'********************
'Standard definitions
'********************
 
Const UseSolenoids = 2
Const UseLamps = 1
Const UseSync = 0
Const HandleMech = 0
 
' Standard Sounds
Const SSolenoidOn = "Solenoid"
Const SSolenoidOff = ""
Const SFlipperOn = "FlipperUp"
Const SFlipperOff = "FlipperDown"
Const SCoin = "Coin"
Const MusicOn = true ' False if you don't want music

 
Set GiCallback2 = GetRef("UpdateGI2")
 
Dim bsTrough, bsCryptPopper, bsBLPopper, bsCastlePopper, bsCoffinPopper
Dim mMagnet, dtLDrop, bsCastleLock, x, bumper1, bumper2, bumper3, plungerIM
 
 
On Error Resume Next
Dim i
For i=0 To 127
    If IsObject(eval("L" & i)) Then
    Execute "Set Lights(" & i & ")  = L" & i
    End If
Next
 
Lights(58)=Array(L58,L58A)
 
'************
' Table init.
'************
 
Const cGameName = "drac_l1"
 
Sub Table1_Init
    vpmInit Me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Bram Stoker's Dracula, Williams, 1993" & vbNewLine & "VP91x table by JPSalas v1.03"
        .Games(cGameName).Settings.Value("rol") = 0 'rotated left
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = 0   'Desktopmode

        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
        .Switch(22) = 1 'close coin door
        .Switch(24) = 1 'and keep it close
    End With
 
    ' Nudging
    vpmNudge.TiltSwitch = 14
    vpmNudge.Sensitivity = 1
    vpmNudge.TiltObj = Array(bumper1, bumper2, bumper3, LeftSlingshot, RightSlingshot)
 
    ' Trough
    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 48, 41, 42, 43, 44, 0, 0, 0
        .InitKick BallRelease, 90, 4
        .InitEntrySnd "Solenoid", "Solenoid"
        .InitExitSnd SoundFX("ballrel",DOFContactors), SoundFX("Solenoid",DOFContactors)
        .Balls = 4
    End With
 
    ' Crypt Popper
    set bsCryptPopper = new cvpmBallStack
    With bsCryptPopper
        .InitSw 0, 56, 0, 0, 0, 0, 0, 0
        .InitKick sw56, 80, 10
        .KickForceVar = 1
        .KickAngleVar = 5
        .KickBalls = 2
        .InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
        .Balls = 0
    End With
 
    ' Wire Ramp Popper
    set bsBLPopper = new cvpmBallStack
    With bsBLPopper
        .InitSw 0, 55, 0, 0, 0, 0, 0, 0
        .InitKick sw55, 180, 1
        .InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
        .Balls = 0
    End With
 
    ' Castle Popper
    set bsCastlePopper = new cvpmBallStack
    With bsCastlePopper
        .InitSw 0, 71, 0, 0, 0, 0, 0, 0
        .InitKick sw71, 210, 10
        .KickForceVar = 1
        .KickAngleVar = 5
        .KickBalls = 2
        .InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
    End With
 
    ' Coffin Popper
    set bsCoffinPopper = new cvpmBallStack
    With bsCoffinPopper
        .InitSw 0, 72, 0, 0, 0, 0, 0, 0
        .InitKick sw72, 180, 1
        .InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
    End With
 
    ' Mist Magnet
    Set mMagnet = New cMagnet
    mMagnet.InitMagnet Magnet, 5
    mMagnet.Size = 60
    MagnetPos = 0:SetMagnetPosition
 
    ' Drop target
     Set dtLDrop = New cvpmDropTarget
    With dtLDrop
        .InitDrop sw15, 15
        .InitSnd SoundFX("droptarget_l",DOFDropTargets), SoundFX("Solenoid",DOFContactors)
        .CreateEvents "dtLDrop"
    End With
 
    ' Castle Lock
    Set bsCastleLock = new cvpmBallStack
    With bsCastleLock
        .initsw 0, 53, 54, 57, 0, 0, 0, 0
        .InitKick CastleLock, 135, 1
    End With
 
 
    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1
 
Plunger.Pullback
Wdivert.Isdropped = 1
 
If MusicOn Then PLaySound "vampirella", -1  
End Sub
 
Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub


'**********
' Keys
'**********
 
Sub table1_KeyDown(ByVal Keycode)
    'If keycode = 3 Then SetFlash 131,1
    If keycode = PlungerKey Then Controller.Switch(34) = 1
        If keycode = LeftTiltKey Then
        Nudge 90, 2
    End If
 
    If keycode = RightTiltKey Then
        Nudge 270, 2
    End If
 
    If keycode = CenterTiltKey Then
        Nudge 0, 2
    End If
    If vpmKeyDown(keycode) Then Exit Sub
End Sub
 
Sub table1_KeyUp(ByVal Keycode)
    'If keycode = 3 Then SetFlash 131,0
    If keycode = PlungerKey Then Controller.Switch(34) = 0
    If vpmKeyUp(keycode) Then Exit Sub
End Sub
 
 
'*********
' Switches
'*********
 
' Slings & div switches
Dim LStep, RStep
 
Sub LeftSlingShot_Slingshot
PlaySound SoundFX("slingshot",DOFContactors), 0, 0.3, -0.1, 0.25, 0, 1, AudioFade(sling2)
vpmTimer.PulseSw 64
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    LeftSlingShot.TimerInterval = 10
End Sub
 
Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub
 
Sub RightSlingShot_Slingshot
PlaySound SoundFX("slingshot",DOFContactors), 0, 0.3, 0.1, 0.25, 0, 1, AudioFade(sling1)
vpmTimer.PulseSw 65
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    RightSlingShot.TimerInterval = 10
End Sub
 
Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub
 
 
' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 61:PlaySoundAtVol SoundFX("bumper",DOFContactors), ActiveBall, 1:End Sub
' Sub Bumper1_Hit:vpmTimer.PulseSw 61:PlaySound "bumper", 0, 0.1, 0, 0.25, 0, 1, AudioFade(ActiveBall):End Sub
Sub Bumper1_Timer()
    Select Case bump1
        Case 1:Ring1a.IsDropped = 0:bump1 = 2
        Case 2:Ring1b.IsDropped = 0:Ring1a.IsDropped = 1:bump1 = 3
        Case 3:Ring1c.IsDropped = 0:Ring1b.IsDropped = 1:bump1 = 4
        Case 4:Ring1c.IsDropped = 1:Me.TimerEnabled = 0
    End Select
End Sub
 
Sub Bumper2_Hit:vpmTimer.PulseSw 62:PlaySoundAtVol SoundFX("bumper",DOFContactors), ActiveBall, 1:End Sub
' Sub Bumper2_Hit:vpmTimer.PulseSw 62:PlaySound "bumper", 0, 0.1, -0.1, 0.25, 0, 1, AudioFade(ActiveBall):End Sub
Sub Bumper2_Timer()
    Select Case bump2
        Case 1:Ring2a.IsDropped = 0:bump2 = 2
        Case 2:Ring2b.IsDropped = 0:Ring2a.IsDropped = 1:bump2 = 3
        Case 3:Ring2c.IsDropped = 0:Ring2b.IsDropped = 1:bump2 = 4
        Case 4:Ring2c.IsDropped = 1:Me.TimerEnabled = 0
    End Select
End Sub
 
Sub Bumper3_Hit:vpmTimer.PulseSw 63:PlaySoundAtVol SoundFX("bumper",DOFContactors), ActiveBall, 1:End Sub
' Sub Bumper3_Hit:vpmTimer.PulseSw 63:PlaySound "bumper", 0, 0.1, 0.1, 0.25, 0, 1, AudioFade(ActiveBall):End Sub
Sub Bumper3_Timer()
    Select Case bump3
        Case 1:Ring3a.IsDropped = 0:bump3 = 2
        Case 2:Ring3b.IsDropped = 0:Ring3a.IsDropped = 1:bump3 = 3
        Case 3:Ring3c.IsDropped = 0:Ring3b.IsDropped = 1:bump3 = 4
        Case 4:Ring3c.IsDropped = 1:Me.TimerEnabled = 0
    End Select
End Sub
 
' Drain holes, vuks & saucers
Sub Drain_Hit:PlaysoundAt "drain",Drain:bsTrough.AddBall Me:End Sub
Sub Drain1_Hit:PlaysoundAt "drain",Drain1:ClearBallID:bsTrough.AddBall Me:End Sub
Sub Drain2_Hit:PlaysoundAt "drain",Drain2:ClearBallID:bsTrough.AddBall Me:End Sub
Sub Drain3_Hit:PlaysoundAt "drain",Drain3:ClearBallID:bsTrough.AddBall Me:End Sub
Sub Drain4_Hit:PlaysoundAt "drain",Drain4:ClearBallID:bsTrough.AddBall Me:End Sub
 
'Sub sw72a_Hit:PlaySound "hole_enter", 0, 0.3, 0.1, 0.25:bsCoffinPopper.addball Me:End Sub
 
Dim rball
 
Sub sw72a_Hit:PlaySound "hole_enter", 0, 0.3, 0.1, 0.25, 0, 1, AudioFade(sw72a):me.destroyball:set rball = me.createball:drop.enabled = 1:End Sub
 
Sub drop_timer()
If rball.Z <= -50 Then
me.enabled = 0
drop2.enabled = 1
End If
rball.Z = rball.Z - 1
End Sub
 
Sub drop2_timer()
sw72a.destroyball
bsCoffinPopper.addball rball
me.enabled = 0
End Sub
 
Sub sw71_Hit
    PlaySound "hole_enter", 0, 0.3, 0.1, 0.25, 0, 1, AudioFade(sw71)
    'ClearBallID
    vpmTimer.PulseSwitch 71, 0, 0
    mMagnet.RemoveBall ActiveBall
    Me.destroyball
    bsCastlePopper.AddBall Me
End Sub
 
Sub sw58_Hit
    PlaySound "hole_enter", 0, 0.3, -0.1, 0.25, 0, 1, AudioFade(sw58)
    'ClearBallID
    Me.DestroyBall
    PlaySoundAt "subway2", sw58
    vpmTimer.PulseSwitch 58, 1250, "bsBLPopper.AddBall 0 '"
End Sub
 
Sub sw56_Hit
    PlaySound "hole_enter", 0, 0.3, -0.1, 0.25, 0, 1, AudioFade(sw56)
    'ClearBallID
    vpmTimer.PulseSwitch 56, 100, 0
    mMagnet.RemoveBall ActiveBall
    bsCryptPopper.AddBall Me
End Sub
 
Sub sw56a_Hit
    PlaySound "hole_enter", 0, 0.3, -0.1, 0.25, 0, 1, AudioFade(sw56a)
    'ClearBallID
    mMagnet.RemoveBall ActiveBall
    bsCryptPopper.AddBall Me
End Sub
 
Sub CastleLock_Hit()
    PlaysoundAt "metalhit2", CastleLock
    'ClearBallID
    bsCastleLock.AddBall Me
End Sub
 
' Rollovers & Ramp Switches
Sub sw35_Hit:Controller.Switch(35) = 1:PlaySoundAt "sensor",sw35:End Sub
Sub sw35_UnHit:Controller.Switch(35) = 0:PlaySoundAt "outlane",sw35:End Sub
 
Sub sw36_Hit:Controller.Switch(36) = 1:PlaySoundAt "sensor",sw36:End Sub
Sub sw36_UnHit:Controller.Switch(36) = 0:End Sub
 
Sub sw37_Hit:Controller.Switch(37) = 1:PlaySoundAt "sensor",sw37:End Sub
Sub sw37_UnHit:Controller.Switch(37) = 0:End Sub
 
Sub sw38_Hit:Controller.Switch(38) = 1:PlaySoundAt "sensor",sw38:End Sub
Sub sw38_UnHit:Controller.Switch(38) = 0:PlaySoundAt "outlane",sw38:End Sub
 
Sub sw25_Hit:Controller.Switch(25) = 1:PlaySoundAt "sensor",sw25:End Sub
Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub
 
Sub sw26_Hit:Controller.Switch(26) = 1:PlaySoundAt "sensor",sw26:End Sub
Sub sw26_Unhit:Controller.Switch(26) = 0:End Sub
 
Sub sw27_Hit:Controller.Switch(27) = 1:PlaySoundAt "sensor",sw27:End Sub
Sub sw27_Unhit:Controller.Switch(27) = 0:End Sub
 
Sub sw16_Hit:Controller.Switch(16) = 1:PlaySoundAt "sensor",sw16:End Sub
Sub sw16_Unhit:Controller.Switch(16) = 0:End Sub
 
Sub sw28_Hit:Controller.Switch(28) = 1:PlaySoundAt "gate",sw28:End Sub
Sub sw28_Unhit:Controller.Switch(28) = 0:PlaySound "metalrolling", 0, 0.3, -0.1, 0.25, 0, 1, AudioFade(sw28):End Sub
 
Sub sw84_Hit:Controller.Switch(84) = 1:PlaySoundAt "gate",sw84:End Sub
Sub sw84_Unhit:Controller.Switch(84) = 0:PlaySoundAt "metalrolling",sw84:End Sub
 
Sub sw85_Hit:Controller.Switch(85) = 1:PlaySoundAt "gate",sw85:End Sub
Sub sw85_Unhit:Controller.Switch(85) = 0:PlaySoundAt "metalrolling",sw85:End Sub
 
Sub sw31_Hit:Controller.Switch(31) = 1:PlaySoundAt "sensor",sw31:End Sub
Sub sw31_Unhit:Controller.Switch(31) = 0:End Sub
 
Sub sw51_Hit:Controller.Switch(51) = 1:PlaySoundAt "sensor",sw51:End Sub
Sub sw51_Unhit:Controller.Switch(51) = 0:End Sub
 
Sub sw52_Hit:Controller.Switch(52) = 1:PlaySoundAt "sensor",sw52:End Sub
Sub sw52_Unhit:Controller.Switch(52) = 0:End Sub
 
Sub sw73_Hit
  Controller.Switch(73) = 1
  If ActiveBall.VelY < -25 Then
    PlaySoundAt "Subway2",sw73
  End If
End Sub
 
Sub sw73_Unhit:Controller.Switch(73) = 0:End Sub
 
Sub sw17_Hit:Controller.Switch(17) = 1:End Sub
Sub sw17_Unhit:Controller.Switch(17) = 0:End Sub
 
' Targets
Sub sw66_Hit:vpmTimer.PulseSw 66:PlaySoundAt SoundFX("target",DOFTargets),sw66:End Sub
Sub sw66_Timer:sw66.IsDropped = 0:End Sub
 
Sub sw67_Hit:vpmTimer.PulseSw 67:PlaySoundAt SoundFX("target",DOFTargets),sw67:End Sub
Sub sw67_Timer:sw67.IsDropped = 0:End Sub
 
Sub sw68_Hit:vpmTimer.PulseSw 68:PlaySoundAt SoundFX("target",DOFTargets),sw68:End Sub
Sub sw68_Timer:sw68.IsDropped = 0:End Sub
 
Sub sw86_Hit:vpmTimer.PulseSw 86:PlaySoundAt SoundFX("target",DOFTargets),sw86:End Sub
Sub sw86_Timer:sw86.IsDropped = 0:End Sub
 
Sub sw87_Hit:vpmTimer.PulseSw 87:PlaySoundAt SoundFX("target",DOFTargets),sw87:End Sub
Sub sw87_Timer:sw87.IsDropped = 0:End Sub
 
Sub sw88_Hit:vpmTimer.PulseSw 88:PlaySoundAt SoundFX("target",DOFTargets),sw88:End Sub
Sub sw88_Timer:sw88.IsDropped = 0:Me.TimerEnabled = 0:End Sub
 
' Gates
Sub Gate2_Hit():PlaySoundAt "gate",Gate2:End Sub
Sub Gate4_Hit():PlaySoundAt "gate",Gate4:End Sub
Sub Gate6_Hit():PlaySoundAt "gate",Gate6:End Sub
 
' Ramps helpers
Sub RHelp1_Hit()
    StopSound "metalrolling"
    PlaySound "ballhit"
End Sub
 
Sub RHelp2_Hit()
    StopSound "metalrolling"
    PlaySound "ballhit"
End Sub
 
Sub RHelp3_Hit()
    StopSound "metalrolling"
    PlaySound "ballhit"
End Sub
 
'*********
'Solenoids
'*********
 
SolCallback(1) = "Auto_Plunger"
SolCallback(2) = "bsCoffinPopper.SolOut"
SolCallback(3) = "bsCastlePopper.SolOut"
SolCallback(4) = "SolRRampDown"
SolCallback(5) = "bsCryptPopper.SolOut"
SolCallback(6) = "bsBLPopper.SolOut"
SolCallback(7) = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallback(8) = "SolShooterRamp"
SolCallback(14) = "SolRRampUp"
SolCallback(15) = "bsTrough.SolIn"
SolCallback(16) = "bsTrough.SolOut"
 
SolCallback(17) = "Sol117"
SolCallback(18) = "Sol118"
SolCallback(19) = "Sol119"
SolCallback(20) = "Sol120"
SolCallback(21) = "Sol121"
SolCallback(22) = "Sol122"
SolCallback(23) = "Sol123"
SolCallback(24) = "Sol124"
SolCallback(25) = "dtLDrop.SolDropUp"
SolCallback(27) = "SolMistMagnet"
SolCallback(33) = "solTopDiverter"
SolCallback(34) = "SolRGate"
SolCallback(35) = "bsCastleLock.SolOut"
SolCallback(36) = "SolLGate"
 
Sub Auto_Plunger(Enabled)
    If Enabled Then
        Plunger.Fire
        PlaySound SoundFX("solenoid",DOFContactors), 0, 0.3, 0.1, 0.25
Else
        Plunger.PullBack
    End If
End Sub
 
'*************
' Moving Ramp
'*************
 
Dim RRampDir, RRAmpCurrPos, RRamp
RRampCurrPos = 0 ' down
RRampDir = 1     '1 is up -1 is down dir
Controller.Switch(77) = False
'RightRamp.Collidable = True
 
Sub SolRRampUp(Enabled)
    If Enabled Then
        RRampDir = 1
        Controller.Switch(77) = True
        RightRamp.Collidable = False
        UpdateRamp.Enabled = True
        ''SetLamp 116, 1
        playsound SoundFX("diverter",DOFGear), 0, 0.3, 0.1, 0.25
    End If
End Sub
 
Sub SolRRampDown(Enabled)
    If Enabled Then
        RRampDir = -1
        Controller.Switch(77) = False
        RightRamp.Collidable = True
        UpdateRamp.Enabled = True
        ''SetLamp 116, 0
        playsound SoundFX("diverter",DOFGear), 0, 0.3, 0.1, 0.25
    End If
End sub
 
Sub UpdateRamp_Timer
    RRampCurrPos = RRampCurrPos + RRampDir
    If RRampCurrPos> 10 Then
        RRampCurrPos = 10
        UpdateRamp.Enabled = 0
    End If
    If RRampCurrPos <0 Then
        RRampCurrPos = 0
        UpdateRamp.Enabled = 0
    End If
    RightRamp2.HeightBottom = RRampCurrPos *5
    CoffinLiftRampOpaque.RotX = RRampCurrPos -11
    CoffinLiftRamp.RotX = RRampCurrPos -11
    Refresh.State = 1
    Refresh.State = 0
End Sub
 
' Shooter Ramp
 
Sub SolShooterRamp(Enabled)
    If Enabled Then
        sramp2.Collidable = 0
        dirsrt = 1:shootramp.enabled = 1
        Playsound SoundFX("solenoid",DOFGear), 0, 0.3, 0.1, 0.25
    Else
        sramp2.Collidable = 1
        dirsrt = 2:shootramp.enabled = 1
        Playsound SoundFX("solenoid",DOFGear), 0, 0.3, 0.1, 0.25
    End If
End Sub
 
' Top Ramp Diverter
 
Sub SolTopDiverter(Enabled)
    Playsound SoundFX("Diverter",DOFContactors)
    If Enabled Then
        'diverter1.isdropped = False
        'Controller.Switch(78) = True
 
        wDivert.isdropped = 0
        ddir = 1:Divert.enabled = 1
    Else
        'diverter1.isdropped = True
        'Controller.Switch(78) = False
        WDivert.isdropped = 1
        ddir = 2:Divert.enabled = 1
    End If
End sub
 
Dim FlashState(200), FlashLevel(200)
Dim FlashSpeedUp, FlashSpeedDown
'Dim x
 
FlashInit()
FlasherTimer.Interval = 10 'flash fading speed
FlasherTimer.Enabled = 1
 
Sub FlashInit
    Dim i
    For i = 0 to 200
        FlashState(i) = 0
        FlashLevel(i) = 0
    Next
 
    FlashSpeedUp = 500   ' fast speed when turning on the flasher
    FlashSpeedDown = 100 ' slow speed when turning off the flasher, gives a smooth fading
    AllFlashOff()
End Sub
 
Sub AllFlashOff
    Dim i
    For i = 0 to 200
        FlashState(i) = 0
    Next
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
            If FlashLevel(nr) > 1000 Then
                FlashLevel(nr) = 1000
                FlashState(nr) = -2 'completely on
            End if
            Object.opacity = FlashLevel(nr)
    End Select
End Sub
 
Sub Flashm(nr, object) 'multiple flashers, it doesn't change the flashstate
    Select Case FlashState(nr)
        Case 0         'off
            Object.opacity = FlashLevel(nr)
        Case 1         ' on
            Object.opacity = FlashLevel(nr)
    End Select
End Sub
 
Sub SetFlash(nr, stat)
    FlashState(nr) = ABS(stat)
End Sub
 
Sub FlasherTimer_Timer()
Flash 161, f61
Flash 162, f62
Flash 163, f63
'Flashm 158, f58
'Flash 158, f58a
Flashm 121, F121
Flash 121, F121a
Flashm 122, F122
Flash 122, F122a
End Sub
 
Dim ddir
 
Sub Divert_Timer()
Select Case ddir
Case 1:
If Diverter.Z = -315 Then
me.enabled = 0
Controller.Switch(78) = True
Diverter.Z = -315
End If
Diverter.Z = Diverter.Z - 1
Case 2:
If Diverter.Z = -280 Then
me.enabled = 0
Controller.Switch(78) = False
Diverter.Z = -280
End If
Diverter.Z = Diverter.Z + 1
End Select
End Sub
 
 
' Mist Gates
 
Sub SolLGate(Enabled)
    If Enabled then
        LGate.open = 1
        Wall_LO.isdropped = 1
    else
        LGate.open = 0
        Wall_LO.isdropped = 0
    End If
End Sub
 
Sub SolRGate(Enabled)
    If Enabled then
        RGate.open = 1
        'Light51.state = 1
        'RGate.Move 1, 1, 90
        RGateWall.IsDropped = True
    else
        RGate.open = 0
        'Light51.state = 0
        'RGate.Move 0, 1, 0
        RGateWall.IsDropped = False
    End If
End Sub
 
Sub Sol117(Enabled)
F17.State=(Enabled)
F17a.State=(Enabled)
F17b.State=(enabled)
End Sub
 
Sub Sol118(Enabled)
F18.State=(Enabled)
F18a.State=(Enabled)
End Sub
 
Sub Sol119(Enabled)
F19.State=(Enabled)
F19a.State=(Enabled)
End Sub
 
Sub Sol120(Enabled)
F20.State=(Enabled)
F20a.State=(Enabled)
End Sub
 
Sub Sol121(enabled)
F21.State=(Enabled)
F21a.State=(Enabled)
End Sub
 
Sub Sol122(enabled)
F22.State=(Enabled)
F22a.State=(Enabled)
End Sub
 
Sub Sol123(enabled)
F23.State=(Enabled)
F23a.State=(Enabled)
End Sub
 
Sub Sol124(enabled)
F24.State=(Enabled)
End Sub
 
'**************
' Flipper Subs
'**************
 
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
 
Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("flipperup",DOFFlippers),LeftFlipper:LeftFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("flipperdown",DOFFlippers),LeftFlipper:LeftFlipper.RotateToStart
    End If
End Sub
 
Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("flipperup",DOFFlippers),RightFlipper:RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFX("flipperdown",DOFFlippers),RightFlipper:RightFlipper.RotateToStart
    End If
End Sub
 
'***********
' Update GI
'***********
 
Dim gistep, xxx
gistep = 1 / 8
 
Sub UpdateGI2(no, step)
 
   Select Case no
 
        Case 0
 
   For each xxx in GIBOT:xxx.IntensityScale = gistep * step:next
 
        Case 1
 
   For each xxx in GITOP:xxx.IntensityScale = gistep * step:next
 
        Case 2
 
   For each xxx in GIMID:xxx.IntensityScale = gistep * step:next
 
        Case 4
 
'   Light80.IntensityScale = gistep * step
 
    End Select
End Sub
 
'*******************************
'          Mist Magnet
'    taken from Lander's table
' with only a small modification
'*******************************
 
'-------------------------------
' Magnet Simulator Class
' (07/10/2001 Dorsola)
' Modified for Dracula (08/16/2001) by Dorsola
'-------------------------------
 
class cMagnet
    Private cX, cY, cStrength, cRange
    private cBalls, cClaimed
    private cTempX, cTempY
 
    Private Sub Class_Initialize()
        set cBalls = CreateObject("Scripting.Dictionary")
        cRange = 1
        cStrength = 0
    End Sub
 
    Public Sub InitMagnet(aTrigger, inStrength)
        cX = aTrigger.X
        cY = aTrigger.Y
        cRange = aTrigger.Radius
        cStrength = inStrength
    End Sub
 
    Public Sub MoveTo(inX, inY)
        cX = inX
        cY = inY
    End Sub
 
    Public Property Get X:X = cX:End Property
    Public Property Get Y:Y = cY:End Property
    Public Property Get Strength:Strength = cStrength:End Property
    Public Property Get Size:Size = cRange:End Property
    Public Property Get Range:Range = cRange:End Property
    Public Property Get Balls:Balls = cBalls.Keys:End Property
 
    Public Property Let X(inX):cX = inX:End Property
    Public Property Let Y(inY):cY = inY:End Property
    Public Property Let Strength(inStrength):cStrength = inStrength:End Property
    Public Property Let Size(inSize):cRange = inSize:End Property
    Public Property Let Range(inSize):cRange = inSize:End Property
 
    Public Sub AddBall(aBall)
        cBalls.Item(aBall) = 0
    End Sub
 
    Public Sub RemoveBall(aBall)
        ' This function tags balls for removal, but does not remove them.
        ' Another sub will be called to remove tagged objects from the dictionary.
        If cBalls.Exists(aBall) then
            if cClaimed then
                cBalls.Item(aBall) = 1
            else
                cBalls.Remove(aBall)
            end if
        end if
    End Sub
 
    Public Sub Claim():cClaimed = True:End Sub
 
    Public Sub Release()
        cClaimed = False
        Dim tempobj
        for each tempobj in cBalls.Keys
            if cBalls.Item(tempobj) = 1 then cBalls.Remove(tempobj)
        next
    End Sub
 
    Public Sub ProcessBalls()
        Dim tempObj
        for each tempObj in cBalls.Keys:AttractBall tempObj:next
    End Sub
 
    Public Function GetDist(aBall)
        on error resume next
        if aBall is Nothing then
            GetDist = 100000
        else
            cTempX = aBall.X - cX
            cTempY = aBall.Y - cY
            GetDist = Sqr(cTempX * cTempX + cTempY * cTempY)
            if Err then GetDist = 100000
        end if
    End Function
 
    Public Sub AttractBall(aBall)
        if aBall is Nothing then Exit Sub
        Dim Dist
        Dist = GetDist(aBall)
        if Dist> cRange then Exit Sub
 
        ' Attract ball toward magnet center (cX,cY).
 
        ' Attraction force is determined by distance from center, and strength of magnet.
 
        Dim Force, Ratio
        Ratio = Dist / (1.5 * cRange)
 
        ' TODO: Figure out how to dampen the force when ball is near center and
        ' at low velocity, so that balls don't jitter on the magnets.
        ' Also shore up instability on moving magnet.
 
        Force = cStrength * exp(-0.2 / Ratio) / (Ratio * Ratio * 56)
        aBall.VelX = (aBall.VelX - cTempX * Force / Dist) * 0.985
        aBall.VelY = (aBall.VelY - cTempY * Force / Dist) * 0.985
    End Sub
End Class
 
'-----------------------------------------------
' Mist Multiball - courtesy of Dorsola
'-----------------------------------------------
 
' Method: Since any ball that can block the Mist opto is necessarily in the Mist Magnet's trigger area,
' we automatically have access to all balls in this range.  We can therefore check each ball's position
' against a line equation and see if it happens to be blocking the opto, and set the switch accordingly.
' This requires a timer loop.
 
Dim MagnetOn
MagnetOn = false
 
Sub MistTimer_Timer()
    ' Endpoints of the line are (108,1247) and (908,895)
    ' Slope: m = (y2-y1)/(x2-x1) = -0.44
    ' Y-intercept: b = y1 - m*x1 = 1294.52
 
    mMagnet.Claim
 
    Dim obj, CheckState, x, TargetY
    CheckState = 0
    on error resume next
    for each obj in mMagnet.Balls
        ' y = mx+b (m=slope, b=yint)
        TargetY = (-0.44) * obj.X + 1250.52
        if(obj.Y> TargetY - 25) and(obj.Y <TargetY + 25) then CheckState = 1
    next
    on error goto 0
 
    Controller.Switch(82) = CheckState
 
    if MagnetOn then mMagnet.ProcessBalls
 
    mMagnet.Release
End Sub
 
Sub SolMistMagnet(enabled)
    MagnetOn = enabled
End Sub
 
Sub Magnet_Hit()
    mMagnet.AddBall ActiveBall
End Sub
 
Sub Magnet_UnHit()
    mMagnet.RemoveBall ActiveBall
End Sub
 
'------------------------
' Handle the Mist Motor
'------------------------
' Method: Treat motor's position as a number from right to left (0-500)
' and compute its position based on the line equation given above.
 
const motorx1 = 880
const motorx2 = 108
const motorxrange = 800
const motory1 = 850
const motory2 = 1200
const motoryrange = -312
const motorslope = -0.44
const motoryint = 1249.52
 
' Endpoints of the line are (108,1247) and (908,895)
' Slope: m = (y2-y1)/(x2-x1) = -0.44
' Y-intercept: b = y1 - m*x1 = 1294.52
 
Dim MagnetPos, MagnetDir
MagnetPos = 0:MagnetDir = 0
 
' Coding for MagnetDir: 0 = left, 1 = right, toggle at endpoints.
 
Sub MotorTimer_Timer()
    if Controller.Solenoid(28) then
        if MagnetDir = 0 then
            MagnetPos = MagnetPos + .55
            'mist lights
            Select Case MagnetPos \ 33
                Case 0:ml1.State = 0:ml2.State = 0:ml3.State = 0:ml4.State = 0
                    ml5.State = 0:ml6.State = 0:ml7.State = 0:ml8.State = 0
                    ml9.State = 0:ml10.State = 0:ml11.State = 0:ml12.State = 0:ml13.State = 0
                Case 1:ml13.State = 1
                Case 2:ml12.State = 1:ml13.State = 0
                Case 3:ml11.State = 1:ml12.State = 0
                Case 4:ml10.State = 1:ml11.State = 0
                Case 5:ml9.State = 1:ml10.State = 0
                Case 6:ml8.State = 1:ml9.State = 0
                Case 7:ml7.State = 1:ml8.State = 0
                Case 8:ml6.State = 1:ml7.State = 0
                Case 9:ml5.State = 1:ml6.State = 0
                Case 10:ml4.State = 1:ml5.State = 0
                Case 11:ml3.State = 1:ml4.State = 0
                Case 12:ml2.State = 1:ml3.State = 0
                Case 13:ml1.State = 1:ml2.State = 0
                Case 14:ml1.State = 0
            End Select
 
            if MagnetPos >= 500 then
                MagnetPos = 500
                MagnetDir = 1
            end if
        else
            MagnetPos = MagnetPos - 1
            Select Case MagnetPos \ 33
                Case 0:ml1.State = 0:ml2.State = 0:ml3.State = 0:ml4.State = 0
                    ml5.State = 0:ml6.State = 0:ml7.State = 0:ml8.State = 0
                    ml9.State = 0:ml10.State = 0:ml11.State = 0:ml12.State = 0:ml13.State = 0
                Case 1:ml13.State = 0
                Case 2:ml12.State = 0:ml13.State = 1
                Case 3:ml11.State = 0:ml12.State = 1
                Case 4:ml10.State = 0:ml11.State = 1
                Case 5:ml9.State = 0:ml10.State = 1
                Case 6:ml8.State = 0:ml9.State = 1
                Case 7:ml7.State = 0:ml8.State = 1
                Case 8:ml6.State = 0:ml7.State = 1
                Case 9:ml5.State = 0:ml6.State = 1
                Case 10:ml4.State = 0:ml5.State = 1
                Case 11:ml3.State = 0:ml4.State = 1
                Case 12:ml2.State = 0:ml3.State = 1
                Case 13:ml1.State = 0:ml2.State = 1
                Case 14:ml1.State = 1
            End Select
            if MagnetPos <= 0 then
                MagnetPos = 0
                MagnetDir = 0
            end if
        end if
 
        SetMagnetPosition
        Controller.Switch(81) = (MagnetPos> 490)
        Controller.Switch(83) = (MagnetPos <10)
    end if
End Sub
 
Sub SetMagnetPosition()
    mMagnet.X = motorx1 -(motorxrange * (MagnetPos / 500) )
    mMagnet.Y = motorslope * mMagnet.X + motoryint
    If MagnetPos MOD 33 = 0 Then
        MotorTimer.Interval = 80
    Else
        MotorTimer.Interval = 8
    End If
End Sub
 
 
'******************************************
' Use the motor callback to call div subs
'******************************************
 
Set MotorCallback = GetRef("RealTimeUpdates")
 
Sub RealTimeUpdates()
lfs.RotZ = LeftFlipper.CurrentAngle
rfs.RotZ = RightFlipper.CurrentAngle
LFLogo.RotY =  LeftFlipper.CurrentAngle
RFlogo.RotY =  RightFlipper.CurrentAngle
WireGateLR.RotX=Spinner2.currentangle
WireGateLR1.RotX=Spinner1.currentangle
WireGateLR3.RotY=Spinner3.currentangle
BallShadowUpdate
 
If L61.State = 1 Then
setflash 161,1
else
setflash 161,0
End If
 
If L62.State = 1 Then
setflash 162,1
else
setflash 162,0
End If
 
If L63.State = 1 Then
setflash 163,1
else
setflash 163,0
End If
 
If l58.state = 1 Then
setflash 158,1
else
setflash 158,0
End If
 
If l21.state = 1 Then
setflash 121,1
else
setflash 121,0
End If
 
If l22.state = 1 Then
setflash 122,1
else
setflash 122,0
End If
 
End Sub
 
Sub Trigger55off_Hit:sw55.Enabled=0:End Sub
Sub Trigger55on_Hit:sw55.Enabled=1:End Sub
 
Sub LHD_Hit()
PlaySound "ball_bounce", 0, 0.3, -0.1, 0.25, 0, 1, AudioFade(LHD)
End Sub
 
Sub CHD_Hit()
PlaySound "ball_bounce", 0, 0.3, 0, 0.25, 0, 1, AudioFade(CHD)
StopSound "metalrolling"
End Sub
 
Sub CHM_Hit()
PlaySound "metalrolling", 0, 0.3, 0, 0.25, 0, 1, AudioFade(CHM)
End Sub
 
Sub RHD_Hit()
StopSound "subway2"
PlaySound "ball_bounce", 0, 0.3, 0.1, 0.25, 0, 1, AudioFade(RHD)
End Sub
 
Dim dirsrt
 
Sub shootramp_Timer()
Select Case dirsrt
Case 1:
If ramp_sl.heightbottom = 60 Then
me.enabled = 0
ramp_sl.heightbottom = 60
End If
ramp_sl.heightbottom = ramp_sl.heightbottom + 1
Liftramp.ObjRotx = LiftRamp.ObjRotx + 0.25
LiftRampRod.Z = LiftRampRod.Z + 0.25
Primitive108.ObjRotx = Primitive108.ObjRotx + 0.25
Primitive285.ObjRotx = Primitive285.ObjRotx + 0.25
Case 2:
If ramp_sl.heightbottom = 0 Then
me.enabled = 0
ramp_sl.heightbottom = 0
End If
ramp_sl.heightbottom = ramp_sl.heightbottom - 1
Liftramp.ObjRotx = LiftRamp.ObjRotx - 0.25
LiftRampRod.Z = LiftRampRod.Z - 0.25
Primitive108.ObjRotx = Primitive108.ObjRotx - 0.25
Primitive285.ObjRotx = Primitive285.ObjRotx - 0.25
End Select
End Sub
 
 
Sub sw55_UnHit(): StopSound "subway2" : End Sub
 
Sub Pins_Hit (idx)
    PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub
 
Sub Targets_Hit (idx)
    PlaySound "target", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub
 
Sub Metals_Thin_Hit (idx)
    PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub
 
Sub Metals_Medium_Hit (idx)
    PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub
 
Sub Metals2_Hit (idx)
    PlaySound "metalhit2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub
 
Sub Gates_Hit (idx)
    PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub
 
Sub Spinner_Spin
    PlaySoundAt "fx_spinner", Spinner
End Sub
 
Sub Rubbers_Hit(idx)
    dim finalspeed
    finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    If finalspeed > 20 then
        PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End if
    If finalspeed >= 6 AND finalspeed <= 20 then
        RandomSoundRubber()
    End If
End Sub
 
Sub Posts_Hit(idx)
    dim finalspeed
    finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    If finalspeed > 16 then
        PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End if
    If finalspeed >= 6 AND finalspeed <= 16 then
        RandomSoundRubber()
    End If
End Sub
 
Sub RandomSoundRubber()
    Select Case Int(Rnd*3)+1
        Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
        Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End Select
End Sub
 
Sub Table1_Exit
  Controller.Stop
End Sub
 
' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX
' PlaySound sound, 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
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
 
Const tnob = 5 ' total number of balls
ReDim rolling(tnob)
InitRolling

Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6)

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
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        Else ' Ball on raised ramp
          PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.5, AudioPan(BOT(b) ), 0, Pitch(BOT(b) )+50000, 1, 0, AudioFade(BOT(b) )
        End If
      Else
        If rolling(b) = True Then
          StopSound("fx_ballrolling" & b)
          rolling(b) = False
        End If
      End If
    Next
End Sub
 
Sub BallShadowUpdate()
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
        If BOT(b).Z > 0 and BOT(b).Z < 120 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next	
End Sub

'**********************
' Ball Collision Sound
'**********************
 
Sub OnBallBallCollision(ball1, ball2, velocity)
  If Table1.VersionMinor > 3 OR Table1.VersionMajor > 10 Then
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 200, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
  Else
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 200, Pan(ball1), 0, Pitch(ball1), 0, 0
  End if
End Sub
