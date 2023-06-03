' Cyclone VP9 by melon. December 2009.
' Build For VPX by Sliderpoint 2017...
' Rearranged 2019/2020 by ClarkKent

Option Explicit
Randomize


On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

'**************************************************************
'						DECLARACIONES
'**************************************************************
Const UseSolenoids = True
Const UseLamps = True
Const UseSync = False
' Standard Sounds
Const SSolenoidOn = "Solenoid"
Const SSolenoidOff = ""
Const SFlipperOn = "FlipperUp"
Const SFlipperOff = "FlipperDown"
Const SCoin = "Coin"

const BallSize = 50
const BallMass = 1.7

'**************************************************************
'                    USER OPTIONS
'**************************************************************
Dim FlipperShadows, LaneInsertReflections

FlipperShadows = 1 ' change to 0 to turn off flipper shadows
LaneInsertReflections = 1 'Set to 1 to turn on reflections. Improves visibility of top lane lights. Mostly for desktop mode to see through the ramps.

'**************************************************************
' 					INICIO DE LA MESA
'**************************************************************
Const cGameName = "cycln_l5" ' Nombre corto de la ROM para el pinmame. Usamos la última versión oficial.

LoadVPM "00990300", "S11.VBS", 1.91

Dim DesktopMode: DesktopMode = Table1.ShowDT

Sub Table1_Init()

    'Iniciamos el pinmame
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Cyclone. Based on the Williams Table 1988" & vbNewLine & "For VPX by Sliderpoint"
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = 1
        On Error Resume Next
        .Run GetPlayerHWnd
        .DIP(0) = &H80
        If Err Then MsgBox Err.Description
        On Error Goto 0
    End With

    ' Nudging
    vpmNudge.TiltSwitch = swTilt
    vpmNudge.Sensitivity = 1
    vpmNudge.TiltObj = Array(BumperIzquierda, BumperDerecha, BumperAbajo, LeftSlingshot, RightSlingshot)

    'Mystery Wheel
    Dim mWheelMech
    Set mWheelMech = New cvpmMech
    With mWheelMech
        .MType = vpmMechStepSol + vpmMechCircle + vpmMechLinear + vpmMechFast
        .Sol1 = 14
        .Sol2 = 13
        .Length = 200
        .Steps = 90
        .AddSw 41, 0, 45
        .Callback = GetRef("SpinWheel")
        .Start
    End With

	Intensity 'sets GI brightness depending on day/night slider settings

	vpmMapLights AllLamps
	
	Outhole.CreateSizedBallWithMass BSize, BMass
	Outhole.kick 0,0

	'fastFlips enabled
	Set FastFlips = new cFastFlips
		with FastFlips
			.CallBackL = "SolLFlipper"	'Point these to flipper subs
			.CallBackR = "SolRFlipper"	'...
			.TiltObjects = True 'Optional, if True calls vpmnudge.solgameon automatically. IF YOU GET A LINE 1 ERROR, DISABLE THIS! (or setup vpmNudge.TiltObj!)
			.InitDelay "FastFlips", 100			'Optional, if > 0 adds some compensation for solenoid jitter (occasional problem on Bram Stoker's Dracula)
			.DebugOn = False
		end with
End Sub

Sub Table1_KeyDown(keycode)
	If keycode = LeftFlipperKey Then FastFlips.FlipL True :  FastFlips.FlipUL True
	If keycode = RightFlipperKey Then FastFlips.FlipR True :  FastFlips.FlipUR True
    If keycode = PlungerKey Then Plunger.Pullback
    If keycode = LeftTiltKey Then PlaySound "nudge_left"
    If keycode = RightTiltKey Then PlaySound "nudge_right"
    If keycode = CenterTiltKey Then PlaySound "nudge_forward"
    If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If keycode = LeftFlipperKey Then FastFlips.FlipL False :  FastFlips.FlipUL False
   	If keycode = RightFlipperKey Then FastFlips.FlipR False :  FastFlips.FlipUR False
    If keycode = PlungerKey Then Plunger.Fire:PlaySound "plunger",0,1,AudioPan(Plunger),0.25,0,0,1,AudioFade(Plunger)
    If vpmKeyUp(keycode) Then Exit Sub
End Sub

Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub
sub Table1_Exit:Controller.Stop:end sub

'**************************************************************
' 							SOLENOIDS
'**************************************************************
SolCallback(1) = "OutholeKick"
SolCallback(3) = "EMKicker"
SolCallback(4) = "solBoomerangKickout"
SolCallback(5) = "SpookHouseDropUp"
SolCallback(7) = "vpmSolSound ""Knocker"","
SolCallback(10) = "SolGI" 'PF GI
SolCallback(11) = "SolBackGenIllumin" 'BG GI
SolCallback(15) = "Flash15" 'boomerang flashers
SolCallback(16) = "SolWheelDrive"
SolCallBack(23) = "FastFlips.TiltSol"
SolCallback(25) = "Flash25" 'pops left
SolCallback(26) = "Flash26" 'pops right
SolCallback(27) = "Flash27" 'backmiddle
SolCallback(28) = "Flash28" 'cats
SolCallback(29) = "Flash29" 'ducks
SolCallback(30) = "Flash30" 'backleft/Ferris
SolCallback(31) = "Flash31" 'Cyclone Flasher
SolCallback(32) = "Flash32" 'SpookHouse

Sub OutholeKick(enabled)
	If enabled Then
	Outhole.kick 65, 25
	Playsound SoundFX("BallRel",DOFContactors), 0, 1, AudioPan(Outhole), 0,0,0, 1, AudioFade(Outhole)
	End If
end Sub
Sub Outhole_Hit:PlaySound "drain", 0, 1, AudioPan(Outhole), 0,0,0, 1, AudioFade(Outhole):controller.switch(10) = 1:End Sub
Sub Outhole_unHit:Controller.Switch(10) = 0: End Sub

Dim empos, kForce
Sub EMKicker(Enabled)
    If Enabled Then
	kForce = 46.5 + (Rnd*(rnd*9.3))
		KSalida.Kick 0,kForce
		Playsound SoundFX("solenoid",DOFContactors), 0, 1, AudioPan(KSalida), 0,0,0, 1, AudioFade(KSalida)
        empos = 0
        emkickertimer.Enabled = 1
    End If
End Sub

Sub KSalida_Hit
		Playsound "MetalHit", 0, 1, AudioPan(KSalida), 0,0,0, 1, AudioFade(KSalida)
End Sub

Sub emkickertimer_Timer
    Select Case empos
        Case 0:emk.TransY = -10
        Case 1:emk.TransY = -20
        Case 2:emk.TransY = -30
        Case 3:emk.TransY = -40
        Case 4:emk.TransY = -30
        Case 5:emk.TransY = -20
        Case 6:emk.TransY = -10
        Case 7:emk.TransY = 0
        Case else emkickertimer.Enabled = 0
    End Select
    empos = empos + 1
End Sub

Sub SolBackGenIllumin(Enabled)
If Enabled Then 
	PlaySound"tickon" 'plays in BG sounds
Else
	PlaySound"tickoff" 'Plays in BG sounds
End If
End Sub

Sub Flash15(Enabled)
	If Enabled Then
		Flash15a.State = 1
		Flash15b.State = 1
		Flash15c.State = 1
	Else
		Flash15a.State = 0
		Flash15b.State = 0
		Flash15c.State = 0
	End If
End Sub

Sub Flash25(enabled)
	If Enabled Then
		F25.State = 1
		F25b.State =1
	Else
		F25.State = 0
		F25b.State = 0
	End If
End Sub

Sub Flash26(enabled)
	If Enabled Then
		F26.State = 1
		F26b.State = 1
	Else
		F26.State = 0
		F26b.State = 0
	End If
End Sub

Sub Flash27(enabled)
	If Enabled Then
		Flasher27p.image = "DomeOn"
		Flasher27p.material = "Orange FlashersOn"
		F27.State = 1
		F27b.State = 1
	Else
		Flasher27p.image = "DomeOff"
		Flasher27p.material = "Orange Flashers"
		F27.state = 0
		F27b.State = 0
	End If
End Sub

Sub Flash28(enabled)
	If Enabled Then
		F28.State = 1
		F28b.State = 1
	Else
		F28.State = 0
		F28b.State = 0
	End If
End Sub

Sub Flash29(Enabled)
	If Enabled Then
		Flasher29p.image = "DomeOn"
		F29.State = 1
		F29b.State = 1
		F29c.State = 1
		F29d.State = 1
		Flasher29p.material = "Orange FlashersOn"	
	Else
		Flasher29p.image = "DomeOff"
		F29.State = 0
		F29b.State = 0
		F29c.State = 0
		F29d.State = 0
		Flasher29p.material = "Orange Flashers"
	End If
End Sub

Sub Flash30(enabled)
	If Enabled Then
		Flasher30p.image = "DomeOn"
		Flasher30pb.image = "DomeOn"
		Flasher30p.material = "Orange FlashersOn"
		Flasher30pb.material = "Orange FlashersOn"
		F30.State = 1
		F30b.State =1
		F30c.State =1
		F30d.State =1
	Else
		Flasher30p.image = "DomeOff"
		Flasher30pb.image = "DomeOff"
		Flasher30p.material = "Orange Flashers"
		Flasher30pb.material = "Orange Flashers"
		F30.State = 0
		F30b.State = 0
		F30c.State = 0
		F30d.State = 0
	End If
End Sub

Sub Flash31(enabled)
	If Enabled Then
		Flasher31p.image = "DomeOn"
		Flasher31p.material = "Blue Flasherson"
		F31.State = 1
		F31b.State = 1
	Else
		Flasher31p.image = "DomeOff"
		Flasher31p.material = "Blue Flashers"
		F31.State = 0
		F31b.State = 0
	End If
End Sub



Sub Flash32(enabled)
	If Enabled Then
	F32.State = 1
	F32b.State = 1
	F32c.State = 1
	Else
	F32.State = 0
	F32b.State = 0
	F32c.State = 0
	End If
End Sub

'**************************************************************
' 						ILUMINACIÓN GENERAL (GI)
'**************************************************************
Dim xx
DayNight = Table1.NightDay

Sub SolGI(Enabled)
		If Enabled Then
			PlaySound"tickon" 'plays in BG sounds
			Table1.ColorGradeImage = "ColorGrade_0"

			For each xx in GI:xx.State = 0:Next
			For each xx in GI2:xx.State = 0:Next
		Else
			For each xx in GI:xx.State = 1:Next
			For each xx in GI2:xx.State = 1:Next
			PlaySound"tickoff" 'plays in BG sounds
			Table1.ColorGradeImage = "ColorGrade_1"
		End If
End Sub

Dim GILevel, DayNight

Sub Intensity
	If DayNight <= 20 Then 
			GILevel = .5
	ElseIf DayNight <= 40 Then 
			GILevel = .4125
	ElseIf DayNight <= 60 Then 
			GILevel = .325
	ElseIf DayNight <= 80 Then 
			GILevel = .2375
	Elseif DayNight <= 100  Then 
			GILevel = .15
	End If

	For each xx in GI: xx.IntensityScale = xx.IntensityScale * (GILevel): Next
	For each xx in GI2: xx.IntensityScale = xx.IntensityScale * (GILevel): Next
'	For each xx in AllLamps: xx.IntensityScale = xx.IntensityScale * .6: Next 'reduce insert brightness
End Sub

Sub LightTimer_Timer
	If LaneInsertReflections = 1 Then
		l14b.visible = l14a.State
		l15b.visible = l15a.State
		l16b.visible = l16a.State
	End If
	L32b.visible = L32.State
	L31b.visible = L31.State
	L30b.visible = L30.State
	L29b.visible = L29.State
	L28b.visible = L28.State
	L57b.visible = L57.State
	L58b.visible = L58.State
	L59b.visible = L59.State
	L60b.visible = L60.State
	L61b.visible = L61.State
	L62b.visible = L62.State
	L63b.visible = L63.State
	L64b.visible = L64.State
End Sub


'**************************************************************
' 							FLIPPERS
'**************************************************************
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
		PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0,0,0,1,AudioFade(LeftFlipper)
		LeftFlipper.RotateToEnd
    Else
		PlaySound SoundFX("fx_flipperDown",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0,0,0,1,AudioFade(LeftFlipper)
		LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
		PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, 1, AudioPan(RightFlipper), 0,0,0,1,AudioFade(RightFlipper)
		RightFlipper.RotateToEnd
    Else
		PlaySound SoundFX("fx_flipperDown",DOFFlippers), 0, 1, AudioPan(RightFlipper), 0,0,0,1,AudioFade(RightFlipper)
		RightFlipper.RotateToStart
    End If
End Sub

' ball hit flipper sounds
Sub LeftFlipper_Collide(parm)
	PlaySound "rubber2", 0, Vol(ActiveBall)*10, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
	PlaySound "rubber2", 0, Vol(ActiveBall)*10, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

'cFastFlips by nFozzy
'Bypasses pinmame callback for faster and more responsive flippers
'Version 1.1 beta2 (More proper behaviour, extra safety against script errors)
'Williams System 11: Sol23

'********************Setup*******************:

'....somewhere outside of any subs....
'dim FastFlips

'....table init....
'Set FastFlips = new cFastFlips
'with FastFlips
'	.CallBackL = "SolLflipper"	'Point these to flipper subs
'	.CallBackR = "SolRflipper"	'...
''	.CallBackUL = "SolULflipper"'...(upper flippers, if needed)
''	.CallBackUR = "SolURflipper"'...
'	.TiltObjects = True 'Optional, if True calls vpmnudge.solgameon automatically. IF YOU GET A LINE 1 ERROR, DISABLE THIS! (or setup vpmNudge.TiltObj!)
''	.InitDelay "FastFlips", 100			'Optional, if > 0 adds some compensation for solenoid jitter (occasional problem on Bram Stoker's Dracula)
''	.DebugOn = False		'Debug, always-on flippers. Call FastFlips.DebugOn True or False in debugger to enable/disable.
'end with

'...keydown section... commenting out upper flippers is not necessary as of 1.1
'If KeyCode = LeftFlipperKey then FastFlips.FlipL True :  FastFlips.FlipUL True
'If KeyCode = RightFlipperKey then FastFlips.FlipR True :  FastFlips.FlipUR True
'(Do not use Exit Sub, this script does not handle switch handling at all!)

'...keyUp section...
'If KeyCode = LeftFlipperKey then FastFlips.FlipL False :  FastFlips.FlipUL False
'If KeyCode = RightFlipperKey then FastFlips.FlipR False :  FastFlips.FlipUR False

'...Solenoid...
'SolCallBack(31) = "FastFlips.TiltSol"
'//////for a reference of solenoid numbers, see top /////


'One last note - Because this script is super simple it will call flipper return a lot.
'It might be a good idea to add extra conditional logic to your flipper return sounds so they don't play every time the game on solenoid turns off
'Example:
'Instead of
		'LeftFlipper.RotateToStart
		'playsound SoundFX("FlipperDown",DOFFlippers), 0, 1, 0.01	'return
'Add Extra conditional logic:
		'LeftFlipper.RotateToStart
		'if LeftFlipper.CurrentAngle = LeftFlipper.StartAngle then
		'	playsound SoundFX("FlipperDown",DOFFlippers), 0, 1, 0.01	'return
		'end if
'That's it]
'*************************************************
Dim FastFlips

Function NullFunction(aEnabled):End Function	'1 argument null function placeholder
Class cFastFlips
	Public TiltObjects, DebugOn, hi
	Private SubL, SubUL, SubR, SubUR, FlippersEnabled, Delay, LagCompensation, Name, FlipState(3)
	
	Private Sub Class_Initialize()
		Delay = 0 : FlippersEnabled = False : DebugOn = False : LagCompensation = False
		Set SubL = GetRef("NullFunction"): Set SubR = GetRef("NullFunction") : Set SubUL = GetRef("NullFunction"): Set SubUR = GetRef("NullFunction")
	End Sub
	
	'set callbacks
	Public Property Let CallBackL(aInput)  : Set SubL  = GetRef(aInput) : Decouple sLLFlipper, aInput: End Property
	Public Property Let CallBackUL(aInput) : Set SubUL = GetRef(aInput) : End Property
	Public Property Let CallBackR(aInput)  : Set SubR  = GetRef(aInput) : Decouple sLRFlipper, aInput:  End Property
	Public Property Let CallBackUR(aInput) : Set SubUR = GetRef(aInput) : End Property
	Public Sub InitDelay(aName, aDelay) : Name = aName : delay = aDelay : End Sub	'Create Delay
	'Automatically decouple flipper solcallback script lines (only if both are pointing to the same sub) thanks gtxjoe
	Private Sub Decouple(aSolType, aInput)  : If StrComp(SolCallback(aSolType),aInput,1) = 0 then SolCallback(aSolType) = Empty End If : End Sub

	'call callbacks
	Public Sub FlipL(aEnabled)
		FlipState(0) = aEnabled	'track flipper button states: the game-on sol flips immediately if the button is held down (1.1)
		If not FlippersEnabled and not DebugOn then Exit Sub
		subL aEnabled
	End Sub

	Public Sub FlipR(aEnabled)
		FlipState(1) = aEnabled
		If not FlippersEnabled and not DebugOn then Exit Sub
		subR aEnabled
	End Sub

	Public Sub FlipUL(aEnabled)
		FlipState(2) = aEnabled
		If not FlippersEnabled and not DebugOn then Exit Sub
		subUL aEnabled
	End Sub	

	Public Sub FlipUR(aEnabled)
		FlipState(3) = aEnabled
		If not FlippersEnabled and not DebugOn then Exit Sub
		subUR aEnabled
	End Sub	
	
	Public Sub TiltSol(aEnabled)	'Handle solenoid / Delay (if delayinit)
		If delay > 0 and not aEnabled then 	'handle delay
			vpmtimer.addtimer Delay, Name & ".FireDelay" & "'"
			LagCompensation = True
		else
			If Delay > 0 then LagCompensation = False
			EnableFlippers(aEnabled)
		end If
	End Sub
	
	Sub FireDelay() : If LagCompensation then EnableFlippers False End If : End Sub
	
	Private Sub EnableFlippers(aEnabled)
		If aEnabled then SubL FlipState(0) : SubR FlipState(1) : subUL FlipState(2) : subUR FlipState(3)
		FlippersEnabled = aEnabled
		If TiltObjects then vpmnudge.solgameon aEnabled
		If Not aEnabled then
			subL False
			subR False
			If not IsEmpty(subUL) then subUL False
			If not IsEmpty(subUR) then subUR False
		End If		
	End Sub
	
End Class

'**************************************************************
'					 SLINGS, BUMPERS, ETC
'**************************************************************
'Bumpers

Sub BumperIzquierda_Hit:vpmTimer.pulsesw 60:PlaySound SoundFX("fx_bumper1",DOFContactors), 0,1,AudioPan(BumperIzquierda),0,0,0,1,AudioFade(BumperIzquierda):End Sub
Sub BumperDerecha_Hit:vpmTimer.pulsesw 61:PlaySound SoundFX("fx_bumper2",DOFContactors), 0,1,AudioPan(BumperDerecha),0,0,0,1,AudioFade(BumperDerecha):End Sub
Sub BumperAbajo_Hit:vpmTimer.pulsesw 62:PlaySound SoundFX("fx_bumper3",DOFContactors), 0,1,AudioPan(BumperAbajo),0,0,0,1,AudioFade(BumperAbajo):End Sub

'Slings
Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    vpmTimer.pulsesw 63
	PlaySound SoundFX("slingshotL",DOFContactors), 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0:LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer ' animation of the rubber
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    vpmTimer.pulsesw 64
	PlaySound SoundFX("slingshotR",DOFContactors), 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0:RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'**************************************************************
'					 Targets
'**************************************************************
'Ducks
Sub sw19_Hit():vpmTimer.pulsesw 19:Playsound "target", 0, 1, AudioPan(sw19), 0, Pitch(ActiveBall), 1, 0, AudioFade(sw19):End sub
Sub sw20_Hit():vpmTimer.pulsesw 20:Playsound "target", 0, 1, AudioPan(sw20), 0, Pitch(ActiveBall), 1, 0, AudioFade(sw20):End sub
Sub sw21_Hit():vpmTimer.pulsesw 21:Playsound "target", 0, 1, AudioPan(sw21), 0, Pitch(ActiveBall), 1, 0, AudioFade(sw21):End sub
'Cats
Sub sw22_Hit():vpmTimer.pulsesw 22:Playsound "target", 0, 1, AudioPan(sw22), 0, Pitch(ActiveBall), 1, 0, AudioFade(sw22):End sub
Sub sw23_Hit():vpmTimer.pulsesw 23:Playsound "target", 0, 1, AudioPan(sw23), 0, Pitch(ActiveBall), 1, 0, AudioFade(sw23):End sub
Sub sw24_Hit():vpmTimer.pulsesw 24:Playsound "target", 0, 1, AudioPan(sw24), 0, Pitch(ActiveBall), 1, 0, AudioFade(sw24):End sub
Sub sw59_Hit():vpmTimer.pulsesw 25:Playsound "target", 0, 1, AudioPan(sw59), 0, Pitch(ActiveBall), 1, 0, AudioFade(sw59):End sub


'Boomerang Kicker
dim bola, bola2, altura, altura2
Sub sw13_Hit:Controller.Switch(13) = 1:End Sub
Sub sw13_unHit:	Controller.Switch(13) = 0:end Sub
Sub sw11_Hit:vpmtimer.pulsesw 11:End Sub

'SpookHouse Drop Target
Dim stepTarget
Sub Sw18_Hit:Sw18.Isdropped = True:PlaySound "DropTarget", 0, 1, AudioPan(sw18), 0, Pitch(ActiveBall), 1, 0, AudioFade(sw18):Controller.Switch(18) = 1:End Sub

Sub SpookHouseDropUp(Enabled)
	If Enabled Then
	sw18.isDropped = 0
	Controller.Switch(18) = 0
	Playsound SoundFX("solenoid",DOFContactors), 0, 1, AudioPan(sw18), 0, 0, 1, 0, AudioFade(sw18)
	End If
End Sub

Sub SolBoomerangKickOut(enabled)
	Kicker2.enabled = 0
	Sw13.kick 325, 36
	Kicker2.timerenabled = 1
	Playsound SoundFX("boomerang_kick",DOFContactors), 0, 1, AudioPan(Sw13), 0, 0, 1, 0, AudioFade(Sw13)
End Sub

Sub Kicker2_Hit:Playsound "Scoop_Enter", 0, 1, AudioPan(Kicker2), 0, 0, 1, 0, AudioFade(Kicker2):End Sub
Sub Kicker1_Hit:Playsound "Kicker2", 0, 1, AudioPan(Kicker1), 0, 0, 1, 0, AudioFade(Kicker1):End Sub

Sub Kicker2_Timer
	Kicker2.enabled = 1
	Kicker2.timerenabled = 0
End Sub

'**************************************************************
' 						SENSORES (SWITCHES)
'**************************************************************
Sub ShooterLane_Hit:Controller.Switch(25) = 1:End Sub
Sub ShooterLane_Unhit:Controller.Switch(25) = 0:PlaySound "metalhit":End Sub

'Pasillos
Sub sw53_Hit:Controller.Switch(53) = 1:PlaySound "Sensor", 0, 1, AudioPan(sw53), 0, 0, 1, 0, AudioFade(sw53): End Sub
Sub sw53_Unhit:Controller.Switch(53) = 0:End Sub
Sub sw54_Hit:Controller.Switch(54) = 1:PlaySound "Sensor", 0, 1, AudioPan(sw54), 0, 0, 1, 0, AudioFade(sw54): End Sub
Sub sw54_Unhit:Controller.Switch(54) = 0:End Sub
Sub sw55_Hit:Controller.Switch(55) = 1:PlaySound "Sensor", 0, 1, AudioPan(sw55), 0, 0, 1, 0, AudioFade(sw55): End Sub
Sub sw55_Unhit:Controller.Switch(55) = 0:End Sub
Sub sw56_Hit:Controller.Switch(56) = 1:PlaySound "Sensor", 0, 1, AudioPan(sw56), 0, 0, 1, 0, AudioFade(sw56): End Sub
Sub sw56_Unhit:Controller.Switch(56) = 0:End Sub
Sub sw14_Hit:Controller.Switch(14) = 1:PlaySound "Sensor", 0, 1, AudioPan(sw14), 0, 0, 1, 0, AudioFade(sw14): End Sub
Sub sw14_Unhit:Controller.Switch(14) = 0:End Sub
Sub sw15_Hit:Controller.Switch(15) = 1:PlaySound "Sensor", 0, 1, AudioPan(sw15), 0, 0, 1, 0, AudioFade(sw15): End Sub
Sub sw15_Unhit:Controller.Switch(15) = 0:End Sub
Sub sw16_Hit:Controller.Switch(16) = 1:PlaySound "Sensor", 0, 1, AudioPan(sw16), 0, 0, 1, 0, AudioFade(sw16): End Sub
Sub sw16_Unhit:Controller.Switch(16) = 0:End Sub
Sub Sw17_Hit:Controller.Switch(17) = 1:PlaySound "Sensor", 0, 1, AudioPan(sw17), 0, 0, 1, 0, AudioFade(sw17): End Sub 'Entrada ferris wheel
Sub Sw17_UnHit:Controller.Switch(17) = 0:End Sub
Sub Sw31_Hit:Controller.Switch(31) = 1:PlaySound"comet_spinner", 0, 1, AudioPan(sw31), 0, 0, 1, 0, AudioFade(sw31):End Sub'Entrada rampa comet
Sub Sw31_UnHit
	Controller.Switch(31) = 0
	If ActiveBall.VelY < 0 Then
		PlaySound"comet_ramp", 0, 1, AudioPan(sw31), 0, 0, 1, 0, AudioFade(sw31)
	Else
		StopSound"comet_ramp"
	End If
End Sub 
Sub Sw32_Hit:Controller.Switch(32) =1:SwitchWire2.objRotZ = -15:End Sub                          'Score rampa comet
Sub Sw32_UnHit:Controller.Switch(32) =0:SwitchWire2.objRotZ = 0:End Sub
Sub Sw34_Hit:Controller.Switch(34) =1:SwitchWire1.objRotZ = -15:End Sub                          'Score rampa cyclone
Sub Sw34_UnHit:Controller.Switch(34) =0:SwitchWire1.objRotZ = 0:End Sub 
Sub Sw35_Hit:VPMTimer.PulseSw 35:PlaySound "Rubber_hit_3", 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall): End Sub 'Sensores en varias gomas
Sub Sw36_Hit:VPMTimer.PulseSw 36:PlaySound "Rubber_hit_2", 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall): End Sub
Sub Sw37_Hit:VPMTimer.PulseSw 37:PlaySound "Rubber_hit_1", 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall): End Sub

'SkillShot
Sub sw26_Hit():VPMTimer.PulseSw 26:PlaySound "Sensor", 0, 1, AudioPan(sw26), 0, Pitch(ActiveBall), 1, 0, AudioFade(sw26): End Sub
Sub sw27_Hit():VPMTimer.PulseSw 27:PlaySound "Sensor", 0, 1, AudioPan(sw27), 0, Pitch(ActiveBall), 1, 0, AudioFade(sw27): End Sub
Sub sw28_Hit():VPMTimer.PulseSw 28:PlaySound "Sensor", 0, 1, AudioPan(sw28), 0, Pitch(ActiveBall), 1, 0, AudioFade(sw28): End Sub
Sub sw29_Hit():VPMTimer.PulseSw 29:PlaySound "Sensor", 0, 1, AudioPan(sw29), 0, Pitch(ActiveBall), 1, 0, AudioFade(sw29): End Sub
Sub sw30_Hit():VPMTimer.PulseSw 30:PlaySound "Sensor", 0, 1, AudioPan(sw30), 0, Pitch(ActiveBall), 1, 0, AudioFade(sw30): End Sub


'**************************************************************
' 						MYSTERY WHEEL
'**************************************************************

Sub SpinWheel(aNewPos, aSpeed, aLastPos)
     If aNewPos <> aLastPos then
		MysteryWheel.Rotz = aNewPos
     End If
End Sub

'**************************************************************
' 	NEW FERRIS WHEEL  - Ball movement based on Rothbauerw script - MWR
'**************************************************************

Dim Tball,tcount,xstart,ystart,zstart,xbstart,ybstart,zbstart,xphstart,yphstart,zphstart
Dim dradius, xyangle, zangle, dzangle

Function PI()
	PI = 4*Atn(1)
End Function

Function Radians(angle)
	Radians = PI * angle / 180
End Function

xphstart = 230
yphstart = 243
zphstart = -18

dradius= 80
xyangle = 290
zangle = 475

xstart = dradius*cos(radians(xyangle))*Sin(Radians(zangle))
ystart = dradius*sin(radians(xyangle))*Sin(Radians(zangle))
zstart = dradius*cos(radians(zangle))

Sub Trigger2_Hit
'	PlaySound "knocker" 'for testing
	PlaySound"ferris_hit1", 0, 1, AudioPan(Trigger2), 0, 0, 1, 0, AudioFade(Trigger2)
	activeball.vely=0
	activeball.velz=0
	activeball.x=xphstart
	activeball.y=yphstart
	activeball.z=zphstart
	Set Tball = Activeball
	tcount = 0
	WheelHit.enabled = 1
	Trigger2.enabled = 0
End Sub

Sub Trigger2_timer
		WheelHit.Enabled = 0
	Dim xd, yd, zd
	If tcount =  1 Then
		xbstart = Tball.x
		ybstart = Tball.y
		zbstart = Tball.z
		dzangle = zangle
	End If

	If tcount > 2 Then
		dzangle = dzangle - .4
		xd = xstart - dradius*cos(radians(xyangle))*Sin(Radians(dzangle))
		yd = ystart - dradius*sin(radians(xyangle))*Sin(Radians(dzangle))
		zd = zstart - dradius*cos(radians(dzangle))
		Tball.x = xbstart - xd
		Tball.y = ybstart - yd
		Tball.z = zbstart - zd
		Tball.velx = 0
		Tball.vely = 0
		Tball.velz = 0
		if dzangle < 350 Then
			dzangle = 0
			Trigger2.TimerEnabled = false
			Trigger2.Enabled = 1
		end if
	End If
	tcount = tcount + 1
End Sub

Sub SolWheelDrive(enabled)
    if enabled then
        FWTimer.Enabled = 1
		PlaySound"motor", -1, 1, AudioPan(Trigger2), 0, 0, 1, 0, AudioFade(Trigger2)
    else
        FWTimer.Enabled = 0
		StopSound"motor"
    end if
End Sub

Sub FWTimer_Timer
    RedWheel.RotX = (RedWheel.RotX + 1) MOD 360

	If  RedWheel.RotX >15 and RedWheel.RotX <65 Then
		FerrisBlockedWall.isdropped = 1
	elseif RedWheel.RotX >105 and RedWheel.RotX <150 Then
		FerrisBlockedWall.isdropped = 1
	elseif RedWheel.RotX >200 and RedWheel.RotX <240 Then
		FerrisBlockedWall.isdropped = 1	
	elseif RedWheel.RotX >285 and RedWheel.RotX <330 Then
		FerrisBlockedWall.isdropped = 1
	else
		FerrisBlockedWall.isdropped = 0	
	End If
End Sub

Sub WheelHit_Timer
	If (RedWheel.Rotx > 49 and RedWheel.Rotx < 52) or (RedWheel.Rotx > 139 and RedWheel.Rotx < 142) or (RedWheel.Rotx > 229 AND RedWheel.Rotx < 232) or (RedWheel.Rotx > 319 and RedWheel.Rotx < 322) Then
'	If RedWheel.Rotx = 50  or RedWheel.Rotx = 140 or RedWheel.Rotx = 230 or RedWheel.RotX = 320 Then
		Trigger2.TimerEnabled = True
	End If
End Sub

Sub FerrisBlockedWall_Hit
	PlaySound "MetalHit", 0, (Vol(ActiveBall)*5), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

'**************************************************************
' 	END FERRIS WHEEL
'**************************************************************

' Ferris Wheel RAMP Sounds
Sub FerrisSE_Hit: PlaySound"ferris_Exit", 0, 1, AudioPan(FerrisSE), 0, 0, 1, 0, AudioFade(FerrisSE):End Sub
Sub FerrisS1_Hit: PlaySound"ferris_hit1", 0, 1, AudioPan(FerrisS1), 0, 0, 1, 0, AudioFade(FerrisS1):End Sub
Sub FerrisS2_Hit: PlaySound"ferris_hit2", 0, 1, AudioPan(FerrisS2), 0, 0, 1, 0, AudioFade(FerrisS2):End Sub
Sub FerrisS3_Hit: PlaySound"ferris_hit1", 0, 1, AudioPan(FerrisS3), 0, 0, 1, 0, AudioFade(FerrisS3):End Sub
Sub FerrisS4_Hit: PlaySound"ferris_hit2", 0, 1, AudioPan(FerrisS4), 0, 0, 1, 0, AudioFade(FerrisS4):End Sub
Sub FerrisS5_Hit: PlaySound"ferris_hit1", 0, 1, AudioPan(FerrisS5), 0, 0, 1, 0, AudioFade(FerrisS5):End Sub
Sub FerrisS6_Hit: PlaySound"ferris_hit2", 0, 1, AudioPan(FerrisS6), 0, 0, 1, 0, AudioFade(FerrisS6):End Sub
Sub FerrisS7_Hit: PlaySound"ferris_hit1", 0, 1, AudioPan(FerrisS7), 0, 0, 1, 0, AudioFade(FerrisS7):End Sub
Sub FerrisS8_Hit: PlaySound"ferris_hit2", 0, 1, AudioPan(FerrisS8), 0, 0, 1, 0, AudioFade(FerrisS8):End Sub
Sub FerrisS9_Hit: PlaySound"top_lane", 0, 1, AudioPan(FerrisS9), 0, 0, 1, 0, AudioFade(FerrisS9):End Sub

'**************************************************************
' OBJETOS NO PINMAME
'**************************************************************

Sub FinalRampaComet_Hit
    ActiveBall.VelZ = -2
    ActiveBall.VelY = 0
    ActiveBall.VelX = 0
    Playsound "comet_exit1", 0, 1, AudioPan(FinalRampaComet), 0, 0, 1, 0, AudioFade(FinalRampaComet)
End Sub

Sub MitadRampaComet_Hit
    ActiveBall.VelZ = -2
    ActiveBall.VelY = 0
    ActiveBall.VelX = 0
    me.timerinterval = 180
    PlaySound"comet_exit3", 0, 1, AudioPan(MitadRampaComet), 0, 0, 1, 0, AudioFade(MitadRampaComet)
End Sub

Sub FinalRampaCyclone_Hit
    ActiveBall.VelZ = -2
    ActiveBall.VelY = 0
    ActiveBall.VelX = 0
    me.timerinterval = 150
    PlaySound"comet_exit2", 0, 1, AudioPan(FinalRampaCyclone), 0, 0, 1, 0, AudioFade(FinalRampaCyclone)
End Sub

Sub FinalRampaFerris_Hit
	ActiveBall.VelZ = -2
    ActiveBall.VelY = 0
    ActiveBall.VelX = 0
    me.timerinterval = 150
	PlaySound"ferris_ball_drop", 0, 1, AudioPan(FinalRampaFerris), 0, 0, 1, 0, AudioFade(FinalRampaFerris)
End Sub

'Comet /Cyclone Ramp sounds
Sub CometS1_Hit
	If ActiveBall.VelY < 0 then
		PlaySound"cyclone_ramp_enter", 0, 1, AudioPan(CometS1), 0, 0, 1, 0, AudioFade(CometS1)
	End If
End Sub
Sub CometS1_UnHit
	If ActiveBall.VelY > 0 Then
		StopSound"cyclone_ramp_enter"
	End If
End Sub

Sub CometS2_Hit: PlaySound"comet_hit1", 0, 1, AudioPan(CometS2), 0, 0, 1, 0, AudioFade(CometS2):End Sub
Sub CometS3_Hit: PlaySound"comet_hit2", 0, 1, AudioPan(CometS3), 0, 0, 1, 0, AudioFade(CometS3):End Sub
Sub CometS4_Hit: PlaySound"comet_hit1", 0, 1, AudioPan(CometS4), 0, 0, 1, 0, AudioFade(CometS4):End Sub
Sub CometS5_Hit: PlaySound"comet_hit2", 0, 1, AudioPan(CometS5), 0, 0, 1, 0, AudioFade(CometS5):End Sub
Sub CometS6_Hit: PlaySound"comet_hit1", 0, 1, AudioPan(CometS6), 0, 0, 1, 0, AudioFade(CometS6):End Sub
Sub CometS7_Hit: PlaySound"comet_hit2", 0, 1, AudioPan(CometS7), 0, 0, 1, 0, AudioFade(CometS7):End Sub
Sub CometS8_Hit: PlaySound"comet_hit1", 0, 1, AudioPan(CometS8), 0, 0, 1, 0, AudioFade(CometS8):End Sub
Sub CometS9_Hit: PlaySound"comet_hit2", 0, 1, AudioPan(CometS9), 0, 0, 1, 0, AudioFade(CometS9):End Sub

'SkillShot drop sounds
Sub skillS1_Hit:PlaySound"ball_drop", 0, 1, AudioPan(skillS1), 0, 0, 1, 0, AudioFade(skillS1):End Sub
Sub skillS2_Hit:PlaySound"ball_drop", 0, 1, AudioPan(skillS2), 0, 0, 1, 0, AudioFade(skillS2):End Sub
Sub skillS3_Hit:PlaySound"ball_drop", 0, 1, AudioPan(skillS3), 0, 0, 1, 0, AudioFade(skillS3):End Sub
Sub skillS4_Hit:PlaySound"ball_drop", 0, 1, AudioPan(skillS4), 0, 0, 1, 0, AudioFade(skillS4):End Sub
Sub skillS5_Hit:PlaySound"ball_drop", 0, 1, AudioPan(skillS5), 0, 0, 1, 0, AudioFade(skillS5):End Sub
Sub skillS6_Hit:PlaySound"ball_drop", 0, 1, AudioPan(skillS6), 0, 0, 1, 0, AudioFade(skillS6):End Sub

'******************
' RealTime Updates
'******************
 Set MotorCallback = GetRef("GameTimer")

Sub GameTimer
	If FlipperShadows = 1 Then
		FlipperShadowL.RotZ = LeftFlipper.currentAngle
		FlipperShadowR.RotZ = RightFlipper.currentAngle
	End If
End Sub

If FlipperShadows = 1 Then
	FlipperShadowL.visible = 1
	FlipperShadowR.visible = 1
	Else
	FlipperShadowL.visible = 0
	FlipperShadowR.visible = 0
End If

'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************
 Dim Digits(32)
 Digits(0)=Array(a00, a05, a0c, a0d, a08, a01, a06, a0f, a02, a03, a04, a07, a0b, a0a, a09, a0e)
 Digits(1)=Array(a10, a15, a1c, a1d, a18, a11, a16, a1f, a12, a13, a14, a17, a1b, a1a, a19, a1e)
 Digits(2)=Array(a20, a25, a2c, a2d, a28, a21, a26, a2f, a22, a23, a24, a27, a2b, a2a, a29, a2e)
 Digits(3)=Array(a30, a35, a3c, a3d, a38, a31, a36, a3f, a32, a33, a34, a37, a3b, a3a, a39, a3e)
 Digits(4)=Array(a40, a45, a4c, a4d, a48, a41, a46, a4f, a42, a43, a44, a47, a4b, a4a, a49, a4e)
 Digits(5)=Array(a50, a55, a5c, a5d, a58, a51, a56, a5f, a52, a53, a54, a57, a5b, a5a, a59, a5e)
 Digits(6)=Array(a60, a65, a6c, a6d, a68, a61, a66, a6f, a62, a63, a64, a67, a6b, a6a, a69, a6e)
 Digits(7)=Array(a70, a75, a7c, a7d, a78, a71, a76, a7f, a72, a73, a74, a77, a7b, a7a, a79, a7e)
 Digits(8)=Array(a80, a85, a8c, a8d, a88, a81, a86, a8f, a82, a83, a84, a87, a8b, a8a, a89, a8e)
 Digits(9)=Array(a90, a95, a9c, a9d, a98, a91, a96, a9f, a92, a93, a94, a97, a9b, a9a, a99, a9e)
 Digits(10)=Array(aa0, aa5, aac, aad, aa8, aa1, aa6, aaf, aa2, aa3, aa4, aa7, aab, aaa, aa9, aae)
 Digits(11)=Array(ab0, ab5, abc, abd, ab8, ab1, ab6, abf, ab2, ab3, ab4, ab7, abb, aba, ab9, abe)
 Digits(12)=Array(ac0, ac5, acc, acd, ac8, ac1, ac6, acf, ac2, ac3, ac4, ac7, acb, aca, ac9, ace)
 Digits(13)=Array(ad0, ad5, adc, add, ad8, ad1, ad6, adf, ad2, ad3, ad4, ad7, adb, ada, ad9, ade)
 Digits(14)=Array(ae0, ae5, aec, aed, ae8, ae1, ae6, aef, ae2, ae3, ae4, ae7, aeb, aea, ae9, aee)
 Digits(15)=Array(af0, af5, afc, afd, af8, af1, af6, aff, af2, af3, af4, af7, afb, afa, af9, afe)
 
 Digits(16)=Array(b00, b05, b0c, b0d, b08, b01, b06, b0f, b02, b03, b04, b07, b0b, b0a, b09, b0e)
 Digits(17)=Array(b10, b15, b1c, b1d, b18, b11, b16, b1f, b12, b13, b14, b17, b1b, b1a, b19, b1e)
 Digits(18)=Array(b20, b25, b2c, b2d, b28, b21, b26, b2f, b22, b23, b24, b27, b2b, b2a, b29, b2e)
 Digits(19)=Array(b30, b35, b3c, b3d, b38, b31, b36, b3f, b32, b33, b34, b37, b3b, b3a, b39, b3e)
 Digits(20)=Array(b40, b45, b4c, b4d, b48, b41, b46, b4f, b42, b43, b44, b47, b4b, b4a, b49, b4e)
 Digits(21)=Array(b50, b55, b5c, b5d, b58, b51, b56, b5f, b52, b53, b54, b57, b5b, b5a, b59, b5e)
 Digits(22)=Array(b60, b65, b6c, b6d, b68, b61, b66, b6f, b62, b63, b64, b67, b6b, b6a, b69, b6e)
 Digits(23)=Array(b70, b75, b7c, b7d, b78, b71, b76, b7f, b72, b73, b74, b77, b7b, b7a, b79, b7e)
 Digits(24)=Array(b80, b85, b8c, b8d, b88, b81, b86, b8f, b82, b83, b84, b87, b8b, b8a, b89, b8e)
 Digits(25)=Array(b90, b95, b9c, b9d, b98, b91, b96, b9f, b92, b93, b94, b97, b9b, b9a, b99, b9e)
 Digits(26)=Array(ba0, ba5, bac, bad, ba8, ba1, ba6, baf, ba2, ba3, ba4, ba7, bab, baa, ba9, bae)
 Digits(27)=Array(bb0, bb5, bbc, bbd, bb8, bb1, bb6, bbf, bb2, bb3, bb4, bb7, bbb, bba, bb9, bbe)
 Digits(28)=Array(bc0, bc5, bcc, bcd, bc8, bc1, bc6, bcf, bc2, bc3, bc4, bc7, bcb, bca, bc9, bce)
 Digits(29)=Array(bd0, bd5, bdc, bdd, bd8, bd1, bd6, bdf, bd2, bd3, bd4, bd7, bdb, bda, bd9, bde)
 Digits(30)=Array(be0, be5, bec, bed, be8, be1, be6, bef, be2, be3, be4, be7, beb, bea, be9, bee)
 Digits(31)=Array(bf0, bf5, bfc, bfd, bf8, bf1, bf6, bff, bf2, bf3, bf4, bf7, bfb, bfa, bf9, bfe)
 
 Sub DisplayTimer_Timer
    Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
    ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
		If DesktopMode = True Then
       For ii=0 To UBound(chgLED)
          num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
			if (num < 32) then
              For Each obj In Digits(num)
                   If chg And 1 Then obj.State=stat And 1
                   chg=chg\2 : stat=stat\2
                  Next
			Else
			       end if
        Next
	   end if
    End If
 End Sub

'*********************************************************************
'                 Positional Sound Playback Functions
'*********************************************************************

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

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol(sound, VolMult)
		PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
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

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 500)
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
Const tnob = 1 ' total number of balls
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
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

	' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball
    For b = 0 to UBound(BOT)
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), AudioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0
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
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'Extra Sounds
Sub Rubbers_Hit(idx):RandomRubberSound:End Sub 
Sub aGates_Hit(idx):RandomGateSound:End Sub
Sub Posts_Hit(idx):PlaySound "rubber", 0, Vol(ActiveBall)*10, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):End Sub 
Sub Posts2_Hit(idx):PlaySound "rubber", 0, Vol(ActiveBall)*10, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):End Sub 
Sub aGates2_Hit(idx):PlaySound "Gate", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):End Sub
Sub aMetals_Hit(idx):PlaySound "MetalHit", 0, Vol(ActiveBall)*10, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):End Sub
Sub RampEnds_Hit(idx):PlaySound "RampEndHit", 0, Vol(ActiveBall)*10, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):End Sub
Sub Trigger1_hit:Playsound "metalhit", 0, Vol(ActiveBall)*10, AudioPan(Trigger1), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall):End Sub

Sub RandomRubberSound
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtBallVol "rubber_hit_1",2
		Case 2 : PlaySoundAtBallVol "rubber_hit_2",2
		Case 3 : PlaySoundAtBallVol "rubber_hit_3",2
	End Select
End Sub

Sub RandomGateSound
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtBallVol "GateHit1",2
		Case 2 : PlaySoundAtBallVol "GateHit2",2
		Case 3 : PlaySoundAtBallVol "GateHit3",2
	End Select
End Sub

'*****************************************
'	Ball Shadow
'*****************************************

Dim BallShadow
BallShadow = Array (BallShadow1)

Sub BallShadowUpdate_Timer()
    Dim BOT, b
    BOT = GetBalls

	' render the shadow for each ball
    For b = 0 to UBound(BOT)
		If BOT(b).X < Table1.Width/2 Then
			BallShadow(b).X = ((BOT(b).X) - (50/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 10
		Else
			BallShadow(b).X = ((BOT(b).X) + (50/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 10
		End If

			BallShadow(b).Y = BOT(b).Y + 20
			BallShadow(b).Z = 1
		If BOT(b).Z > 20 Then
			BallShadow(b).visible = 1
		Else
			BallShadow(b).visible = 0
		End If
	Next
End Sub
