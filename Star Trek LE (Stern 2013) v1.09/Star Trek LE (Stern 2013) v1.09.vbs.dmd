'Star Trek - Limited Edition (Stern 2013)
'IPDB #6046

Option Explicit
Randomize

'Table created by nFozzy

'Additional Credits:
'EOStimer script taken from HMLF by Wrd1972 and Rothbauer
'Round flasher caps by Zany & Dark
'SFX by Knorr and Clark Kent
'Ballshadow routine by Ninuzzu

'Much thanks to JesperPark for targets, vengeance images and plastic scans

'Version 1.01 Changelog 
'Fixed / Added ball reflections
'Script cleanup
'Some redrawn playfield elements courtesy of Neofr45

' 1.09 - TastyWasps - Fleep Sounds, VR Room integration from Sixtoe assets, VR Backglass upscaled, Lighting Tweaks, Play Balance

Dim UseVPMDMD:UseVPMDMD = DesktopMode
Dim DesktopMode:DesktopMode = Table1.ShowDT
Dim TableWidth:TableWidth = Table1.width
Dim TableHeight:TableHeight = Table1.height
Const tnob = 5

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

dim SoundLevelMult, PunchItKey, StagedFlippers, UpperFlipperKey, RGBSaturation

'----- Table Options ----
Const cGameName = "st_161h"	' Default game rom
'Const cGameName = "st_161hc" ' Color ROM

' Minimal desktop background included in "Image Manager" under "desktop_background_minimal"

RGBSaturation = 0.12			' Amount of desaturation applied to the RGB inserts. Adjust to taste.

PunchItKey = RightMagnaSave			' Set the keybind for the lockdown bar button. EG LockbarKey, LeftMagnaSave, RightMagnaSave

StagedFlippers = False			' Staged Flippers Support
UpperFlipperKey = 205 			' Define a key for the upper flipper. '205 = Right Arrow Key

Const Shaker = True				' DOF only - Shaker motor. Can be disabled in rom code, and here.

const SingleScreenFS = False	' Rotates DMD for single screen FS setups

Dim MoveButton
MoveButton = 1 					' Punch It button mod, moves button to Apron. 1 = on (default)

'----- Target Bounce Options -----
Const TargetBouncerEnabled = 1	  '0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7	 'Level of bounces. Recommmended value of 0.7

'----- General Sound Options -----
Const VolumeDial = 0.80			  ' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Const BallRollVolume = 0.5		  ' Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 0.5		  ' Level of ramp rolling volume. Value between 0 and 1

'----- Desktop Objects -----
If DesktopMode = True Then
	PunchIt_Metal.Visible = 0
End If

'----- VR Room Auto-Detect -----
Dim VRMode, VR_Obj

If RenderingMode = 2 Then
	VRMode = True
	MoveButton = 0 ' Punch it is defaulted on VR lockdown.  Punch It lights have to be moved too.
	l66.y = 2211.931:l66.height = 155
	l67.y = 2211.931:l67.height = 155
	l68.y = 2211.931:l68.height = 155
	For Each VR_Obj in VRCabinet : VR_Obj.Visible = 1 : Next
	For Each VR_Obj in VRRoom : VR_Obj.Visible = 1 : Next
	PunchIt_Metal.Visible = 0
	PunchIt_Button_Base.Visible = 0
	PunchIt_Button_Clear.Visible = 0
	CabinetRails.Visible = 0
	Gi_BackwallA.Height = 320
Else
	VRMode = False
	Gi_BackwallA.Height = 400
	For Each VR_Obj in VRCabinet : VR_Obj.Visible = 0 : Next
	For Each VR_Obj in VRRoom : VR_Obj.Visible = 0 : Next
End If

UseVPMDMD=DesktopMode
Sub ImplicitDMD_Init
   Me.x = 30
   Me.y = 30
   Me.width = 128 * 2
   Me.height = 32 * 2
   Me.visible = true
   Me.intensityScale=1.2
   Me.fontColor = RGB(102,255,255)
End Sub

'---------------
'Debug stuff
Dim DebugFlippers : DebugFlippers = False
dim aDebugBoxes : aDebugBoxes = array(TBflipper, TBgi,TBn,TBn1,TB2,tb1,TBS,TBBounces,TbPL)
Sub DebugF(input)
	dim x
	if input > 4 then for each x in aDebugBoxes : x.visible = True : next : Exit Sub
	debugflippers = cbool(input)
	destroyer.enabled = cbool(input)
	for each x in aDebugBoxes : x.visible = cbool(input) : next 
	for each x in aDebugBoxes : x.TimerEnabled = cbool(input) : next 
	tb.text = " "
end sub
Debugf 5
Debugf 0
Sub Gimme():k0358k	20, 5	, ballmass: debugf 1:End Sub 'gimme a ball
Sub GimmeF():k0358k	-60, 10	, ballmass: debugf 1:End Sub 'gimme a ball to L flipper
Sub GimmeT():KickMe KickerTrough, 0, 0	, ballmass: debugf 1:End Sub 'gimme a ball to Trough
'debugf 0
sub KickMe(object, dir,vel,mass) : object.createsizedballwithmass 25, mass : object.kick dir, vel : debugf 1 :end sub

'fix buggy autoplunger
Sub ShooterHack_Hit() : if activeball.vely > -20 then exit sub end if : if activeball.x < 895 then activeball.x = 895 end if: End Sub	

'EOStimer (just switches elast falloff)
'============
eostimer.enabled = 1
'LFHM physics by wrd1972 and Rothbauer
dim EOSAngle,ElastFalloffUp,ElastFalloffDown

'This rules but it would be way better if it was elasticity and not elast falloff
ElastFalloffup = LeftFlipper.ElasticityFalloff
ElastFalloffdown = 0.25'0.7

'EOS angle
EOSAngle = 4

'Flipper EOS timer (HMLF)
dim LastAngle1, LastAngle2, LastAngle3
Sub eostimer_Timer()	'use -1 timer interval for this?
	If LeftFlipper.CurrentAngle <> LastAngle1 then	'slight optimization
		If LeftFlipper.CurrentAngle < LeftFlipper.EndAngle + EOSAngle Then
			LeftFlipper.ElasticityFalloff = ElastFalloffup
		Else
			LeftFlipper.ElasticityFalloff = ElastFalloffdown	' This works for flippers :D
		End If
	End If
	If RightFlipper.CurrentAngle <> LastAngle2 then
		If RightFlipper.CurrentAngle > RightFlipper.EndAngle - EOSAngle Then
			RightFlipper.ElasticityFalloff = ElastFalloffup
		Else
			RightFlipper.ElasticityFalloff = ElastFalloffdown
		End If
	End If
	If RightFlipper1.CurrentAngle <> LastAngle3 then
		If RightFlipper1.CurrentAngle > RightFlipper1.EndAngle - EOSAngle Then
			RightFlipper1.ElasticityFalloff = ElastFalloffup
		Else
			RightFlipper1.ElasticityFalloff = ElastFalloffdown
		End If
	End If
	LastAngle1 = LeftFlipper.CurrentAngle
	LastAngle2 = RightFLipper.CurrentAngle
	LastAngle3 = RightFlipper1.CurrentAngle
End Sub	


'________________________________________________________
'    _____                      ______                   
'    /    '    /    ,             /                      
'---/__-------/------------__----/--------__----__---_/_-
'  /         /    /      /   )  /       /___)  (_ `  /   
'_/_________/____/______/___/__/_______(___ __(__)__(_ __
'                      /                                                         
'Setup - 			  /
'Four Kickers named kiL, kL, kR, kiR placed in inlanes and on flippers
'Primitive FlipStick (make it a flat vertical stick with >1 opacity)
'Timer "FlipTest2"
'textbox "tb2"


dim FlipperLagSeqStep : FlipperLagSeqStep = 0
dim FlipType, FlipDelay, FlipDir :flipdir = 0
'30 = Left Spinner : 600MS / '491~ish X coord
sub FlipTest(Frames, delay, dir)
	DebugF 1
	tb2.text = frames
	FlipDir = Dir
	if dir = 2 then 
		kR.CreateSizedBallWithMass 25, ballmass : Kr.Kick -2, 5
	Else
		KickerT2.CreateSizedBallWithMass 25, ballmass : KickerT2.Kick 2, 5
	end if
	FlipType = Frames
	FlipDelay = delay
'	RightFlipper.RotateToEnd
	SolBFlipper True
	FlipStep = -100
	FlipTimer.Interval = 20
	FlipTimer.Enabled = 1
	Rtest.Enabled = 1
End Sub
Dim FlipStep : FlipStep = 0
Sub FlipTimer_Timer()
	Select Case FlipSTep
		Case 1 : 	SolBFlipper False : playsound "DropTarget"
'		case 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29 : 
		case FlipType
			if FlipDelay > 0 then me.Interval = FlipDelay : FlipStep = FlipStep + 1 : Exit Sub
			SolBFlipper True : playsound "DropTarget"
		case FlipType+1 : 	SolBFlipper True : playsound "DropTarget" : me.Interval = 20
		case fliptype+10 : 	SolBFlipper False : me.Enabled = 0
	End Select
	TrackFlip
	FlipStep = FlipStep + 1
End Sub

Sub TrackFlip()
	On Error Resume Next
	if FlipStep = FlipType then tb2.text = FlipBall.X & vbnewline & flipball.Y
End Sub

dim FlipBall
Sub Rtest_Hit()
	Rtest.Enabled = 0
	Set FlipBall = ActiveBall
	FlipBall.Color = RGB(255,25,25)
End Sub

'if 0 + 5 > 5
'if 0 > 5 - 5

dim FTSball : set FTSball = Nothing
dim FlipDirV, FlipDelayV
Sub FTS(dir, input) 'hopefully more accurate flipper test sub
	debugf 1
	dim x : x = 0
	FlipDir = dir
	FlipDelayV = input
	Select Case dir
		case 0 : Set FTSball = kil.createsizedballwithmass(25, ballmass) : kil.kick 0, 0
		case 1 : Set FTSball = KickerT2.CreateSizedBallWithMass(25, ballmass) : KickerT2.Kick 2, 5 : SolBFlipper True : x = 2000
		case 2 : Set FTSball = kR.CreateSizedBallWithMass(25, ballmass) : Kr.Kick -2, 5 : SolBFlipper True : x = 2000
		case 5 : Set FTSball = kUF.CreateSizedBallWithMass(25, ballmass) : kUF.Kick 0, 0 : flipdir = 5	'upper flipper
		case Else : Set FTSball = kir.createsizedballwithmass(25, ballmass) : kir.kick 0, 0 : flipdir = 3' : SolBFlipper True : x = 2000
	End Select
	if x > 0 then 
		FlipDelayT1.Interval = x
		FlipDelayT1.Enabled = 1
	Else
		FlipTestV input	'fire flipper after a delay
	end if
End Sub

Sub FlipDelayT1_Timer(): SolBFlipper False : FlipTestV FlipDelayV : me.enabled = 0 : End Sub


Sub SolBFlipper(enabled)
	On Error Resume Next
	FlipStick.y = 1914.066 : FlipStick.RotZ = 0
	select case FlipDir
		case 2, 3 : SolRFlipper enabled : x = (FTSball.x-RightFlipper.X) / (EndPointR - rightflipper.x)
		case 5 : SolURFlipper enabled : x = (FTSball.x-RightFlipper1.X) / (773.7165 - rightflipper1.x)
		case else : SolLFlipper enabled : x = (FTSball.x-LeftFlipper.X) / (EndPointL - LeftFlipper.x)
	end select
	if not enabled then exit sub
	FlipStick.x = FTSball.x : FlipStick.Visible = True
	if FlipDir = 5 then FlipStick.y = FTSball.y : FlipStick.RotZ = -75
	x = RoundPercent(x)
	tb2.text = tb2.text & vbnewline & "%" & x' & vbnewline & "  flip: " & LeftFlipper.x & " ball:" & FTSball.x
End Sub

Function RoundPercent(input) : RoundPercent = mid(input, 1, 7)*100 : End Function	'round and mult by 100 for percentage

dim FlipInputV
Sub FlipTestV(input)
	tb2.text = "                          " & input & " MS"
	FlipTest2.Interval = input : FlipTestOn = True
	FlipTest2.Enabled = 1
End Sub

dim FlipTestOn
Sub FlipTest2_Timer()
	if FlipTestOn then 
		SolBflipper True : me.interval = 100 : flipteston = False
	Else
		SolBflipper False
		me.enabled = 0
	end if
End Sub



'====ELASTICITY========================================================
' _______      ___       __       __        ______    _______  _______ 
'|   ____|    /   \     |  |     |  |      /  __  \  |   ____||   ____|
'|  |__      /  ^  \    |  |     |  |     |  |  |  | |  |__   |  |__   
'|   __|    /  /_\  \   |  |     |  |     |  |  |  | |   __|  |   __|  
'|  |      /  _____  \  |  `----.|  `----.|  `--'  | |  |     |  |     
'|__|     /__/     \__\ |_______||_______| \______/  |__|     |__|                                                                           
'======================================================================
'This script doesn't track ball angle like Jimmyfinger's script does so it may cause some weird ball movement on glancing shots
'Usage - Define a falloff line with two points, and then a string for debug purposes
'Falloffn x1,y1,x2,y2, DebugString
'(x = Input velocity, y = output Coef)
'Debug box "TBbounces"
'Usage: falloffsimple 1,1, 54,0.5, "Targets"
'TODO might add 3 point and 5 point envelopes

' REPLACED WITH TARGETBOUNCER


'Sub FalloffSimple(X1, Y1, X2, Y2, DebugString)	'Two points	
'	Dim FinalSpeed : FinalSpeed = BallSpeed(ActiveBall) : if FinalSpeed < X1 then Exit Sub	'Cutoff Low
'	Dim BounceCoef : BounceCoef = SlopeIt(FinalSpeed,X1,Y1,X2,Y2)
'		if BounceCoef < Y2 then BounceCoef = Y2	: DebugString = DebugString & vbnewline & "Clamped" 'Clamp High
'		activeball.velx = activeball.velx * BounceCoef
'		activeball.vely = activeball.vely * BounceCoef
'		
'		DebugString = "FalloffSimple " & Debugstring & vbnewline
'		FalloffDebugBox TBbounces, Finalspeed, BallSpeed(ActiveBall), BounceCoef, DebugString
'End Sub
'
'Sub FalloffDebugBox(object, input,output,Coef,debugstring)	'Debug Box
'	'if not debugflippers then Exit Sub
'	object.Text = Debugstring & round(input,4) & vbnewline & round(output,4) & vbnewline & "%" & round(coef,4)
'	object.TimerEnabled = 1
'End Sub
'TBbounces.TimerInterval = 3000	'reset debug textbox after this interval
'Sub TBbounces_Timer():me.timerenabled = 0 : me.text = Empty : End Sub
'TBFlipper.TimerInterval = 5000	'reset debug textbox after this interval
'Sub TBFlipper_Timer():me.timerenabled = 0 : me.text = Empty : End Sub



'dim v1, v2, v3, v4
'dim b1, b2, b3, b4
'dim n1, n2, n3, n4
''debugf 1
'setit 18, 1, 65, 0.4	'pegs
'setitt 18, 1, 60, 0.25	'post
'setitR 2, 1, 50, 0.4	'Targets
'
'Sub setit(input1, input2, input3, input4)	: v1 = input1 : v2 = input2 : v3 = input3 : v4 = input4 : End Sub
'Sub setitt(input1, input2, input3, input4)	: b1 = input1 : b2 = input2 : b3 = input3 : b4 = input4 : End Sub
'Sub setiTr(input1, input2, input3, input4)	: n1 = input1 : n2 = input2 : n3 = input3 : n4 = input4 : End Sub


' Flippers
Const ReflipAngle = 20
  
Sub SolLflipper(Enabled)
     If Enabled Then
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If                
		LeftFlipper.RotateToEnd
		ProcessballsL
     Else
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
		LeftFlipper.RotateToStart
     End If
  End Sub
  
Sub SolRflipper(Enabled)
     If Enabled Then
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
		RightFlipper.RotateToEnd
		ProcessballsR
     Else
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If        
		FlipperRightHitParm = FlipperUpSoundLevel
		RightFlipper.RotateToStart
     End If
End Sub

Sub SolURflipper(Enabled)
     If Enabled Then
		RightFlipper1.RotateToEnd
		TriggerRF1.TimerEnabled = 1
		RF1on = True
     Else
		RightFlipper1.RotateToStart
     End If
End Sub


' Flipper Collide sounds
Sub LeftFlipper_Collide(parm)
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm) 
	RightFlipperCollide parm
End Sub

Sub RightFlipper1_Collide(parm) 
	RightFlipperCollide parm
End Sub

'-----------------

Const UseVPMModSol = 1
Dim BallMass : BallMass = 1.65

LoadVPM "01560000", "sam.VBS", 3.16

Const UseSolenoids = 1
Const UseLamps = 0
Const UseSync = 0
Const HandleMech = 1'?
Const UseGI = 1
Const SCoin = ""

Set GICallback = GetRef("GIon")	'This handles tilts but nothing else. The real GI callbacks are buried in the chglamp array

'Dim UseVPMColoredDMD : UseVPMColoredDMD = True
dim BIP: BIP = 0
dim BsKickout, vMag, imAutoplunger

Sub Table1_Init
    vpmInit Me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "Star Trek Enterprise Edition (Stern 2012)"
        .Games(cGameName).Settings.Value("rol") = 0 'set it to 1 to rotate the DMD to the left
'        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
		.Hidden = 0
		if SingleScreenFs and Not DesktopMode then .Games(cGameName).Settings.Value("rol") = 1
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
    End With

    ' Nudging
    vpmNudge.TiltSwitch = swTilt
    vpmNudge.Sensitivity = 0.25
    vpmNudge.TiltObj = Array(bumper1, bumper2, bumper3, LeftSlingshot, RightSlingshot)

	Set BsKickout = New cvpmSaucer
	With BsKickout	
		.InitKicker sw10, 10, 85, 38, 100	'object, switch...dir force zforce%
		.InitAltKick k1, k2, k3 'down	'not tuned for hmlf
		.InitExitVariance 5, 5	'As with all other odd geometry... mesh it correctly and it will work
	End With

	Set vMag = New cvpmMagnet
	with vMag
		.InitMagnet VengMagnet, 0	'define strength with ModMagnet sub. It's in lamptimer.
		.GrabCenter = True	'False
		.Size = 135
		.CreateEvents "vMag"
	End With

	Set imAutoplunger = New cvpmImpulseP
	with imAutoplunger
		.InitImpulseP AutoPlungerTrigger, 52, 0	'trigger, power, time to full plunge
		.CreateEvents "imAutoPlunger"
		.random 3
	End With

    ' Main Timer init
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1
	Lamptimer.Enabled = 1
	
	' Init Kickback
    KickBack.Pullback
	AutoPlunger.Pullback

	'start trough
	sw19.CreateSizedBallWithMass 25, BallMass
	sw20.CreateSizedBallWithMass 25, BallMass
	sw21.CreateSizedBallWithMass 25, BallMass
	sw22.CreateSizedBallWithMass 25, BallMass
	BallSearch

End Sub

dim K1, k2, k3
k1 = 190 : k2 = 26 : k3 = 25 

Sub table1_Paused:Controller.Pause = 1 : End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub Table1_exit():Controller.Pause = False:Controller.Stop:End Sub

sub Destroyer_hit:me.destroyball:end sub	'debug ball destroyer

dim Catchinput(1)
Sub table1_KeyDown(ByVal Keycode)

	If Keycode = LeftFlipperKey Then 
		PinCab_Flipper_Left.X = PinCab_Flipper_Left.X + 8
		If DebugFlippers Then 
			SolLFlipper true
			Exit Sub
		End If
	End If

 	If Keycode = RightFlipperKey then 
		PinCab_Flipper_Right.X = PinCab_Flipper_Right.X - 8
		If Not StagedFlippers Then
			controller.switch(86) = 1
		End If
		If DebugFlippers Then 
			SolRFlipper true
			SolURFlipper true
			Exit Sub
		End If
	End If

	If keycode = LeftTiltKey Then vpmNudge.donudge 90, 3.5:SoundNudgeLeft(): Exit Sub
	If keycode = RightTiltKey Then vpmNudge.donudge 270, 3.5:SoundNudgeRight(): Exit Sub
	If keycode = CenterTiltKey Then vpmNudge.doNudge 0, 4:SoundNudgeCenter(): Exit Sub

    If keycode = PlungerKey Then 
		Plunger.Pullback
		SoundPlungerPull()
		TimerVRPlunger.Enabled = True
		TimerVRPlunger2.Enabled = False
	End If
	
	If keycode = LeftMagnaSave Then catchinput(0) = True
	If keycode = RightMagnaSave Then catchinput(1) = True

	If keycode = StartGameKey Then soundStartButton()

	If StagedFlippers then 
		If Keycode = UpperFlipperKey then controller.switch(86) = 1
	End If

	If Keycode = PunchItKey then 
		Controller.Switch(71) = 1
		PlaySound "fx_ButtonClick" : setlamp 492, 0  
		If PunchItKey = LockBarKey Then Exit Sub 'For when this inevitably gets added to Sam.VBS
	End If

	If Keycode = KeyRules Then 
		If DesktopMode Then
			Setlamp 491, 1
		Else
			Setlamp 490, 1
		End If
		Exit Sub
	End If

'	if keycode = 31 then bsKickout.ExitSol_On' : light1.state = 1

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

'	if keycode = 208 then mofo RndNum(0, 2)	'downarrow vengeance animation test

	If vpmKeyDown(keycode) Then Exit Sub

End Sub

Sub table1_KeyUp(ByVal Keycode)

 	If Keycode = LeftFlipperKey then 
		PinCab_Flipper_Left.X = PinCab_Flipper_Left.X - 8
		if DebugFlippers then 
			SolLFlipper false	'debug
			Exit Sub
		End If
	End If

 	If Keycode = RightFlipperKey then 
		PinCab_Flipper_Right.X = PinCab_Flipper_Right.X + 8
		If Not StagedFlippers Then
			controller.switch(86) = 0
		End If
		if DebugFlippers then 
			SolRFlipper false
			SolURFlipper false
			Exit Sub
		End If
	End If

	If keycode = LeftMagnaSave then catchinput(0) = False
	if keycode = RightMagnaSave then Catchinput(1) = False

	If StagedFlippers then 
		If Keycode = UpperFlipperKey then controller.switch(86) = 0
	End If

    If keycode = PlungerKey Then 
		Plunger.Fire
		TimerVRPlunger.Enabled = False
		TimerVRPlunger2.Enabled = True
		PinCab_Plunger.Y = -8
		If bipl.BallCntOver > 0 Then 
			SoundPlungerReleaseBall() 
		Else
			SoundPlungerReleaseNoBall() 
		End If
	End If

	if Keycode = KeyRules then 
		If DesktopMode Then
			Setlamp 491, 0
		Else
			Setlamp 490, 0
		End If
		Exit Sub
	End If

	If Keycode = PunchItKey then 
		Controller.Switch(71) = 0 
		setlamp 492, 1
		if PunchItKey = LockBarKey then Exit Sub 'For when this inevitably gets added to Sam.VBS
	End If

    If vpmKeyUp(keycode) Then Exit Sub

End Sub

' Drops
Sub k505k(x, y, z) : k505.createsizedballwithmass 25, z : k505.Kick x, y : End Sub 
Sub k1025k(x, y, z) : k505.createsizedballwithmass 25, z : k505.Kick x, y : End Sub 

Sub k1027k(x, y, z) : k0254.createsizedballwithmass 25, z : k0254.Kick x, y : End Sub 
Sub k1337k(x, y, z) : k1337.createsizedballwithmass 25, z : k1337.Kick x, y : End Sub 
Sub k1410k(x, y, z) : k1410.createsizedballwithmass 25, z : k1410.Kick x, y : End Sub 
Sub k1730k(x, y, z) : k1730.createsizedballwithmass 25, z : k1730.Kick x, y : End Sub 
Sub k1939k(x, y, z) : k1939.createsizedballwithmass 25, z : k1939.Kick x, y : End Sub 
Sub k2004k(x, y, z) : k2004.createsizedballwithmass 25, z : k2004.Kick x, y : End Sub 
Sub k2021k(x, y, z) : k2021.createsizedballwithmass 25, z : k2021.Kick x, y : End Sub 
'Sub k2118k(x, y, z) : k2118.createsizedballwithmass 25, z : k2118.Kick x, y : End Sub 
Sub k2245k(x, y, z) : k2245.createsizedballwithmass 25, z : k2245.Kick x, y : End Sub 

Sub k2347k(x, y, z) : k2347.createsizedballwithmass 25, z : k2347.Kick x, y : End Sub 


'Sub k0242k(x, y, z) : k0242.createsizedballwithmass 25, z : k0242.Kick x, y : End Sub 
Sub k0242k(x, y, z) : k2021.createsizedballwithmass 25, z : k2021.Kick x, y : End Sub 
Sub k0254k(x, y, z) : k0254.createsizedballwithmass 25, z : k0254.Kick x, y : End Sub 
Sub k0358k(x, y, z) : k0358.createsizedballwithmass 25, z : k0358.Kick x, y : End Sub 
Sub k0458k(x, y, z) : k0458.createsizedballwithmass 25, z : k0458.Kick x, y : End Sub 
'Sub k0500k(x, y, z) : k0500.createsizedballwithmass 25, z : k0500.Kick x, y : End Sub 
Sub k0500k(x, y, z) : k0358.createsizedballwithmass 25, z : k0358.Kick x, y : End Sub 
Sub k0609k(x, y, z) : k0609.createsizedballwithmass 25, z : k0609.Kick x, y : End Sub 
'Sub k0709k(x, y, z) : k0709.createsizedballwithmass 25, z : k0709.Kick x, y : End Sub 
'Sub k1007k(x, y, z) : k1007.createsizedballwithmass 25, z : k1007.Kick x, y : End Sub 
Sub k1013k(x, y, z) : k1730.createsizedballwithmass 25, z : k1730.Kick x, y : End Sub 
Sub k1121k(x, y, z) : k0254.createsizedballwithmass 25, z : k0254.Kick x, y : End Sub 
Sub k1449k(x, y, z) : k1449.createsizedballwithmass 25, z : k1449.Kick x, y : End Sub 
Sub k1634k(x, y, z) : k1634.createsizedballwithmass 25, z : k1634.Kick x, y : End Sub 
Sub k1846k(x, y, z) : k0609.createsizedballwithmass 25, z : k0609.Kick x, y : End Sub 
Sub k2016k(x, y, z) : k2016.createsizedballwithmass 25, z : k2016.Kick x, y : End Sub 
Sub k2203k(x, y, z) : k2016.createsizedballwithmass 25, z : k2016.Kick x, y : End Sub 


' Trough Handling
SolCallback(1) = "TroughOut"
Sub TroughOut(enabled): if Enabled then sw22.Kick 55, 4 :Playsound SoundFX("BallRelease",DOFcontactors): Else Ballsearch : End If: end Sub

'Trough Switches
Sub sw18_hit():controller.Switch(18) = 1 : UpdateTrough : RandomSoundDrain sw18 : BIP = BIP - 1: End Sub
Sub sw19_hit():controller.Switch(19) = 1 : UpdateTrough : End Sub
Sub sw20_hit():controller.Switch(20) = 1 : UpdateTrough : End Sub
Sub sw21_hit():controller.Switch(21) = 1 : UpdateTrough : End Sub
Sub sw22_hit():controller.Switch(22) = 1 : UpdateTrough : End Sub
Sub sw18_UnHit():controller.Switch(18) = 0 : UpdateTrough : End Sub
Sub sw19_UnHit():controller.Switch(19) = 0 : UpdateTrough : End Sub
Sub sw20_UnHit():controller.Switch(20) = 0 : UpdateTrough : End Sub
Sub sw21_UnHit():controller.Switch(21) = 0 : UpdateTrough : End Sub
Sub sw22_UnHit():controller.Switch(22) = 0 : UpdateTrough : BIP = BIP + 1: End Sub

Sub UpdateTrough: TroughTimer.enabled = 1 : TroughTimer.Interval = 128: end sub

Sub TroughTimer_Timer()
	If sw22.BallCntOver = 0 then sw21.kick 58, 12
	If sw21.BallCntOver = 0 then sw20.kick 58, 12
	If sw20.BallCntOver = 0 then sw19.kick 58, 12
	If sw19.BallCntOver = 0 then sw18.kick 58, 15
End Sub

Sub BallSearch	'Call in case of hard pinmame reset
'	tb.text = "Ballsearch"
	if sw18.BallCntOver > 0 then controller.Switch(18) = 1 else controller.Switch(18) = 0
	if sw19.BallCntOver > 0 then controller.Switch(19) = 1 else controller.Switch(19) = 0
	if sw20.BallCntOver > 0 then controller.Switch(20) = 1 else controller.Switch(20) = 0
	if sw21.BallCntOver > 0 then controller.Switch(21) = 1 else controller.Switch(21) = 0
	if sw22.BallCntOver > 0 then controller.Switch(22) = 1 else controller.Switch(22) = 0
	if sw10.BallCntOver > 0 then controller.Switch(10) = 1 else controller.Switch(10) = 0
End Sub

'==============

'GI
sub GIon(no, Value)
'	tbgi.text = "GICallback:" & no & value & vbnewline & gametime 'debug box
	select case no
		case 0
			SetLamp 499, Value
	end select
End sub
sub gioff : gion 0, false : end sub	'debug command


'Solenoids part 1
'..................

SolModCallBack(2) = "AutoPlunge"

sub BIPL_Hit() : Activeball.Vely = activeball.vely * 0.625 : End Sub 'autoplunger debounce

dim PlungerState : PlungerState = False
Sub Autoplunge(Value)
'	tbp.text = Value
	if Value > 0 then 
		AutoPlunger.Fire
		imAutoplunger.AutoFire
		If PlungerState = False Then
			If BIPl.BallCntOver > 0 then 
				SoundPlungerReleaseBall() 
				PlaySoundAt SoundFX("TrapDoorHigh",DOFContactors), Autoplunger
			Else
				PlaySoundAt SoundFX("TrapDoorHigh",DOFContactors), Autoplunger
			End If
			PlungerState = True
		End If
	Else
		PlungerState = False
		AutoPlunger.Pullback
	End If
End Sub

SolModCallback(3) = "SetModLamp 0," 			' Magnet handled by fading script

SolModCallback(17) = "SetModLamp 417,"			' Left Asteroid
SolModCallback(18) = "SetModLamp 418,"			' Right Asteroid
SolModCallback(19) = "SetModLampm 419, 469,"	' Left Flasher Cap
SolModCallback(20) = "SetModLampm 420, 470,"	' Right Flasher Cap
SolModCallback(21) = "SetModLamp 421,"			' Kickback Flasher (vengeance Kickback)
'22 laser Motor
SolModCallback(23) = "SetModLamp 423,"			' Flash Ramp (Left)
'24 unused
SolModCallback(25) = "SetModLamp 425,"			' Bumper Flasher
SolModCallback(26) = "SetModLamp 426,"			' Warp Ramp Entrance
SolModCallback(27) = "SetModLamp 427,"			' Center three bank
SolModCallback(28) = "SetModLamp 428,"			' Flash Ramp (Right)
SolModCallback(29) = "SetModLamp 429,"			' Left Loop
SolModCallback(30) = "SetModLamp 430,"			' Flash Upper Flipper
SolModCallback(31) = "SetModLamp 431,"			' Vengeance Flasher
SolModCallback(32) = "SetModLamp 432,"			' Left Sling

SolModCallback(59) = "SetModLamp 459,"			' Right Sling	'Aux board flasher 41
'60 through 64 - AUX backbox flashers

SolCallback(4) = "DropUp"
SolCallback(5) = "DropDown"

dim PopBall, PopBallz : Set PopBall = Nothing : Set PopBallz = Nothing
Sub DTpop_Hit() : Set PopBall = Activeball : End Sub	'pop captured ball back when DT drops
Sub DTpop_UnHit() : Set PopBall = Nothing : End Sub
Sub DTpopz_Hit() : Set PopBallz = Activeball : End Sub	'pop ball up when DT raises
Sub DTpopz_UnHit() :Set PopBallz = Nothing : End Sub

'sw11col1 = forward part sw11col2 = back part
'anim notes
'rotx animation on #493
'sw11p.rotx = 180
'sw11p.rotx = 185
'z animation on #494
'sw11p.z = -35
'sw11p.z = -85

Sub DropUp(enabled)
'	tb4.text = "dropup: " & enabled
	if Enabled Then
'		Sw11p.isdropped = 0
		setlamp 494, 1
		sw11col1.Collidable = True
		sw11col2.Collidable = True
'		sw11.isDropped = 0
		PlaySoundAt SoundFX("ResetDrop",DOFContactors), sw11p
		Controller.Switch(11) = 0
		On Error Resume Next
'		PopBallZ.Velx = PopBallZ.Velx / 4
'		PopBallZ.Vely = PopBallZ.Vely / 4
		PopBallZ.Z = 50
		PopBallZ.VelZ = 25
		PopBall.VelY = RndNum(15, 25)
	End If
End Sub

Sub DropDown(enabled)
'	tb5.text = "dropdwn: " & enabled
	if Enabled Then
		setlamp 494, 0
'		Sw11p.isdropped = 1
		sw11col1.Collidable = False
		sw11col2.Collidable = False
'		sw11.isDropped = 1
		PlaySoundAt SoundFX("DropTarget",DoFContactors), sw11col1
'		playsound SoundFX("ResetDrop",DOFContactors), 0, LVL(0.1)
		Controller.Switch(11) = 1

		On Error Resume Next
		PopBall.VelY = RndNum(-5, -13)
		PopBall.Velx = PopBall.Velx + RndNum(-2, 2)
	End If
End Sub

Sub Sw11col1_Hit()
	setlamp 493, 1	'wiggle
'	controller.Switch(11)=1 
	vpmtimer.Pulsesw 11
'	Playsound SoundFX("DropTarget",DoFContactors), 0, LVL(1)

	On Error Resume Next
'	PopBallZ.VelZ = 25
	PopBall.VelY = RndNum(-5, -13)	'without the proper DT this is basically just dirty pool
	PopBall.Velx = PopBall.Velx + RndNum(-2, 2)
End Sub

'Sub sw11_Dropped : vpmtimer.Pulsesw 11 : End Sub
'Sub Sw11p_Dropped 
'	controller.Switch(11)=1 
'	Playsound SoundFX("DropTarget",DoFContactors), 0, LVL(1)
'	On Error Resume Next
''	PopBallZ.VelZ = 25
'	PopBall.VelY = RndNum(-5, -13)
'	PopBall.Velx = PopBall.Velx + RndNum(-2, 2)
'End Sub
'sub tb_timer():me.text = controller.Switch(11) : end sub	'debug

'---------------Rotating Scoop Handling
Sub InitScoop
	'Special Fading Number Glossary
	'0 	 - Magnet Fade
	'...some other stuff...
	'497 - Vengeance (SolModValue Only)
	'498 - Rotating Scoop
	'499 - GI Fade
	'500 - Scoop ball-Fall-in animation (runs on sw10 timer, not Lamptimer)
	FlashLevel(500) = 0	'ball fall-in animation
	FadingLevel(500) = 1
	FlashSpeedUp(500) = 0.3
	FlashMax(500) = 26.5

	FlashLevel(498) = 10	'Rotating Scoop
	FadingLevel(498) = 1
	FlashSpeedUp(498) = 2.5
	FlashSpeedDown(498) = 1.5
	FlashMin(498) = 10
	FlashMax(498) = 90
End Sub

SolCallback(55) = "ScoopR" 	' Rotating Kickout Scoop

Sub ScoopR(enabled) 
	if Enabled then 
		SetLamp 498, 1
'		ScoopTimer.Enabled = 1
		PlaySoundAt SoundFX("DiverterRight",DOFcontactors), kUF
	else 
		SetLamp 498, 0
		PlaySoundAt SoundFX("FlipperDown",DOFcontactors), kUF
	end if
end sub

Sub ObjRotation(nr, Object, Direction)	'flashmin and flashmax determine angle
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * CGT)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
               FadingLevel(nr) = 0 'completely off
            End if
            Object.RotZ = FlashLevel(nr) * Direction
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr) * CGT)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.RotZ = FlashLevel(nr) * Direction
    End Select
End Sub

SolCallback(6) = "ScoopKick"	'sw10 Left Kickout
Sub ScoopKick(Enabled)
	If Enabled then
		If Sw10.BallCntOver = 1 then
'			LampState(498) = 1 ' debug, test kickouts
			If LampState(498) = 1 then 'If Scoop out of position...
				BsKickout.ExitSol_On
				SoundSaucerKick 1, sw10
			Else
				BsKickout.ExitAltSol_On
				SoundSaucerKick 1, sw10
			End If
		Else
			PlaysoundAt SoundFX("SafeHouseKick",DOFContactors), sw10
			BallSearch
		End If
	Else
'		Sw10.Enabled = 1
	End If
End Sub

dim ScoopBall

Sub sw10_hit : Set ScoopBall = Activeball : Setlamp 500, 1 : me.timerenabled = 1 : SoundSaucerLock : end sub
Sub sw10_Timer()	'Modified fading routine to handle scoop entry animation
	dim x, nr, offset : nr = 500 : offset = 50
	Select Case FadingLevel(nr)
		Case 1
			me.TimerEnabled = 0
			bsKickout.addball me
			Setlamp 500, 0
		Case 5	'fade
            FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr) * CGT)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            ScoopBall.Z = FlashLevel(500) * -1 + offset
			ScoopBallDebug = ScoopBall.Z + offset
	End Select
'	ScoopBallAnim 500, ScoopBall, 25
end sub

dim scoopballdebug
Sub tbScoop_Timer()	'debug
	me.text = "BallCnt: " & Sw10.BallCntOver & vbnewline &_
				"Timer? " & sw10.TimerEnabled & vbnewline &_
				"animStep: " & FadingLevel(500) & vbnewline &_
				"lvl: " & FlashLevel(500) & " " & ScoopballDebug & vbnewline &_
				"ScoopPos: " & Lampstate(498) & vbnewline &_
				"..." 
End Sub

SolCallback(7) = "VengeanceKickBack"	'Vengeance Kickout Coil

Sub VengeanceKickBack(aEnabled)
	If aEnabled then 
		VengKicker.Enabled = aEnabled 
	Else
		VengKicker.TimerInterval = 128	'Bit of lag compensation
		VengKicker.TimerEnabled = 1
	End If
End Sub
Sub VengKicker_Timer() : Me.TimerEnabled = 0 : Me.Enabled = 0 : PlaysoundAt SoundFX("GunPopper",DOFContactors), VengKicker:End Sub
Sub VengKicker_Hit() : Me.Kick RndNum(167, 180), RndNum(40,45), RndNum(14,16) : PlaysoundAt SoundFX("GunShot",DOFContactors), VengKicker: End Sub
'base kick angle 173 '-6 '+5


SolCallback(8) = "ShakerMotor"'Shaker Motor
Sub ShakerMotor(enabled): If Not Shaker then : Exit Sub : End If : If Enabled then Playsound SoundFX(" ", DOFShaker) else Stopsound SoundFX(" ", DOFShaker) End If : End Sub

'9-11 Pop Bumpers, 13 & 14 Slings
SolCallback(12) = "SolURflipper"
SolCallBack(15) = "SolLflipper"
SolCallBack(16) = "SolRflipper"


SolCallback(51) = "LeftGateCoil"
Sub LeftGateCoil(aEnabled) : LeftGate.Open = aEnabled : End Sub
SolCallback(52) = "RightGateCoil"
Sub RightGateCoil(aEnabled) : RightGate.Open = aEnabled : End Sub

SolCallback(54) = "KickBackCoil" 
Sub KickBackCoil(enabled) : If enabled Then kickback.fire : PlaySoundAt SoundFX("TrapDoorHigh",DOFContactors), kickback Else : kickback.Pullback : End If : End Sub

'Solenoids part 2, Flash Lamps
		

SolCallback(22) = "LaserMotor"
Sub LaserMotor(enabled) 
	if Enabled then 
		LaserN.visible = 1
		LaserNt.Enabled = 1
	Else 
		LaserN.visible = 0
		LaserNt.Enabled = 0
	End If 
End Sub
LaserMotor False

'LaserMotor True	'debug
dim checkintensity
Sub LaserNt_Timer()
	FlashSeq Lasern, "Stars", 15
	'if CheckIntensity = GIscale then exit Sub else Lasern.IntensityScale = GIscale ^ 2 : End If
	'CheckIntensity = GIscale
End Sub

dim LaserStep : LaserStep = 0

Sub FlashSeq(object, imgseq, steps)	'Primitive texture image sequence
	dim x, x2
	object.ImageA = imgseq & "_" & LaserStep
	Laserstep = LaserStep + 1
	if Laserstep > steps-1 then Laserstep = 0
End Sub
	

SolCallback(56) = "VengeanceLatch"

Sub VengeanceLatch(Enabled)
'	Latch = Enabled

	If Enabled then 
'		If Latch then Latch = False Else Latch = True	'coil toggles latch on and off?
		If Not Latch then
			If VengPos = 1 then
				ReturnSequence.Interval = 100
				ReturnSequence.Enabled = 1 'must sequence properly if the vengeance is mid-animation...
			End If
		End If
		Latch = False	'try2 Disables latch, which is automatically engaged when the Vengeance passes over it?
	End If
'	If Not Latch then
'		If VengPos = 1 then
'			ReturnSequence.Interval = 100
'			ReturnSequence.Enabled = 1 'must sequence properly if the vengeance is mid-animation...
'		End If
'	End If

	'tbV2.text = "veng Coil: " & Enabled & vbnewline & "Latch? " & Latch

'	Latch = Enabled
'
'	If enabled then
'		'
'	Else
'		If VengPos = 1 then
'			ReturnSequence.Interval = 100
'			ReturnSequence.Enabled = 1 'must sequence properly if the vengeance is mid-animation...
'		End If
'	End If
 End Sub 


SolModCallback(53) = "VengeanceAnimCoil"	'Vengeance Animation

dim VengPos : VengPos = 0 '0 = Up, 1 = down, 5 = Animation in progress
dim Latch : Latch = False	'True
'	tbV2.text = "Latch? " & Latch


Sub VengeanceAnimCoil(Value) 'could def use bool for this
	SolModValue(496) = Value
	'tbv1.text = "Vng: " & Value

'	Controller.Switch(53) = cBool(Value)

	If Value > 1 then 'Coil fired....
		ReturnSequence.Interval = 100
		if VengPos = 0 Then
'			If Latch then	'regardless...
	'		tbv3.text = "Anim- Dive Down" ': VengPos = 1 'temp simulation
			Latch = True 'try2 Disables latch, which is automatically engaged when the Vengeance passes over it?
			VengAnimH 2
		Elseif VengPos = 1 then 
'			If Latch then
				VengAnimH 101
	'			tbv3.text = "Anim- Down, Shake"
		End If
	ElseIf Value = 0 then 'Coil Return...
'		tb.text = "vengoff" & vbnewline & "VengPos: " & VengPos & vbnewline & "Latch" & Latch
'		If VengPos = 1 Then
			If not Latch then
'				tb.text = "vengoff 2"
'				ReturnStep = 0
				ReturnSequence.Interval = 100
				ReturnSequence.Enabled = 1 'must sequence properly if the vengeance is mid-animation...
'				tbv3.text = "Anim- Return"
			End If
'		End If
	End If
End Sub


ReturnSequence.Interval = 100
'dim ReturnStep :returnstep = 0
Sub ReturnSequence_Timer() 'Lag Correction
'	If Latch then Me.Enabled = 0 : Exit Sub
'	If SolModValue(496) = 0 then tbv3.text = "Anim- Return" : Me.Enabled = 0 : VengAnimH 201 : Else : tb.text = "Return Cancelled,"  & SolModValue(496) : End If' : VengPos = 0 'temp simulation

	If VengPos = 5 then Exit Sub ' Retry, wait for animation to finish...
	If Latch then Me.Enabled = 0 : Exit Sub ' Cancel if latched
	If VengPos = 1 then 
	'	tbv3.text = "Anim- Return"
		VengAnimH 201
	End If
	




'	Select Case ReturnStep
'		Case 0, 1, 2
'			if SolModValue(496) > 1 then ReturnStep = 0 Else ReturnStep = ReturnStep + 1
'		Case 3
'			me.Enabled = 0
'			tbv3.text = "Anim- Return"
'	End Select
End Sub


dim vRotZDesired, vRotZstep
Sub VengAnimH(input)
	Select Case input
		Case 2	'down
			vRotZDesired = RndNum(3, 5)*-1
			vRotZstep = (vRotZDesired - vRotZ) / 35
			PlaySoundAt SoundFX("TrapDoorHigh_85",DOFContactors), DropKicker
		Case 101	'shake
'			vRotZ = 0
'			vRotZDesired = RndNum(3.5, 4.5)*-1'-3'RndNum(-3, 0)
'			vRotZstep = (vRotZDesired - vRotZ) / 35
'			vRotZDesired = (vRotZDesired - vRotZ) / 40
			PlaySoundAt SoundFX("TrapDoorhigh_85x2",DOFContactors), DropKicker
		Case 201	'return
'			vRotZ = 0
			vRotZDesired = 0'RndNum(-3, 0)
			vRotZstep = (vRotZDesired - vRotZ) / 45
'			vRotZDesired = (vRotZDesired - vRotZ) / 40
			PlaySoundAt SoundFX("TrapDoorLow_85",DOFContactors), DropKicker
	End Select
'	vRotZDesired = RndNum(0, 3)
	VengPos = 5
	Vstep = Input
	VengT.Enabled = 1

End Sub

Sub TBvr_Timer()
	me.text = "vRotZ: " & vRotZ & vbnewline & _ 
				"RotZ: " & Vengeance.RotZ & vbnewline & _ 
			  "vRotZDesired: " & vrotzdesired

End Sub

Sub TbvT_Timer()	'debug timer
	me.text = "Timer " & ReturnSequence.Enabled & " " &  ReturnSequence.Interval & vbnewline &_
				" . "
'				"Timer? " & sw10.TimerEnabled & vbnewline &_
'				"animStep: " & FadingLevel(500) & vbnewline &_
'				"lvl: " & FlashLevel(500) & " " & ScoopballDebug & vbnewline &_
'				"ScoopPos: " & Lampstate(498) & vbnewline &_
'				"..." 
End Sub


Sub LatchSwitch(enabled)
	If Enabled then 
		If Latch then Controller.Switch(53) = True
	Else
		Controller.Switch(53) = False
	End If
	

'	If Not Enabled then 
'		Controller.Switch(53) = enabled
'	Else
'		If Latch then 
'	If Not Latch then Exit Sub

End Sub

Sub TBVS_Timer(): me.text = "Switch53: " & Controller.Switch(53) : End Sub


Sub mofo(input)	'test vengeance animations
'	vRotZDesired = RndNum(-1, 3)

	Select Case input
		Case 0	'down
			VengAnimH 2
'			Vstep = 2
'		vRotZDesired = (vRotZDesired - vRotZ) / 48
		Case 1	'shake
			VengAnimH 101
'			Vstep = 101
'		vRotZDesired = (vRotZDesired - vRotZ) / 53
		Case 2	'return
			VengAnimH 201
'			Vstep = 201
'		vRotZDesired = (vRotZDesired - vRotZ) / 49

	End Select
'	VengT.Enabled = 1

'	dim vX, vY, vZ, vRotX
'	select case input
'		Case 0
'			vY = -590.344 : vZ = 269.54: vRotX = 	-4.891
'		case 1
'			vY = -653.421 : vZ = 242.367 : vRotX = 24.661	
'	End Select
'	vY = vY * -1
'	vRotX = vRotX - 180
'	vRotX = vRotX * -1
'	Vengeance.y = vY : Vengeance.z = vZ : Vengeance.RotX = vRotX' : Vengeance.RotZ = vRotZ
'	VengeanceLamps.y = vY : VengeanceLamps.z = vZ : VengeanceLamps.RotX = vRotX' : VengeanceLamps.RotZ = vRotZ	
End Sub
'Mofo 0


Sub TBVa_Timer()
	me.text = "VengPos " & VengPos & Vbnewline &_
				" "
End Sub


dim Vstep
Sub VengT_Timer()
	dim vX, vY, vZ, vRotX
	Select Case Vstep

	'2 to 50 -> dive down

	Case 2  : vY = -590.344: vZ = 269.540: vRotX = -4.891 : Vstep = Vstep + 1	'101
	Case 3  : vY = -594.784: vZ = 263.411: vRotX = -9.986: Vstep = Vstep + 1
	Case 4  : vY = -592.247: vZ = 262.917: vRotX = -12.655: Vstep = Vstep + 1
	Case 5  : vY = -612.554: vZ = 244.737: vRotX = -18.629: Vstep = Vstep + 1
	Case 6  : vY = -610.807: vZ = 244.125: vRotX = -20.541: Vstep = Vstep + 1
	Case 7 	: vY = -612.246: vZ = 244.634: vRotX = -18.965: Vstep = Vstep + 1
	Case 8  : vY = -616.180: vZ = 245.802: vRotX = -14.726: Vstep = Vstep + 1
	Case 9 	: vY = -621.934: vZ = 246.962: vRotX = -8.663: Vstep = Vstep + 1

	Case 10 : vY = -628.737: vZ = 247.537: vRotX = -1.609: Vstep = Vstep + 1	'109
	Case 11 : vY = -635.776: vZ = 247.250: vRotX = 5.670: Vstep = Vstep + 1
	Case 12 : vY = -642.346: vZ = 246.161: vRotX = 12.551: Vstep = Vstep + 1
	Case 13 : vY = -647.971: vZ = 244.561: vRotX = 18.592: Vstep = Vstep + 1
	Case 14 : vY = -652.425: vZ = 242.819: vRotX = 23.532: Vstep = Vstep + 1
	Case 15 : vY = -655.686: vZ = 241.249: vRotX = 27.27: Vstep = Vstep + 1
	Case 16 : vY = -657.857: vZ = 240.053: vRotX = 29.829: Vstep = Vstep + 1
	Case 17 : vY = -659.093: vZ = 239.314: vRotX = 31.317: Vstep = Vstep + 1
	Case 18 : vY = -659.572: vZ = 239.016: vRotX = 31.898: Vstep = Vstep + 1
	Case 19 : vY = -659.462: vZ = 239.085: vRotX = 31.764: Vstep = Vstep + 1

	Case 20 : vY = -658.924: vZ = 239.418: vRotX = 31.112: Vstep = Vstep + 1 '119
	Case 21 : vY = -658.105: vZ = 239.908: vRotX = 30.126: Vstep = Vstep + 1: LatchSwitch  True
	Case 22 : vY = -657.135: vZ = 240.465: vRotX = 28.97: Vstep = Vstep + 1: LatchSwitch  True
	Case 23 : vY = -656.123: vZ = 241.019: vRotX = 27.78: Vstep = Vstep + 1: LatchSwitch  True
	Case 24 : vY = -655.158: vZ = 241.522: vRotX = 26.656: Vstep = Vstep + 1
	Case 25 : vY = -654.302: vZ = 241.947: vRotX = 25.669: Vstep = Vstep + 1 : VengPos = 1 : LatchSwitch  True
	Case 26 : vY = -653.596: vZ = 242.285: vRotX = 24.861: Vstep = Vstep + 1 : VengPos = 1 : LatchSwitch  True
	Case 27 : vY = -653.057: vZ = 242.535: vRotX = 24.248: Vstep = Vstep + 1 : VengPos = 1 : LatchSwitch  True
	Case 28 : vY = -652.685: vZ = 242.703: vRotX = 23.826: Vstep = Vstep + 1 : VengPos = 1 : LatchSwitch  True
	Case 29 : vY = -652.467: vZ = 242.800: vRotX = 23.58: Vstep = Vstep + 1 : VengPos = 1 : LatchSwitch  True

	Case 30 : vY = -652.382: vZ = 242.838: vRotX = 23.483: Vstep = Vstep + 1 : VengPos = 1 : LatchSwitch  True '129
	Case 31 : vY = -652.397: vZ = 242.831: vRotX = 23.501: Vstep = Vstep + 1' : VengPos = 1 : LatchSwitch  True
	Case 32 : vY = -652.490: vZ = 242.790: vRotX = 23.605: Vstep = Vstep + 1' : VengPos = 1
	Case 33 : vY = -652.631: vZ = 242.728: vRotX = 23.764: Vstep = Vstep + 1' : VengPos = 1
	Case 34 : vY = -652.796: vZ = 242.653: vRotX = 23.951: Vstep = Vstep + 1' : VengPos = 1
	Case 35 : vY = -652.966: vZ = 242.576: vRotX = 24.145: Vstep = Vstep + 1' : VengPos = 1
	Case 36 : vY = -653.127: vZ = 242.503: vRotX = 24.327: Vstep = Vstep + 1' : VengPos = 1
	Case 37 : vY = -653.269: vZ = 242.438: vRotX = 24.488: Vstep = Vstep + 1' : VengPos = 1
	Case 38 : vY = -653.384: vZ = 242.384: vRotX = 24.619: Vstep = Vstep + 1' : VengPos = 1
	Case 39 : vY = -653.472: vZ = 242.343: vRotX = 24.720: Vstep = Vstep + 1

	Case 40 : vY = -653.533: vZ = 242.315: vRotX = 24.788: Vstep = Vstep + 1	'139
	Case 41 : vY = -653.569: vZ = 242.298: vRotX = 24.829: Vstep = Vstep + 1
	Case 42 : vY = -653.579: vZ = 242.293: vRotX = 24.842: Vstep = Vstep + 1
	Case 43 : vY = -653.584: vZ = 242.291: vRotX = 24.847: Vstep = Vstep + 1
	Case 44 : vY = -653.565: vZ = 242.300: vRotX = 24.825: Vstep = Vstep + 1
	Case 45 : vY = -653.544: vZ = 242.310: vRotX = 24.801: Vstep = Vstep + 1
	Case 46 : vY = -653.520: vZ = 242.321: vRotX = 24.774: Vstep = Vstep + 1
	Case 47 : vY = -653.492: vZ = 242.334: vRotX = 24.742: Vstep = Vstep + 1
	Case 48 : vY = -653.466: vZ = 242.346: vRotX = 24.712: Vstep = Vstep + 1
	Case 49 : vY = -653.439: vZ = 242.359: vRotX = 24.682: Vstep = Vstep + 1

	Case 50 : vY = -653.421 : vZ = 242.367 : vRotX =24.661	:me.enabled = 0				'149


	'100 - 154 - shake anim (down position)


	Case 101  : vY = -651.624: vZ = 244.842: vRotX = 25.260: Vstep = Vstep + 1 '201
	Case 102  : vY = -649.757: vZ = 247.550: vRotX = 26.185: Vstep = Vstep + 1
	Case 103  : vY = -647.836: vZ = 250.242: vRotX = 27.065: Vstep = Vstep + 1
	Case 104  : vY = -645.901: vZ = 252.694: vRotX = 27.545: Vstep = Vstep + 1
	Case 105  : vY = -644.048: vZ = 254.669: vRotX = 27.323: Vstep = Vstep + 1
	Case 106  : vY = -642.437: vZ = 255.879: vRotX = 26.188: Vstep = Vstep + 1
	Case 107  : vY = -642.468: vZ = 254.359: vRotX = 23.604: Vstep = Vstep + 1
	Case 108  : vY = -643.828: vZ = 250.085: vRotX = 19.523: Vstep = Vstep + 1
	Case 109  : vY = -644.695: vZ = 245.571: vRotX = 15.052: Vstep = Vstep + 1

	Case 110 : vY = -642.198: vZ = 246.194: vRotX = 12.395: Vstep = Vstep + 1
	Case 111 : vY = -641.232: vZ = 246.403: vRotX = 11.374: Vstep = Vstep + 1
	Case 112 : vY = -641.489: vZ = 246.350: vRotX = 11.645: Vstep = Vstep + 1: LatchSwitch  False
	Case 113 : vY = -642.631: vZ = 246.095: vRotX = 12.852: Vstep = Vstep + 1: LatchSwitch  False
	Case 114 : vY = -644.331: vZ = 245.669: vRotX = 14.663: Vstep = Vstep + 1
	Case 115 : vY = -646.302: vZ = 245.103: vRotX = 16.780: Vstep = Vstep + 1
	Case 116 : vY = -648.308: vZ = 244.445: vRotX = 18.960: Vstep = Vstep + 1: LatchSwitch  True
	Case 117 : vY = -648.357: vZ = 246.278: vRotX = 21.575: Vstep = Vstep + 1: LatchSwitch  True
	Case 118 : vY = -648.050: vZ = 248.364: vRotX = 24.232: Vstep = Vstep + 1
	Case 119 : vY = -647.368: vZ = 250.481: vRotX = 26.522: Vstep = Vstep + 1

	Case 120 : vY = -646.360: vZ = 252.449: vRotX = 28.081: Vstep = Vstep + 1
	Case 121 : vY = -645.156: vZ = 254.074: vRotX = 28.622: Vstep = Vstep + 1
	Case 122 : vY = -643.958: vZ = 255.092: vRotX = 27.956: Vstep = Vstep + 1
	Case 123 : vY = -644.203: vZ = 253.554: vRotX = 25.579: Vstep = Vstep + 1
	Case 124 : vY = -645.596: vZ = 249.414: vRotX = 21.476: Vstep = Vstep + 1
	Case 125 : vY = -646.329: vZ = 245.095: vRotX = 16.809: Vstep = Vstep + 1
	Case 126 : vY = -643.586: vZ = 245.863: vRotX = 13.868: Vstep = Vstep + 1
	Case 127 : vY = -642.323: vZ = 246.166: vRotX = 12.526: Vstep = Vstep + 1
	Case 128 : vY = -642.271: vZ = 246.178: vRotX = 12.471: Vstep = Vstep + 1: LatchSwitch  False
	Case 129 : vY = -643.123: vZ = 245.978: vRotX = 13.375: Vstep = Vstep + 1: LatchSwitch  False

	Case 130 : vY = -644.571: vZ = 245.605: vRotX = 14.919: Vstep = Vstep + 1
	Case 131 : vY = -646.339: vZ = 245.092: vRotX = 16.820: Vstep = Vstep + 1
	Case 132 : vY = -648.195: vZ = 244.484: vRotX = 18.836: Vstep = Vstep + 1: LatchSwitch  True
	Case 133 : vY = -649.963: vZ = 243.837: vRotX = 20.781: Vstep = Vstep + 1: LatchSwitch  True
	Case 134 : vY = -651.525: vZ = 243.207: vRotX = 22.520: Vstep = Vstep + 1
	Case 135 : vY = -652.815: vZ = 242.645: vRotX = 23.972: Vstep = Vstep + 1
	Case 136 : vY = -653.804: vZ = 242.187: vRotX = 25.099: Vstep = Vstep + 1 : VengPos = 1 : LatchSwitch  True
	Case 137 : vY = -654.500: vZ = 241.851: vRotX = 25.896: Vstep = Vstep + 1 : VengPos = 1 : LatchSwitch  True
	Case 138 : vY = -654.927: vZ = 241.639: vRotX = 26.388: Vstep = Vstep + 1 : VengPos = 1 : LatchSwitch  True
	Case 139 : vY = -655.124: vZ = 241.539: vRotX = 26.617: Vstep = Vstep + 1 : VengPos = 1 : LatchSwitch  True

	Case 140 : vY = -655.138: vZ = 241.532: vRotX = 26.633: Vstep = Vstep + 1
	Case 141 : vY = -655.014: vZ = 241.594: vRotX = 26.49: Vstep = Vstep + 1
	Case 142 : vY = -654.799: vZ = 241.703: vRotX = 26.241: Vstep = Vstep + 1
	Case 143 : vY = -654.532: vZ = 241.835: vRotX = 25.934: Vstep = Vstep + 1
	Case 144 : vY = -654.248: vZ = 241.974: vRotX = 25.606: Vstep = Vstep + 1
	Case 145 : vY = -653.972: vZ = 242.107: vRotX = 25.29: Vstep = Vstep + 1
	Case 146 : vY = -653.725: vZ = 242.225: vRotX = 25.007: Vstep = Vstep + 1
	Case 147 : vY = -653.517: vZ = 242.323: vRotX = 24.771: Vstep = Vstep + 1
	Case 148 : vY = -653.356: vZ = 242.398: vRotX = 24.587: Vstep = Vstep + 1
	Case 149 : vY = -653.241: vZ = 242.451: vRotX = 24.456: Vstep = Vstep + 1' : VengPos = 1 : LatchSwitch  True

	Case 150 : vY = -653.170: vZ = 242.484: vRotX = 24.375: Vstep = Vstep + 1' : VengPos = 1 : LatchSwitch  True
	Case 151 : vY = -653.136: vZ = 242.499: vRotX = 24.337: Vstep = Vstep + 1' : VengPos = 1 : LatchSwitch  True
	Case 152 : vY = -653.131: vZ = 242.501: vRotX = 24.332: Vstep = Vstep + 1
	Case 153 : vY = -653.150: vZ = 242.493: vRotX = 24.353: Vstep = Vstep + 1
	Case 154 : vY = -653.189: vZ = 242.475: vRotX = 24.397: Vstep = Vstep + 1 	:me.enabled = 0	


	'200-> 250, Return anim


	Case 201  : vY = -652.374: vZ = 243.666: vRotX = 25.013: Vstep = Vstep + 1	'301
	Case 202  : vY = -649.759: vZ = 246.830: vRotX = 26.109: Vstep = Vstep + 1
	Case 203  : vY = -646.203: vZ = 250.820: vRotX = 27.833: Vstep = Vstep + 1
	Case 204  : vY = -642.083: vZ = 254.748: vRotX = 29.767: Vstep = Vstep + 1
	Case 205  : vY = -637.682: vZ = 257.870: vRotX = 31.338: Vstep = Vstep + 1
	Case 206  : vY = -633.282: vZ = 259.542: vRotX = 31.932: Vstep = Vstep + 1
	Case 207  : vY = -628.149: vZ = 260.423: vRotX = 31.372: Vstep = Vstep + 1
	Case 208  : vY = -621.692: vZ = 261.566: vRotX = 29.935: Vstep = Vstep + 1
	Case 209  : vY = -614.547: vZ = 262.842: vRotX = 27.843: Vstep = Vstep + 1

	Case 210 : vY = -607.296: vZ = 264.153: vRotX = 25.252: Vstep = Vstep + 1
	Case 211 : vY = -600.477: vZ = 265.429: vRotX = 22.256: Vstep = Vstep + 1
	Case 212 : vY = -594.579: vZ = 266.627: vRotX = 18.89: Vstep = Vstep + 1: LatchSwitch  False
	Case 213 : vY = -590.044: vZ = 267.718: vRotX = 15.127: Vstep = Vstep + 1: LatchSwitch  False
	Case 214 : vY = -586.289: vZ = 268.615: vRotX = 11.14: Vstep = Vstep + 1: LatchSwitch  False
	Case 215 : vY = -582.616: vZ = 269.231: vRotX = 7.293: Vstep = Vstep + 1: LatchSwitch  False
	Case 216 : vY = -579.301: vZ = 269.572: vRotX = 3.853: Vstep = Vstep + 1: LatchSwitch  False
	Case 217 : vY = -576.525: vZ = 269.704: vRotX = 0.982: Vstep = Vstep + 1
	Case 218 : vY = -574.370: vZ = 269.710: vRotX = -1.243: Vstep = Vstep + 1
	Case 219 : vY = -572.847: vZ = 269.664: vRotX = -2.816: Vstep = Vstep + 1

	Case 220 : vY = -571.909: vZ = 269.615: vRotX = -3.786: Vstep = Vstep + 1
	Case 221 : vY = -571.476: vZ = 269.587: vRotX = -4.235: Vstep = Vstep + 1
	Case 222 : vY = -571.449: vZ = 269.585: vRotX = -4.263: Vstep = Vstep + 1
	Case 223 : vY = -571.724: vZ = 269.603: vRotX = -3.977: Vstep = Vstep + 1
	Case 224 : vY = -572.202: vZ = 269.632: vRotX = -3.483: Vstep = Vstep + 1
	Case 225 : vY = -572.792: vZ = 269.662: vRotX = -2.873: Vstep = Vstep + 1
	Case 226 : vY = -573.420: vZ = 269.686: vRotX = -2.225: Vstep = Vstep + 1
	Case 227 : vY = -574.025: vZ = 269.703: vRotX = -1.599: Vstep = Vstep + 1
	Case 228 : vY = -574.568: vZ = 269.713: vRotX = -1.038: Vstep = Vstep + 1
	Case 229 : vY = -575.022: vZ = 269.717: vRotX = -0.57: Vstep = Vstep + 1

	Case 230 : vY = -575.374: vZ = 269.717: vRotX = -0.207: Vstep = Vstep + 1
	Case 231 : vY = -575.623: vZ = 269.716: vRotX = 0.051: Vstep = Vstep + 1
	Case 232 : vY = -575.778: vZ = 269.715: vRotX = 0.211: Vstep = Vstep + 1
	Case 233 : vY = -575.850: vZ = 269.715: vRotX = 0.285: Vstep = Vstep + 1
	Case 234 : vY = -575.857: vZ = 269.715: vRotX = 0.293: Vstep = Vstep + 1
	Case 235 : vY = -575.812: vZ = 269.717: vRotX = 0.246: Vstep = Vstep + 1
	Case 236 : vY = -575.735: vZ = 269.716: vRotX = 0.167: Vstep = Vstep + 1
	Case 237 : vY = -575.640: vZ = 269.716: vRotX = 0.068: Vstep = Vstep + 1
	Case 238 : vY = -575.538: vZ = 269.717: vRotX = -0.037: Vstep = Vstep + 1
	Case 239 : vY = -575.439: vZ = 269.717: vRotX = -0.139: Vstep = Vstep + 1

	Case 240 : vY = -575.351: vZ = 269.717: vRotX = -0.23: Vstep = Vstep + 1 :  VengPos = 0
	Case 241 : vY = -575.277: vZ = 269.717: vRotX = -0.306: Vstep = Vstep + 1 :  VengPos = 0
	Case 242 : vY = -575.220: vZ = 269.717: vRotX = -0.366: Vstep = Vstep + 1 :  VengPos = 0
	Case 243 : vY = -575.179: vZ = 269.717: vRotX = -0.408: Vstep = Vstep + 1 :  VengPos = 0
	Case 244 : vY = -575.153: vZ = 269.717: vRotX = -0.435: Vstep = Vstep + 1
	Case 245 : vY = -575.144: vZ = 269.717: vRotX = -0.443: Vstep = Vstep + 1
	Case 246 : vY = -575.140: vZ = 269.717: vRotX = -0.448: Vstep = Vstep + 1
	Case 247 : vY = -575.147: vZ = 269.717: vRotX = -0.441: Vstep = Vstep + 1
	Case 248 : vY = -575.160: vZ = 269.717: vRotX = -0.428: Vstep = Vstep + 1
	Case 249 : vY = -575.175: vZ = 269.717: vRotX = -0.412: Vstep = Vstep + 1

	Case 250 : vY = -575.191: vZ = 269.717: vRotX = -0.395 	:me.enabled = 0	

	'300 - 350 - Nudge Left TODO

	'350 -> 400 - Nudge Right TODO

End Select


	If vRotZ < vRotZDesired then 
		vRotZ = vRotZDesired
	Elseif vRotZ > 0 then 
		vRotZ = vRotZDesired
	Else
		vRotZ = vRotZ + vRotZStep
	End If
'	vRotZ = 0

	vY = vY * -1
	vRotX = vRotX - 180
	vRotX = vRotX * -1


	'parts that need to be updated:
	'Vengeance
	'L56bulb and L57bulb
	'L56 & L57
	'F31

	'Y, Z, RotX, RotZ

	Vengeance.y = vY : Vengeance.z = vZ : Vengeance.RotX = vRotX : Vengeance.RotZ = vRotZ
	l56bulb.y = vY : l56bulb.z = vZ : l56bulb.RotX = vRotX : l56bulb.RotZ = vRotZ
	L57bulb.y = vY : L57bulb.z = vZ : L57bulb.RotX = vRotX : L57bulb.RotZ = vRotZ
	l56.y = vY : l56.z = vZ : l56.RotX = vRotX : l56.RotZ = vRotZ
	L57.y = vY : L57.z = vZ : L57.RotX = vRotX : L57.RotZ = vRotZ
	f31.y = vY : f31.height = vZ : f31.RotX = vRotX : f31.RotZ = vRotZ


End Sub

'Base cords -
' x = 479.111 / y = 575.237 / z = 269.718 / RotX = -0.347
'Framerate 100FPS (rate 10ms)

dim vRotZ : vRotZ = 0 'keep this in memory

InitVengPositions
Sub InitVengPositions
	dim vX, vY, vZ, vRotX
	vRotZDesired = 0'RndNum(0, 3)
	vRotZ = 0
'	vX = 481.8067: vY = 568.3599: vZ = 314.335		: vRotX = 1.4696
	vX = 479.111
	vY = -575.191: vZ = 269.717: vRotX = -0.395

	vY = vY * -1
	vRotX = vRotX - 180
	vRotX = vRotX * -1

	Vengeance.X = vX	'init X only. X position does not move with animation.
	l56bulb.X = vX
	L57bulb.X = vX
	l56.X = vX
	L57.X = vX
	f31.X = vX


	Vengeance.y = vY : Vengeance.z = vZ : Vengeance.RotX = vRotX : Vengeance.RotZ = vRotZ
	l56bulb.y = vY : l56bulb.z = vZ : l56bulb.RotX = vRotX : l56bulb.RotZ = vRotZ
	L57bulb.y = vY : L57bulb.z = vZ : L57bulb.RotX = vRotX : L57bulb.RotZ = vRotZ
	l56.y = vY : l56.z = vZ : l56.RotX = vRotX : l56.RotZ = vRotZ
	L57.y = vY : L57.z = vZ : L57.RotX = vRotX : L57.RotZ = vRotZ
	f31.y = vY : f31.height = vZ : f31.RotX = vRotX : f31.RotZ = vRotZ

End Sub

' Switches 
Sub SW1_Hit():Controller.Switch(1) = 1 :End Sub	' Rollovers
Sub SW1_UnHit():Controller.Switch(1) = 0 :End Sub
Sub SW2_Hit():Controller.Switch(2) = 1 :End Sub
Sub SW2_UnHit():Controller.Switch(2) = 0 :End Sub
Sub SW4_Hit():Controller.Switch(4) = 1 :End Sub
Sub SW4_UnHit():Controller.Switch(4) = 0 :End Sub

sub sw7_hit():vpmTimer.PulseSw 7:end sub	' Right bank (Power and Weapons)
sub sw8_hit():vpmTimer.PulseSw 8:end sub
sub sw9_hit():vpmTimer.PulseSw 9:end sub

Sub sw12_Spin():vpmTimer.PulseSw 12: End Sub 'SoundSpinner sw12 : End Sub	' Spinner

Sub SW13_Hit():Controller.Switch(13) = 1 :End Sub 	' Side ramp entrance
Sub SW13_UnHit():Controller.Switch(13) = 0 :End Sub
Sub SW14_Hit():Controller.Switch(14) = 1 :End Sub	' Left ramp entrance
Sub SW14_UnHit():Controller.Switch(14) = 0 :End Sub
'sw15 tournament button, sw16 start button
Sub SW23_Hit():Controller.Switch(23) = 1 :End Sub	' Shooter Lane
Sub SW23_UnHit():Controller.Switch(23) = 0 :activeball.x = 897:End Sub	'plunger hack

Sub SW24_Hit():Controller.Switch(24) = 1 :End Sub
Sub SW24_UnHit():Controller.Switch(24) = 0 : if activeball.vely < 12 then activeball.x = 52.5 end if :End Sub
Sub SW25_Hit():Controller.Switch(25) = 1 :End Sub
Sub SW25_UnHit():Controller.Switch(25) = 0 :End Sub

' Slings
Sub LeftSlingShot_SlingShot()
	vpmTimer.PulseSw 26
    RandomSoundSlingshotLeft Col_RUBBER_MISC15
	LeftSling.playanim 0, (3*CGT/500) * 2
	LeftSlingArm.playanim 0, (3*CGT/500) * 2
End Sub

Sub RightSlingShot_SlingShot()
	vpmTimer.PulseSw 27
	RandomSoundSlingshotRight Col_RUBBER_MISC16
	RightSling.playanim 0, (3*CGT/500) * 2
	RightSlingArm.playanim 0, (3*CGT/500) * 2
End Sub

Sub SW28_Hit():Controller.Switch(28) = 1 :End Sub
Sub SW28_UnHit():Controller.Switch(28) = 0 :End Sub

Sub SW29_Hit():Controller.Switch(29) = 1 :End Sub
Sub SW29_UnHit():Controller.Switch(29) = 0 :End Sub

Sub Bumper1_Hit:vpmTimer.PulseSw 30:RandomSoundBumperTop Bumper1:BumperRing1.playanim 0, (3*CGT/50) * 0.5:End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 31:RandomSoundBumperMiddle Bumper2:BumperRing2.playanim 0, (3*CGT/50) * 0.5:End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 32:RandomSoundBumperBottom Bumper3:BumperRing3.playanim 0, (3*CGT/50) * 0.5:End Sub

Sub SW33_Hit():Controller.Switch(33) = 1 : me.timerenabled = 0: End Sub	'Veng Otp 1
Sub SW33_UnHit():me.timerenabled = 1 : End Sub
Sub SW33_Timer():Controller.Switch(33) = 0  : Me.TimerEnabled = 0: End Sub

''Sub SW34_Hit():Controller.Switch(34) = 1 : tb.text = "sw34:" & Controller.Switch(34) :End Sub	'Center Lock (Top)
'Sub SW34_Hit():Controller.Switch(34) = 1 ::End Sub
'Sub SW34_UnHit():Controller.Switch(34) = 0 :End Sub

Sub SW34_Hit():Controller.Switch(34) = 1 : me.timerenabled = 0: End Sub	' Center Lock (Top)
Sub SW34_UnHit():me.timerenabled = 1 : End Sub
Sub SW34_Timer():Controller.Switch(34) = 0  : Me.TimerEnabled = 0: End Sub


Sub SW35_Hit():Controller.Switch(35) = 1 :End Sub	' Left Ramp Exit
Sub SW35_UnHit():Controller.Switch(35) = 0 :End Sub
Sub SW36_Hit():Controller.Switch(36) = 1 :End Sub	' Side Ramp Exit
Sub SW36_UnHit():Controller.Switch(36) = 0 :End Sub

Sub SW37_Hit():Controller.Switch(37) = 1 :End Sub	' Right Ramp Entrance
Sub SW37_UnHit():Controller.Switch(37) = 0 :End Sub
Sub SW38_Hit():Controller.Switch(38) = 1 :End Sub	' Right Ramp Exit
Sub SW38_UnHit():Controller.Switch(38) = 0 :End Sub

sub sw39_hit():vpmTimer.PulseSw 39:end sub	' Center 3-Bank (Lock Targets)
sub sw40_hit():vpmTimer.PulseSw 40:end sub
sub sw41_hit():vpmTimer.PulseSw 41:end sub

sub sw42_hit():vpmTimer.PulseSw 42:end sub	' Left 2-bank (Rescue Targets)
sub sw43_hit():vpmTimer.PulseSw 43:end sub

Sub SW44_Hit():Controller.Switch(44) = 1 :End Sub	' Behind Upper Flipper Rollover
Sub SW44_UnHit():Controller.Switch(44) = 0 :End Sub

sub sw45_hit():vpmTimer.PulseSw 45:end sub	' Red Targets
sub sw46_hit():vpmTimer.PulseSw 46:end sub
sub sw47_hit():vpmTimer.PulseSw 47:end sub
sub sw48_hit():vpmTimer.PulseSw 48:end sub	' Big Red Target (Black Hole)
sub sw49_hit():vpmTimer.PulseSw 49:end sub
sub sw50_hit():vpmTimer.PulseSw 50:end sub

Sub SW51_Hit():Controller.Switch(51) = 1 :End Sub	' Right Orbit
Sub SW51_UnHit():Controller.Switch(51) = 0 :End Sub
Sub SW52_Hit():Controller.Switch(52) = 1 :End Sub	' Left Orbit
Sub SW52_UnHit():Controller.Switch(52) = 0 :End Sub


'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
'***************************************************
Sub SetLamp(nr, value)
	If value = 0 AND LampState(nr) = 0 Then Exit Sub
	If value = 1 AND LampState(nr) = 1 Then Exit Sub
	If value <> LampState(nr) Then
		LampState(nr) = abs(value)
		FadingLevel(nr) = abs(value) + 4
    End If
End Sub

Sub SetModLamp(nr, value)
    If value <> SolModValue(nr) Then
		SolModValue(nr) = value
		if value > 0 then LampState(nr) = 1 else LampState(nr) = 0
        FadingLevel(nr) = LampState(nr) + 4
    End If
End Sub

Sub SetModLampM(nr, nr2, value)	'setlamp Nr x 2
    If value <> SolModValue(nr) Then
		SolModValue(nr) = value
		if value > 0 then LampState(nr) = 1 else LampState(nr) = 0
        FadingLevel(nr) = LampState(nr) + 4
    End If
    If value <> SolModValue(nr2) Then
		SolModValue(nr2) = value
		if value > 0 then LampState(nr2) = 1 else LampState(nr2) = 0
        FadingLevel(nr2) = LampState(nr2) + 4
    End If
End Sub

Sub SetModLampMM(nr, nr2, nr3, value)	'setlamp NR x3
    If value <> SolModValue(nr) Then
		SolModValue(nr) = value
		if value > 0 then LampState(nr) = 1 else LampState(nr) = 0
        FadingLevel(nr) = LampState(nr) + 4
    End If
    If value <> SolModValue(nr2) Then
		SolModValue(nr2) = value
		if value > 0 then LampState(nr2) = 1 else LampState(nr2) = 0
        FadingLevel(nr2) = LampState(nr2) + 4
    End If
    If value <> SolModValue(nr3) Then
		SolModValue(nr3) = value
		if value > 0 then LampState(nr3) = 1 else LampState(nr3) = 0
        FadingLevel(nr3) = LampState(nr3) + 4
    End If
End Sub

Dim LampState(500), FadingLevel(500)
Dim FlashSpeedUp(500), FlashSpeedDown(500), FlashMin(500), FlashMax(500), FlashLevel(500)
'turn off the lights and flashers and reset them to the default parameters
dim SolModValue(500)	'Holds Modulated Data
dim LightFallOff(500, 4)	'2d array to hold alt falloff values in different columns
dim FlashersOpacity(500)
dim FlashersOpacityV(500)
dim LampsOpacity(500)
dim FlashersIntensity(500)
dim FlashersFalloff(500)	'??? (could use multiply? or some other kind of mixing?...)
dim GIscale

dim FlashersOpacityN(500,1)
Dim Nmult(500)


sub TweakRGB(typee, input)
	dim x
	if typee = 0 then 'opacity
		for each x in allRGB
			x.opacity = input
		Next
	elseif typee = 1 then 'Modulatevsadd
		for each x in allRGB
			x.modulatevsadd = input
		Next
	elseif typee = 2 then 'multiply
		for each x in allRGB
			x.imageb = x.imagea
			x.Filter = "Multiply"
			x.Amount = input
			tb.text = x.imagea & x.imageb & x.filter & x.amount
		Next
	elseif typee = 3 then 'additive
		for each x in allRGB
			x.imageb = x.imagea
			x.Filter = "Additive"
			x.Amount = input
			tb.text = x.imagea & x.imageb & x.filter & x.amount
		Next
	elseif typee = 4 then 'screen
		for each x in allRGB
			x.imageb = x.imagea
			x.Filter = "Screen"
			x.Amount = input
			tb.text = x.imagea & x.imageb & x.filter & x.amount
		Next
	end if
End Sub



Sub Tweakref(input1, input2) 
	dim x, x2
	select case Input1
		case 1
			for each x in ReflectionsGI
				x.Intensity = Input2
			next
			x2 = "Intensity"
		case 2
			for each x in ReflectionsGI
				x.BulbModulatevsAdd = input2
			next
			x2 = "Mod"
		Case 3
			for each x in ReflectionsGI
				x.Falloff = input2
			next
			x2 = "Falloff"
		Case 4
			for each x in ReflectionsGI
				x.Falloffpower = input2
			next
			x2 = "FalloffPower"
		Case 5
			for each x in ReflectionsGI
				x.bulbhaloheight = input2
			next
			x2 = "height"
	End Select

	tb.text = "tweaked " & x2' & vbnewline & "Opacity:" & L34.Opacity & vbnewline & "Mod:" & l34.ModulatevsAdd & _
'		vbnewline & "fadeup:" & FLashSpeedUp(34) & vbnewline & "fadeDown:" & FLashSpeedDown(34)
		
End Sub
dim lastrgb(5) : lastrgb(0) = 0 : lastrgb(1) = 0 : lastrgb(2) = 0
InitLamps 0 
Sub InitLamps(input)
	dim x
	if input = 0 then 
		For x = 0 to 500
			LampState(x) = 0         ' current light state, independent of the fading level. 0 is off and 1 is on
			FadingLevel(x) = 4       ' used to track the fading state
			nMult(x) = 1
			giscale = 1.5	'todo ADJUST ME
			FlashSpeedUp(x) = 0.1    ' faster speed when turning on the flasher
			FlashSpeedDown(x) = 0.08 ' slower speed when turning off the flasher
			FlashMax(x) = 1          ' the maximum value when on, usually 1
			FlashMin(x) = 0          ' the minimum value when off, usually 0
			FlashLevel(x) = 0.0001        ' the intensity of the flashers, usually from 0 to 1
			SolModValue(x) = 0
			FlashersOpacity(x) = 0
			FlashersOpacityV(x) = 0
			FlashersIntensity(x) = 0
			FlashersOpacityN(x, 0) = 0
			FlashersOpacityN(x, 1) = 0
			LightFallOff(x, 0) = 0
			LightFallOff(x, 1) = 0
			LightFallOff(x, 2) = 0
			LightFallOff(x, 3) = 0
		Next
		for x = 401 to 480	'flashers
			FlashSpeedUp(x) = 1.1 '0.4  ' faster speed when turning on the flasher
			FlashSpeedDown(x) = 0.9	'0.2 ' slower speed when turning off the flasher
		next
		for x = 105 to 108 'GI fading speeds	'105 is PF GI string (from rom) test GI with setlamp 105, 0
			FlashSpeedUp(x) = 0.014
			FlashSpeedDown(x) = 0.014
		next
		FlashSpeedUp(499)	 = 0.014	'GiCallBack PF GI
		FlashSpeedDown(499)	 = 0.014

		FlashSpeedUp(0) = 4			'magnet
		FlashSpeedDown(0) = 1.75	'magnet

		For x = 490 to 491 'instruction card animations
			FlashSpeedUp(x) = 0.006
			FlashSpeedDown(x) = 0.006	
		next

		FlashMin(493) = 180	'drop target hit animation (rotx wiggle)
		FlashMax(493) = 190
		FlashSpeedUp(493) = 0.8
		FlashSpeedDown(493) = 0.7
		
		FlashMin(494) = -85	'drop target drop animation
		FlashMax(494) = -35
		FlashSpeedUp(494) = 1
		FlashSpeedDown(494) = 1
		
	end if

	'Keep Flasher Opacity into an array for GI brightness scaling
	for x = 0 to (allFlashers.Count - 1)
		FlashersOpacity(x) = allFlashers(x).Opacity
	Next

	'Special for flasher caps
	for x = 0 to (aFlasherCaps.Count - 1)
		FlashersOpacityV(x) = aFlasherCaps(x).Opacity
	Next

	'Same with 'regular' lamps (RGB ones can be handled just using intensityscale)
	for x = 0 to (allRegularLamps.Count - 1)
'		On Error Resume Next ' nah just no light objects please
		LampsOpacity(x) = AllRegularLamps(x).Opacity
	next

	'And Lamp Intensity
	for x = 0 to (allFlashersTransmit.Count - 1)
		FlashersIntensity(x) = allFlashersTransmit(x).Intensity
	next

	'Normalize some close-together flashers '2d array . column 0 = opacity, column 1 = lamp number
	dim nrr
	for x = 0 to (ColFlashersNormalize.Count - 1)
		FlashersOpacityN(x, 0) = ColFlashersNormalize(x).Opacity	'KEEP THESE OBJECTS OUT OF THE REGULAR ALLFLASHERS COLLECTION!
		select case cint(mid(ColFlashersNormalize(x).name, 2, 2) )	'column 1 = lamp number
			case 17 : nrr = 417
			case 18 : nrr = 418
			case 19 : nrr = 419
			case 20 : nrr = 420
'			case 19 : nrr = 469	'special nr
'			case 20 : nrr = 470	'special nr
'			case 23 : nrr = 423
'			case 26 : nrr = 426
'			case 28 : nrr = 428
			case else : tb.text = "nr error: " & mid(ColFlashersNormalize(x).name, 2, 2) 
		end Select
		FlashersOpacityN(x, 1) = nrr
	next

	updatelamps
End Sub

dim CGT	'FrameTime
dim InitFadeTime(0) : InitFadeTime(0) = 0'For finding frametime
Sub LampTimer_Timer()
	cgt = gametime - InitFadeTime(0)
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array	'this will spit out 0-127 for RGBs at weird NRs
'            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
            FadingLevel(chgLamp(ii, 0) ) = abs(cBool(chgLamp(ii, 1) ) ) + 4 'modified to keep it at 1 or 0.
        Next
    End If
	UpdateLamps
	UpdateBallShadow
	'options- nr  obj  size scl str scl offmult
	ModMagnet 0, vMag, 100, 15, 0.1, 0, 3 'nr, Object, InitSize, SizeScale, InitStr, StrScale (scaling via func ScaleLights), offscale (faster fading when off)

	FadeGI 105
	FadeGI 499
	UpdateGIobjects 105, 499, GIscale, 0.15	'NR1, NR2, Giscale var, ColFlashersNormalize percentage
	

	'Normalize close-together flashers... requires extra configuration. See sub
'	NormalizeFlashers ColFlashersNormalize, 0.15	'usage: collection, percentage	'disabled, see UpdateGIobjects

	ObjRotation 498, Scoop, -1

	FadeAnimation 490, Card_FS	'fs instruction card animation	'set up animations start and end points in FlashMin / FlashMax
	FadeAnimation 491, Card_DT	'dt

	FadeAnimationZ 492, PunchIt_Button_Clear

	FadeAnimationRotX 493, sw11p, 1
	fadeAnimationZ 494, sw11p


	InitFadeTime(0) = gametime
End Sub

function FA(input)	'debug function for testing flashers
	FA = input * giscale
End Function

dim aGI(105) : aGI(105) = Array(GI, GIP, GIL, GIR, Gia, GI_lvl2, GIt, GI_BackwallA)

Sub UpdateLamps()

	RGBSimple L1, 84, 85, 86, 0	'RGBSimple Object, R, G, B, ObjectType (latter 3 are lampstate numbers, each returning a number between 0 and 127)
	RGBSimple L2, 81, 87, 88, 0
	RGBSimple L3, 83, 82, 89, 0 '1 = light 0 = flasher
	RGBSimple L4, 92, 91, 90, 0
	RGBSimple L5, 93, 94, 95, 0
	RGBSimple L6, 96, 97, 98, 0
	RGBSimple L7, 99, 100, 101, 0
	RGBSimple L8, 102, 103, 104, 0
	RGBSimple L9, 113, 114, 115, 0

	RGBSimple L10, 116, 117, 118, 0
	RGBSimple L11, 119, 120, 121, 0
	RGBSimple L12, 122, 123, 124, 0
	RGBSimple L13, 125, 126, 127, 0
	RGBSimple L14, 128, 129, 130, 0
	RGBSimple L15, 131, 133, 132, 0
	RGBSimple L16, 134, 136, 135, 0
	RGBSimple L17, 146, 147, 148, 0
	RGBSimple L18, 149, 150, 151, 0
	RGBSimple L19, 152, 153, 154, 0

	RGBSimple L20, 155, 156, 157, 0
	RGBSimple L21, 158, 159, 160, 0
	RGBSimple L22, 161, 162, 163, 0
	RGBSimple L23, 164, 165, 166, 0
	RGBSimple L24, 167, 168, 169, 0
	RGBSimple L25, 170, 171, 172, 0
	RGBSimple L26, 179, 177, 178, 0
	RGBSimple L26r, 179, 177, 178, 0

	RGBSimple L27, 182, 180, 181, 0
	RGBSimple L27r, 182, 180, 181, 0

	RGBSimple L28, 185, 183, 184, 0
	RGBSimple L29, 186, 187, 188, 0
	RGBReflectionC l29R, 192, 193, 194, 189, 190, 191, 186, 187, 188, 0	'RGB contribution from 3 different lamps
	RGBSimple L30, 189, 190, 191, 0
	RGBSimple L31, 192, 193, 194, 0
	RGBSimple L32, 195, 197, 196, 0
	RGBSimple L33, 198, 200, 199, 0
	RGBSimple L34, 214, 215, 216, 0
	RGBSimple L35, 211, 217, 218, 0
	RGBSimple L36, 213, 212, 219, 0
	RGBSimple L37, 222, 221, 220, 0
	RGBSimple L38, 223, 224, 225, 0
	RGBSimple L39, 226, 227, 228, 0

	RGBSimple L40, 229, 230, 231, 0
	RGBSimple L41, 232, 233, 234, 0
	RGBSimple L42, 235, 236, 237, 0
	RGBSimple L43, 238, 239, 240, 0
	RGBSimple L44, 241, 242, 243, 0
	RGBSimple L45, 244, 245, 246, 0
	RGBSimple L46, 247, 248, 249, 0
	RGBSimple L47, 250, 251, 252, 0
	RGBSimple L48, 253, 254, 255, 0
	RGBSimple L49, 256, 257, 258, 0

	FlashC 276, L50 '1L	'vertical bulb
	Flashm 276, L50a


	FlashC 277, L51 'Enterprise
	FlashObjM 277, L51p, "L51_", 3

	RGBSimple L52, 278, 279, 280, 0	'vertical emblem
	RGBSimple L53, 281, 283, 282, 0
	RGBSimple L54, 284, 286, 285, 0
	RGBSimple L55, 287, 289, 288, 0

	RGBSimple L53r, 281, 283, 282, 0
	RGBSimple L54r, 284, 286, 285, 0
	RGBSimple L55r, 287, 289, 288, 0

	EmptyFade 290  'veng saucer
	FlashObjm 290, l56, "AlphaFlash", 3
	FlashObjM 290, l56bulb, "L51_", 3

	EmptyFade 291	'veng nacelles
	FlashObjm 291, l57, "AlphaFlash", 3
	FlashObjM 291, l57bulb, "L51_", 3

	RGBSimple L58, 308, 309, 310, 0
	RGBSimple L59, 311, 312, 313, 0

	RGBSimple L60, 314, 315, 316, 0
	RGBSimple L61, 317, 319, 318, 0
	RGBSimple L62, 320, 322, 321, 0
	RGBSimple L63, 300, 302, 301, 0	'Apron L
	RGBSimple L64, 303, 305, 304, 0	'Apron R
	'RGBdebug l63, 300, 302, 301	'test thing. changes GI colors to reflect the current mode.
'	nFadeL 80, L65 'Start button

	FlashC 78, L66 'fire (red)
	FlashC 77, L67 'fire (green)
	FlashC 76, L68 'fire (blue)

	'nFadeL L69, 79 'tournament button

	FlashC 295, L70_1	'Warp Chasers
	Flashm 295, L70_2
	Flashm 295, WarpAmbient1

	FlashC 292, L71_1
	Flashm 292, L71_2
	Flashm 292, WarpAmbient2
	
	FlashC 293, L72_1
	Flashm 293, L72_2
	Flashm 293, WarpAmbient3
	FlashC 294, L73_1
	Flashm 294, L73_2
	Flashm 294, WarpAmbient4

	FlashC 299, L74_1 	'Warp chaser 5
	Flashm 299, L74_2
	Flashm 299, WarpAmbient5

	FlashC 296, L75_1
	Flashm 296, L75_2
	Flashm 296, WarpAmbient6

	FlashC 297, L76_1
	Flashm 297, L76_2
	Flashm 297, WarpAmbient7
	FlashC 298, L77_1
	Flashm 298, L77_2
	Flashm 298, WarpAmbient8

	'nFadeL L79, 51 'cab side (front) (x2)
	FlashC 51, L79_1
	Flashm 51, L79_2
	Flashm 51, L79_3
	Flashm 51, L79_4
	'
	'nFadeL L80, 52
	FlashC 52, L80_1
	Flashm 52, L80_2
	Flashm 52, L80_3
	Flashm 52, L80_4
	'nFadeL L81, 53
	FlashC 53, L81_1
	Flashm 53, L81_2
	Flashm 53, L81_3
	Flashm 53, L81_4
	'nFadeL L82, 54
	FlashC 54, L82_1
	Flashm 54, L82_2
	Flashm 54, L82_3
	Flashm 54, L82_4
	'nFadeL L83, 55
	FlashC 55, L83_1
	Flashm 55, L83_2
	Flashm 55, L83_3
	Flashm 55, L83_4
	'nFadeL L84, 56
	FlashC 56, L84_1
	Flashm 56, L84_2
	Flashm 56, L84_3
	Flashm 56, L84_4
	'nFadeL L85, 57
	FlashC 57, L85_1
	Flashm 57, L85_2
	Flashm 57, L85_3
	Flashm 57, L85_4
	'nFadeL L86, 58
	FlashC 58, L86_1
	Flashm 58, L86_2
	Flashm 58, L86_3
	Flashm 58, L86_4
	'nFadeL L87, 59
	FlashC 59, L87_1
	Flashm 59, L87_2
	Flashm 59, L87_3
	Flashm 59, L87_4
	'nFadeL L88, 60
	FlashC 60, L88_1
	Flashm 60, L88_2
	Flashm 60, L88_3
	Flashm 60, L88_4
	'nFadeL L89, 61
	FlashC 61, L89_1
	Flashm 61, L89_2
	Flashm 61, L89_3
	Flashm 61, L89_4
	'
	'nFadeL L90, 62
	FlashC 62, L90_1
	Flashm 62, L90_2
	Flashm 62, L90_3
	Flashm 62, L90_4
	'nFadeL L91, 63
	FlashC 63, L91_1
	Flashm 63, L91_2
	Flashm 63, L91_3
	Flashm 63, L91_4
	'nFadeL L92, 64
	FlashC 64, L92_1
	Flashm 64, L92_2
	Flashm 64, L92_3
	Flashm 64, L92_4
	'nFadeL L93, 65
	FlashC 65, L93_1
	Flashm 65, L93_2
	Flashm 65, L93_3
	Flashm 65, L93_4
	'nFadeL L94, 66
	FlashC 66, L94_1
	Flashm 66, L94_2
	Flashm 66, L94_3
	Flashm 66, L94_4
	'nFadeL L95, 67
	FlashC 67, L95_1
	Flashm 67, L95_2
	Flashm 67, L95_3
	Flashm 67, L95_4
	'nFadeL L96, 68
	FlashC 68, L96_1
	Flashm 68, L96_2
	Flashm 68, L96_3
	Flashm 68, L96_4
	'nFadeL L97, 69
	FlashC 69, L97_1
	Flashm 69, L97_2
	Flashm 69, L97_3
	Flashm 69, L97_4
	'nFadeL L98, 70
	FlashC 70, L98_1
	Flashm 70, L98_2
	Flashm 70, L98_3
	Flashm 70, L98_4
	'nFadeL L99, 71
	FlashC 71, L99_1
	Flashm 71, L99_2
	Flashm 71, L99_3
	Flashm 71, L99_4
	'
	'nFadeL L100, 72
	FlashC 72, L100_1
	Flashm 72, L100_2
	Flashm 72, L100_3
	Flashm 72, L100_4

	'nModFlash(nr, object, offset, scaletype, offscale)

	nModFlash 417, F17, 0, 0, 1
	nModFlashM 417, f17w, 0, 0
	nModFlashM 417, f17w1, 0, 0
	nModFlashM 417, f17t, 0, 0	'Transmit light
	nModFlashM 417, f17a1, 0, 0

	nModFlash 418, F18, 0, 0, 1
	nModFlashM 418, f18w, 0, 0
	nModFlashM 418, f18w1, 0, 0
	nModFlashM 418, f18a, 0, 0
	nModFlashM 418, f18t, 0, 0	'Transmit light
	nModFlashM 418, f18a1, 0, 0

'	nModFlash 419, F19a, 0, 0, 1
	nModFlash 419, f19bw, 0, 0, 1
'	ModFlashObjm 419, F19P, "DomeRed", 13
	nModFlash 469, f19w, 0, 37, 1
	nModFlashm 469, f19a, 0, 0
	nModFlashm 469, F19LMTop, 0, 0
	nModFlashm 469, F19LM3, 0, 0
	nModFlashm 469, F19LM4, 0, 0
	nModFlashm 469, F19LM5, 0, 0
	nModFlashm 469, F19LM6, 0, 0
	nModFlashm 469, F19LM7, 0, 0
	nModFlashm 469, F19LM8, 0, 0
	nModFlashm 469, F19LM9, 0, 0
	nModFlashM 469, f19t, 0, 0	'Transmit light

'	nModFlash 420, F20a, 0, 0, 1
	nModFlash 420, f20bw, 0, 0, 1
'	ModFlashObjm 420, F20P, "DomeYellow_", 13
	nModFlash 470, f20w, 0, 37, 1
	nModFlashm 470, f20a, 0, 0
	nModFlashm 470, F20LMTop, 0, 0
	nModFlashm 470, F20LM3, 0, 0
	nModFlashm 470, F20LM4, 0, 0
	nModFlashm 470, F20LM5, 0, 0
	nModFlashm 470, F20LM6, 0, 0
	nModFlashm 470, F20LM7, 0, 0
	nModFlashm 470, F20LM8, 0, 0
	nModFlashm 470, F20LM9, 0, 0
'	nModFlashM 470, f20t, 0, 0	'Transmit light	'not used

	nModFlash 421, F21, 0, 0, 1
	nModFlashM 421, f21t, 0, 0
'
	nModFlash 423, f23, 0, 0, 1 'left Ramp
'
	nModFlash 426, f26, 0, 0, 1 'Side Ramp
	nModFlashM 426, F26w, 0, 0

	nModFlash 428, f28, 0, 0, 1 'Right Ramp
	nModFlashM 428, F28w, 0, 0

	nModFlash 425, F25, 0, 0, 1
	nModFlashM 425, F25a, 0, 0
	
	nModFlash 429, F29, 0, 0, 1
	nModFlashM 429, F29a, 0, 0

	nModFlash 427, F27, 0, 0, 1
	nModFlashM 427, F27p, 0, 0
	nModFlashM 427, F27w, 0, 0

	nModFlash 430, F30, 0, 0, 1
	nModFlashM 430, F30p, 0, 0
	nModFlashM 430, F30w, 0, 0

	nModFlash 431, F31, 0, 0, 1	'Vengeance
	nModFlashm 431, F31a, 0, 0

	nModFlash 432, F32, 0, 0, 1	'Left Sling
	nModFlashM 432, F32a, 0, 0

	nModFlash 459, F59, 0, 0, 1	'Right Sling
	nModFlashM 459, F59a, 0, 0
'	tb.text = LampState(290)
	
End Sub


Sub RGBdebug(object, aR, aG, aB)	'kinda neat, successfully pulls current mode color. unused atm
	tb1.text = LampState(AR) &  " " & lampstate(AG) & " " & lampstate(AB) & vbnewline & lastrgb(0) & " " & lastrgb(1) & " " & lastrgb(2)
'	tb1.text = lastrgb(0) & lastrgb(1) & lastrgb(2)

	if LampState(AR) = 0 and LampState(AG) = 0 and LampState(AB) = 0 then tb.text = "all0" : exit sub
'	if (LampState(AR) + LampState(AG) + LampState(AB))/3 < 79 then tb.text = "avg low" & cInt((LampState(AR) + LampState(AG) + LampState(AB))/3): exit sub
	if LampState(AR) < 129 and LampState(AG) <129 and LampState(AB) < 129 then tb.text = "all low" : exit sub
	if LampState(AR) = lastrgb(0) and LampState(AG) = lastrgb(1) and LampState(AB) = lastrgb(2) then
		 tb.text = "lastrgb"
		Lastrgb(0) = LampState(AR) : LastRGB(1) = LampState(AG) : lastrgb(2) = LampState(AB)
		exit sub
	Else
		tb.text = "gogo"
		Lastrgb(0) = LampState(AR) : LastRGB(1) = LampState(AG) : lastrgb(2) = LampState(AB)
	end if
	dim r, g, b
	R = LampState(aR)*2
	G = LampState(aG)*2
	B = LampState(aB)*2
	Desatvv R,G,B, 0.9
	dim x
	for each x in aGI(105)
		On Error Resume Next
		x.colorfull = RGB(r,g,b)
		x.color = RGB(r,g,b)
	Next
'	tb.text = lampstate(r) & vbnewline & _ 
'				lampstate(g) & vbnewline & _ 
'				lampstate(b) & vbnewline & _ 
'				" "
End Sub


Sub FadeAnimation(Nr, Object) 'primitive animation - Set up start and endpoints in FlashMin and FlashMax (here it's used just between frame 0 and 1)
	Select Case FadingLevel(nr)
		Case 4
            FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * CGT)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
               FadingLevel(nr) = 0 'completely off
            End if
            Object.ShowFrame InterpolateV(FlashLevel(nr)	)
		Case 5
            FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr) * CGT)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.ShowFrame InterpolateV(FlashLevel(nr)	)
	End Select
End Sub

Sub FadeAnimationZ(Nr, Object) 'primitive animate Z - Set up start and endpoints in FlashMin and FlashMax
	Select Case FadingLevel(nr)
		Case 4
            FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * CGT)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
               FadingLevel(nr) = 0 'completely off
            End if
            Object.z = FlashLevel(nr)	
		Case 5
            FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr) * CGT)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.z = FlashLevel(nr)	
	End Select
End Sub

Sub FadeAnimationRotX(Nr, Object, input) 'primitive animate RotX - Set up start and endpoints in FlashMin and FlashMax
	Select Case FadingLevel(nr)
		Case 4
            FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * CGT)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
               FadingLevel(nr) = 0 'completely off
            End if
            Object.RotX = FlashLevel(nr)	
		Case 5
            FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr) * CGT)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
				if Input = 1 then 
					setlamp nr, 0
				Else
					FadingLevel(nr) = 1 'completely on
				End If
            End if
            Object.RotX = FlashLevel(nr)	
	End Select
End Sub

'Lamp Fading Subs based on JPSalas's routine

Sub EmptyFade(nr)	'lazy sub handles fading, updates nothing
    Select Case FadingLevel(nr)
		case 3
			FadingLevel(nr) = 0
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * CGT)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
               FadingLevel(nr) = 3 'completely off
            End if
'            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr) * CGT)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 6 'completely on
            End if
'            Object.IntensityScale = FlashLevel(nr)
		Case 6
			FadingLevel(nr) = 1
    End Select
End Sub

'Sub EmptyFadeDebug(nr)	'lazy sub handles fading, updates nothing	'debug
'    Select Case FadingLevel(nr)
'		Case 3
'			FadingLevel(nr) = 0
'        Case 4 'off
'            FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * CGT)
'            If FlashLevel(nr) < FlashMin(nr) Then
'                FlashLevel(nr) = FlashMin(nr)
'               FadingLevel(nr) = 3 'completely off
'            End if
'           FixLamps.IntensityScale = FlashLevel(nr)
'        Case 5 ' on
'            FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr) * CGT)
'            If FlashLevel(nr) > FlashMax(nr) Then
'                FlashLevel(nr) = FlashMax(nr)
'                FadingLevel(nr) = 6 'completely on
'            End if
'           FixLamps.IntensityScale = FlashLevel(nr)
'		Case 6
'			FadingLevel(nr) = 1
'    End Select
''	tbf.text = "Lampstate: " & Lampstate(nr) & vbnewline & _ 	'debug
''				"Fading Step" & FadingLevel(nr) & vbnewline & _ 
''				"Flashlevel" & FlashLevel(nr) & vbnewline & _ 
''				" ... "
'End Sub


Sub ModFlashObjm(nr, object, imgseq, steps)	'Primitive texture image sequence	'Modified for SolModCallbacks
	dim x, x2, fadex
	Select Case FadingLevel(nr)
		Case 3, 4, 5, 6 'off
			FadeX = ScaleLights(FlashLevel(nr),0 )	'mod for solmodcallbacks
			for x = 0 to steps-1
				if FadeX <= ((x/steps) + ((1/steps)/2 )) then
					x2 = x
'					tb3.text = "on " & x & vbnewline & ((x/steps) + ((1/steps)/2 )) & " =? " & x2
					exit for
				end if
			next
			if	FadeX >= 1-(1/steps) then x2 = steps ': tb3.text = "fullon"
		'	if	FlashLevel(nr) <= (1/steps) then x2 = 0 : tb3.text = "fulloff"


'			tb.text = "flashlevel: " & FlashLevel(nr) & vbnewline & "stepper: " & x2 & vbnewline & "flasherimg: " & x2
'			tb.text = FlashLevel(nr) & vbnewline & imgseq & x2
			object.image = imgseq & x2
	End Select
End Sub

Sub FlashObjm(nr, object, imgseq, steps)	'Primitive texture image sequence
	dim x, x2
	Select Case FadingLevel(nr)
		Case 3, 4, 5, 6, 10, 11
			for x = 0 to steps-1
				if FlashLevel(nr) <= ((x/steps) + ((1/steps)/2 )) then
					x2 = x
	'				tb3.text = "on " & x & vbnewline & ((x/steps) + ((1/steps)/2 )) & " =? " & x2
					exit for
				end if
			next
			if	FlashLevel(nr) >= 1-(1/steps) then x2 = steps ': tb3.text = "fullon"
		'	if	FlashLevel(nr) <= (1/steps) then x2 = 0 : tb3.text = "fulloff"


'			tb.text = "flashlevel: " & FlashLevel(nr) & vbnewline & "stepper: " & x2 & vbnewline & "flasherimg: " & x2
'			tb.text = FlashLevel(nr) & vbnewline & imgseq & x2
			object.image = imgseq & x2
	End Select
End Sub


Sub RGBSimple(object, AR, AG, AB, ObjectType)	'variation with no added values
	FadingLevel(aR) = abs(cBool(LampState(aR) + LampState(aG) + LampState(aB)) ) + 4
	dim R, G, B
	Select Case FadingLevel(aR)
		Case 3
			FadingLevel(aR) = 0
			R = LampState(aR)*2
			G = LampState(aG)*2
			B = LampState(aB)*2
			Desat R,G,B
			if ObjectType = 1 then
				Object.ColorFull = RGB(R, G, B)
			Else
				Object.Color = RGB(R, G, B)
			End If
		Case 4
			FadingLevel(aR) = 3
			R = LampState(aR)*2
			G = LampState(aG)*2
			B = LampState(aB)*2
			Desat R,G,B
			if ObjectType = 1 then
				Object.ColorFull = RGB(R, G, B)
			Else
				Object.Color = RGB(R, G, B)
			End If
		Case 5
			R = LampState(aR)*2
			G = LampState(aG)*2
			B = LampState(aB)*2
			Desat R,G,B
			if ObjectType = 1 then
				Object.ColorFull = RGB(R, G, B)
			Else
				Object.Color = RGB(R, G, B)
			End If
	End Select
End Sub

function DeSat(r,g,b)	'simple desaturation function
	dim f, L, new_r, new_g, new_b
	f = RGBSaturation
	L = 0.3*r + 0.6*g + 0.1*b
	new_r = r + f * (L - r)
	new_g = g + f * (L - g)
	new_b = b + f * (L - b)
	r = new_r : g = new_g : b = new_b
End Function

function DeSatV(r,g,b)	'simple desaturation function (/15)
	dim f, L, new_r, new_g, new_b
	f = RGBSaturation/25
	L = 0.3*r + 0.6*g + 0.1*b
	new_r = r + f * (L - r)
	new_g = g + f * (L - g)
	new_b = b + f * (L - b)
	r = new_r : g = new_g : b = new_b
End Function

function DeSatVV(r,g,b, amount)	'Variable desat
	dim f, L, new_r, new_g, new_b
	f = amount
	L = 0.3*r + 0.6*g + 0.1*b
	new_r = r + f * (L - r)
	new_g = g + f * (L - g)
	new_b = b + f * (L - b)
	r = new_r : g = new_g : b = new_b
End Function


'nFadeLmRGB L4, 92, 91, 90

Sub	RGBReflectionC(Object, R1, G1, B1, R2, G2, B2, R3, G3, B3, FalloffType)	'3 RGB lamps contributing to one object for reflection. For the Asteroids.
	If FadingLevel(R1) + FadingLevel(R2) + FadingLevel(R3) = 0 Then Exit Sub 'probably not going to help much but w/e
	dim R, G, B
	if FalloffType = 0 then 'equal contribution from all 3 lights
		R = cInt((LampState(R1) + LampState(R2) + LampState(R3))*2)/3
		G = cInt((LampState(G1) + LampState(G2) + LampState(G3))*2)/3
		B = cInt((LampState(B1) + LampState(B2) + LampState(B3))*2)/3
		Desat R,G,B
	Elseif FalloffType = 1 then '1-0.5 linear falloff
		R = cInt((LampState(R1)*0.5714 + LampState(R2)*0.2857 + LampState(R3)*0.14286)*2)
		G = cInt((LampState(G1)*0.5714 + LampState(G2)*0.2857 + LampState(G3)*0.14286)*2)
		B = cInt((LampState(B1)*0.5714 + LampState(B2)*0.2857 + LampState(B3)*0.14286)*2)
		Desat R,G,B
	End If
	Object.Color = RGB(R, G, B) 'if something errors or crashes it's probably this
End Sub


dim LSstate : LSstate = False	'fading sub handles SFX
Sub FadeGI(nr) 'in On/off		'Updates nothing but flashlevel
    Select Case FadingLevel(nr)
		Case 3
			FadingLevel(nr) = 0
        Case 4 'off
'			If Not LSstate then Playsound "FX_Relay_Off",0,LVL(0.1) : LSstate = True	'handle SFX
            FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * CGT)
            If FlashLevel(nr) < FlashMin(nr) Then
				FlashLevel(nr) = FlashMin(nr)
				FadingLevel(nr) = 3 'completely off
'				LSstate = False
            End if
        Case 5 ' on
'			If Not LSstate then Playsound "FX_Relay_On",0,LVL(0.1) : LSstate = True	'handle SFX
            FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr) * CGT)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 6 'completely on
'				LSstate = False
            End if
		Case 6
			FadingLevel(nr) = 1
    End Select
End Sub


Sub UpdateGIobjects(nr, nr2, GIscaleOff, input)
'	tbgi.text = "GI: " & SolModValue(nr) & " " & FlashLevel(nr) & " " & FadingLevel(nr) & vbnewline & _
'				"ModGI: " & SolModValue(nr2) & " " & FlashLevel(nr2) & " " & FadingLevel(nr2) & vbnewline & _
'				"Solmodvalue, Flashlevel, Fading step"
	dim Output,x : Output = interpolate(FlashLevel(nr2) * FlashLevel(nr)	)
	dim Giscaler : Giscaler = ((Giscaleoff-1) * (ABS(Output-1) )  ) + 1	'fade GIscale the opposite direction
	dim updateovrride : updateovrride = False	'Normalize sub now handles scaling when GI is fading
	if GIscaler = 0 then Giscaler = 1
	If FadingLevel(nr) > 1 or FadingLevel(nr2) > 1 Then
		updateovrride = True
		'Update GI
		for each x in aGI(105)
			x.IntensityScale = Output
		next
		for x = 0 to (BallReflections.Count - 1) : BallReflections(x).IntensityScale = Output : Next 'Ball Reflections
		'Handle Compensate Flashers

'		tb1.text = giscaler & "" & vbnewline & "flash: " & FlashLevel(nr) & vbnewline & "gifading!"	'debug

		for x = 0 to (allFlashers.Count - 1)
			allFlashers(x).Opacity = FlashersOpacity(x) * Giscaler
		next
		for x = 0 to (allFlashersTransmit.Count - 1)
			allFlashersTransmit(x).Intensity = FlashersIntensity(x) * Giscaler
		next
		for x = 0 to (allRegularLamps.Count - 1)
			allRegularLamps(x).Opacity = LampsOpacity(x) * Giscaler
		next
		For Each x in allRGB
			x.Intensityscale = GIscaler
		Next
		Lasern.Intensityscale = GIscaler*3

		for x = 0 to (aFlasherCaps.Count - 1)
			aFlasherCaps(x).Opacity = FlashersOpacityV(x) * Giscaler*5
		Next

'		for x = 0 to (ColFlashersNormalize.Count - 1)
'			ColFlashersNormalize(x).Opacity = FlashersOpacityN(x, 0) * Giscaler * Nmult(417)
'		next
'

'		TBn1.text = F19a.opacity & " (" & deleteme & ") " & GIscaler & " " & nmult(0) & vbnewline & _
'					"opac, giscaler, norm"
'
'		tbbb.text = giscaler & " on:" & FadingLevel(nr) & vbnewline & "flash: " & output & " onmod:" & FadingLevel(nr2)' & vbnewline & f17.opacity
'		tbbb1.text = FadingLevel(nr) & vbnewline & FadingLevel(nr2)
'	tbgi1.text = Output & " giscale:" & giscaler	'debug
	End If
	NormalizeFlashers ColFlashersNormalize, input, giscaler, updateovrride
'		tbbb1.text = FLashLevel(nr) & vbnewline & FlashLevel(nr2)
end Sub
dim deleteme : deleteme = f19a.opacity

Sub NormalizeFlashers(col, input, giscaler, updateovrride)	'will update flashers
	dim flashlevels, x, a
	dim nrCount : nrCount = 4 	'2 lampnumbers for 4 objects 
	'lamp numbers
	'417, 418, 469, 470, 423, 426, 428
	a = Array(417, 418, 419, 420)

	flashlevels = ScaleLights(FlashLevel(417),0) + ScaleLights(FlashLevel(418),0) + ScaleLights(FlashLevel(419),0) + ScaleLights(FlashLevel(420),0)
'	if FlashLevels = 0 then FlashLevels = 1
	if flashlevels/nrCount > input then 
		for x = 0 to uBound(a)
			Nmult(a(x)) = input / (flashlevels/nrCount)
		Next
		for x = 0 to (col.Count - 1)
			col(x).Opacity = FlashersOpacityN(x, 0) * Giscaler * Nmult(FlashersOpacityN(x, 1) )
'			col(x).Opacity = FlashersOpacityN(x, 0) * 1 * 1
		next	

'		x = "~~~~~steaming mad~~~~~"
'		tbn.fontcolor = RGB(255,210,210)
	Elseif updateovrride then
		for x = 0 to uBound(a)
			Nmult(a(x)) = 1
		Next
		for x = 0 to (col.Count - 1)
			col(x).Opacity = FlashersOpacityN(x, 0) * Giscaler * Nmult(FlashersOpacityN(x, 1) )
'			col(x).Opacity = FlashersOpacityN(x, 0) * 1 * 1
		next
'		x = "good"
'		tbn.fontcolor = RGB(255,255,255)
	end if
'	tbn.text = "No.objs: " & nrCount & vbnewline & _
'			"FlashLevels: " & FlashLevels & vbnewline & _ 
'			"avg flashlevel: " & flashlevels / 7 & vbnewline & _ 
'			"nmult: " & Nmult(417) & " " & nMult(418) & vbnewline & _ 
'			"f17w opacity: " & f17w.opacity & " " & f17w.intensityscale & vbnewline & _ 
'			"f18w.opacity: " & f18w.opacity & " " & f18w.intensityscale  & vbnewline & _ 
'			"giscaler" & giscaler & vbnewline & _ 
'			x

End Sub


Function ScaleRGBbrightness(value)	'in combined RGB data (0 -> 508 max) 'out: intensityscale value 0->1.0
	ScaleRGBbrightness = ((value / 508) +1) / 2	'0.5->1
End Function


'Lamp Functions

Function ScaleLights(value, scaletype)	'returns an intensityscale-friendly 0->100% value out of 255
	dim i
	Select Case scaletype	'select case because bad at maths 	'TODO: Simplify these functions. B/c this is absurdly bad.
		case 0
			i = value * (1 / 255)	'0 to 1
		case 6	'0.0625 to 1
			i = (value + 17)/272
		case 9	'0.089 to 1
			i = (value + 25)/280
		case 15
			i = (value / 300) + 0.15
		case 20
			i = (4 * value)/1275 + (1/5)
		case 25
			i = (value + 85) / 340
		case 37	'0.375 to 1
			i = (value+153) / 408
		case 40
			i = (value + 170) / 425
		case 50
			i = (value + 255) / 510	'0.5 to 1
		case 75
			i = (value + 765) / 1020	'0.75 to 1
		case Else
			i = 10
	End Select
	if value = 0 then ScaleLights = 0 Else ScaleLights = i end if	'might break things
End Function

Function ScaleByte(value, scaletype)	'returns a number between 1 and 255
	dim i
	Select Case scaletype
		case 0
			i = value * 1	'0 to 1
		case 9	'ugh
			i = (5*(200*value + 1887))/1037
		case 15
			i = (16*value)/17 + 15
		case else
			i = (3*(value + 85))/4	'63.75 to 255
	End Select
	ScaleByte = i
End Function

'
Function ScaleFalloff(value, nr)	'TODO make more options here
'	if nr > 128 then 'do not scale special bulb NRs
'		ScaleFalloff = 1
'	Else
'		ScaleFalloff = (value + 255) / 510	'0.5 to 1
		ScaleFalloff = (value + 765) / 1020	'0.75 to 1
'	end if
End Function


Sub ModMagnet(nr, Object, InitSize, SizeScale, InitStr, StrScale, offscale)
	dim DesiredFading, x
	Select Case FadingLevel(nr)
		case 3	'workaround - wait a frame to let M sub finish fading 
			FadingLevel(nr) = 0
'			testlamp.state = 0
			vMag.MagnetOn = False
		Case 4
			FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * cgt	) * offscale
			If FlashLevel(nr) < 0 then FlashLevel(nr) = 0 : FadingLevel(nr) = 3
			Object.Strength = InitStr * ScaleLights(FlashLevel(nr),StrScale)
			Object.Size = InitSize * ScaleLights(FlashLevel(nr),SizeScale)
'			testlamp.falloff = InitSize * ScaleLights(FlashLevel(nr),SizeScale)	'debug lamp
		Case 5 ' Fade (Dynamic)
			DesiredFading = ScaleByte(SolModValue(nr), 0)
			if FlashLevel(nr) < DesiredFading Then 
				FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr)	* cgt	)
				If FlashLevel(nr) >= DesiredFading Then FlashLevel(nr) = DesiredFading : FadingLevel(nr) = 1
			elseif FlashLevel(nr) > DesiredFading Then 
				FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * cgt	)
				If FlashLevel(nr) <= DesiredFading Then FlashLevel(nr) = DesiredFading : FadingLevel(nr) = 1
			End If
			Object.Strength = InitStr * ScaleLights(FlashLevel(nr),StrScale)
			Object.Size = InitSize * ScaleLights(FlashLevel(nr),SizeScale)
'			testlamp.state = 1
'			testlamp.falloff = InitSize * ScaleLights(FlashLevel(nr),SizeScale)	'debug lamp
			vMag.MagnetOn = True
	End Select
'	'debug stuff
'	x = (cint(vMag.strength * 100)) / 100
'	tbm.text = SolModValue(0) * (1/2.55) & "%" & vbnewline & x & vbnewline & "onoff: " & vmag.MagnetOn
End Sub


Sub nModFlash(nr, object, offset, scaletype, offscale)	'Fading using intensityscale with modulated callbacks	'gametime compensated
	dim DesiredFading
	Select Case FadingLevel(nr)
		case 3	'workaround - wait a frame to let M sub finish fading 
			FadingLevel(nr) = 0
		Case 4	'off
			FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * cgt	) * offscale
			If FlashLevel(nr) < 0 then FlashLevel(nr) = 0 : FadingLevel(nr) = 3
			Object.IntensityScale = Interpolate(ScaleLights(FlashLevel(nr),0 )	)
		Case 5 ' Fade (Dynamic)
			DesiredFading = ScaleByte(SolModValue(nr), scaletype)
			if FlashLevel(nr) < DesiredFading Then '+
				FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr)	* cgt	)
				If FlashLevel(nr) >= DesiredFading Then FlashLevel(nr) = DesiredFading : FadingLevel(nr) = 1
			elseif FlashLevel(nr) > DesiredFading Then '-
				FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * cgt	)
				If FlashLevel(nr) <= DesiredFading Then FlashLevel(nr) = DesiredFading : FadingLevel(nr) = 6
			End If
			Object.Intensityscale = Interpolate(ScaleLights(FlashLevel(nr),0 )	)' * GIscale * Nmult(nr)
		Case 6
			FadingLevel(nr) = 1
	End Select
End Sub


Function Interpolate(x)	'smooth, subtle 0-1 interpolation	'used in FlashC and FlashM
'	Interpolate = -x^3/3 + x^2/2 + (5*x)/6  '0-0-1-1
	Interpolate = -x^3/3 + x^2/2 + (5*x)/6  '0-0-1-1 'spline
End Function

Function InterpolateV(x)	'The V stands for 'very much' interpolation
'	InterpolateV = -0.217469*x^3 + 1.10481*x^2 + 0.112656*x - 2.22045*10^-16	'Very low-end heavy
	InterpolateV = -1.94137*x^3 + 2.91206*x^2 + 0.0293147*x + 0	'Balanced but heavy
	if InterpolateV < 0 then InterpolateV = 0
End Function

Sub nModLightM(nr, Object, offset, scaletype)	'uses offset to store different falloff values in a unused lamp number. default 0
	Select Case FadingLevel(nr)
		Case 3, 4, 5, 6
			Object.Intensityscale = ScaleLights(FlashLevel(nr),0 )' * GIscale * Nmult(nr)
'			Object.IntensityScale = ScaleLights(FlashLevel(nr),scaletype ) * GIscale
			Object.Falloff = LightFallOff(nr, offset) * ScaleFalloff(FlashLevel(nr), nr)
	End Select
End Sub

Sub nModFlashM(nr, Object, offset, scaletype)
	Select Case FadingLevel(nr)
		Case 3, 4, 5, 6
			Object.Intensityscale = Interpolate(ScaleLights(FlashLevel(nr),scaletype )	)
	End Select
End Sub

Sub Flashc(nr, object)
    Select Case FadingLevel(nr)
		Case 3
			FadingLevel(nr) = 0
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * CGT)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
               FadingLevel(nr) = 3 'completely off
            End if
            Object.IntensityScale = Interpolate(FlashLevel(nr)	)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr) * CGT)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 6 'completely on
            End if
            Object.IntensityScale = Interpolate(FlashLevel(nr)	)
		Case 6
			FadingLevel(nr) = 1
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
	Select Case FadingLevel(nr)
		case 3, 4, 5, 6
			Object.IntensityScale = Interpolate(FlashLevel(nr)	)
	End Select
End Sub

Sub FlashmDual(nr1,nr2,Object)	'two lights contributing to one object
	if FadingLevel(nr1) <3 and FadingLevel(nr2) <3 then Exit Sub
	Object.IntensityScale = Interpolate((FlashLevel(nr1)+FlashLevel(nr2))/2)
End Sub


Sub NFadeL(nr, object)
    Select Case FadingLevel(nr)
        Case 4:object.state = 0:FadingLevel(nr) = 0
        Case 5:object.state = 1:FadingLevel(nr) = 1
    End Select
End Sub

Sub NFadeLm(nr, object) ' used for multiple lights
    Select Case FadingLevel(nr)
        Case 4:object.state = 0
        Case 5:object.state = 1
    End Select
End Sub

InitFlashers

Sub InitFlashers 'flasher locations (flasher X and Y positions are buggy in the VP10 editor)
	dim x
	if not Table1.ShowDt then Gi_BackwallA.opacity = 1500
	Card_Dt.Visible = Table1.ShowDT
	Card_FS.Visible = not Table1.ShowDT
	'CabinetRails.visible = Table1.ShowDT  ' CHECK LATER!

	'Asteroid flashers backwalls
	f18w1.x = 498
	f18w1.y = 165.35
	f18w1.height = 245
	f18w.x = 482.3715128
	f18w.y = 106

	f17w1.x = 498
	f17w1.y = 165.35
	f17w1.height = 245
	f17w.x = 482.3715128
	f17w.y = 106

	'Leftover Inserts
	l7.x = 569
	l7.y = 781

	l21.x = 189
	l21.y = 1222

	l23.x = 127
	l23.y = 1351

	l37.x = 436
	l37.y = 1733

	l39.x = 491
	l39.y = 1640

	l41.x = 381
	l41.y = 1640

	'depth bias
	Plastics_Clear_Asteroids1.depthbias = -56
	Plastics_Clear_Asteroids2.depthbias = -56
	Plastics_Clear_SideRampTop.depthbias = -240
	Plastics_Clear_VUK.depthbias = -56
	ramps.depthbias = -56
	f25a.depthbias = 100
	f25.depthbias = 100
	f26.depthbias = 100
	f28.depthbias = 0
	plastics_clear.depthbias = -80

	'Animations



	'Button Anim
	FlashMin(492) = -5
	FlashMax(492) = 0
	FlashSpeedUp(492) = 0.15
	FlashSpeedDown(492) = 1	


	InitScoop	'scoop

	dim xx
	dim yy
	dim zz
	if MoveButton = 1 then 
		xx = -49.48
		yy = -192
		zz = -25
		PunchIt_Button_Base.x = xx
		PunchIt_Button_Base.y = yy
		PunchIt_Button_Base.z = zz
		PunchIt_Button_Clear.x = xx
		PunchIt_Button_Clear.y = yy
		PunchIt_Button_Clear.z = zz
		FlashMin(492) = -25
		FlashMax(492) = -20
		L66.x = L66.x + xx
		L66.y = L66.y + yy
		l66.height = l66.height + zz
		L67.x = L67.x + xx
		L67.y = L67.y + yy
		L67.height = L67.height + zz
		L68.x = L68.x + xx
		L68.y = L68.y + yy
		L68.height = L68.height + zz
	End If
	setlamp 492, 1

	LeftSling.Showframe 100
	LeftSlingArm.Showframe 100
	RightSling.Showframe 100
	RightSlingArm.Showframe 100
	BumperRing1.Showframe 100
	BumperRing2.Showframe 100
	BumperRing3.Showframe 100


	'F19 and F20
	F19LMtop.x = 0
	F19LMtop.y = 0

	F19LM3.x = 29.79881
	F19LM3.y = 0

	F19LM4.x = 25.80653
	F19LM4.y = 14.89941

	F19LM5.x = 14.89941
	F19LM5.y = 25.80653

	F19LM6.x = 0
	F19LM6.y = 29.79881

	F19LM7.x = -14.89941
	F19LM7.y = 25.80653

	F19LM8.x = -25.80653
	F19LM8.y = 14.89941

	F19LM9.x = -29.79881
	F19LM9.y = 0

	dim LMa : Lma = Array(F19LMtop, F19LM3, F19LM4, F19LM5, F19LM6, F19LM7, F19LM8, F19LM9, F19P1)
	for each x in Lma
		On Error Resume Next
		x.x = x.x + 47.6688957
		x.y = x.y + 248.8867188
		x.height = x.height + 155.8977509
		x.z = x.z + 155.8977509
	Next

	'f20 flasher cap lightmap test
	F20LMtop.x = 0
	F20LMtop.y = 0

	F20LM3.x = 29.79881
	F20LM3.y = 0

	F20LM4.x = 25.80653
	F20LM4.y = 14.89941

	F20LM5.x = 14.89941
	F20LM5.y = 25.80653

	F20LM6.x = 0
	F20LM6.y = 29.79881

	F20LM7.x = -14.89941
	F20LM7.y = 25.80653

	F20LM8.x = -25.80653
	F20LM8.y = 14.89941

	F20LM9.x = -29.79881
	F20LM9.y = 0

	dim LMb : Lmb = Array(F20LMtop, F20LM3, F20LM4, F20LM5, F20LM6, F20LM7, F20LM8, F20LM9, F20LMp)
	for each x in Lmb
		On Error Resume Next
		x.x = x.x + 713.588913
		x.y = x.y + 158.141861
		x.height = x.height + 209.1091156
		x.z = x.z + 209.1091156
	Next

	'Kickback reflections
	
	l26r.x = 23.7920184
	l26r.y = 1614.7636073

	l27r.x = 24.1032452
	l27r.y = 1527.4105706

	l26r.roty = l26r.roty*-1
	l27r.roty = l27r.roty*-1

	'Rollover Reflections

	f18a.y = 620
	f18a.rotx = -20


	l53r.x = 567
	l53r.y = 101

	l54r.x = 656.973
	l54r.y = 105.55381

	l55r.x = 747.997
	l55r.y = 124.77505

	l53r.roty = l53r.roty*-1
	l54r.roty = l54r.roty*-1
	l55r.roty = l55r.roty*-1

'	l54r.height = 23
'	l54r.x = 662'656.973




	f17.rotx = -25

	f17.roty = -25

	f17.rotz = 45
	f17.height = 520

	f17.x = 610
	f17.y = 460

	f18.rotx = -5
	f18.roty = 0

	f18.rotz = -60
	f18.height = 282

	f18.x = 704.428
	f18.y = 537.715


'''
	f19a.x = 47.6688957
	f19a.y = 248.8867188
'	f19a.z = 156

	f20a.x = 713.588913
	f20a.y = 158.141861

	f19bw.x = 482.3715128
	f19bw.y = 106
	F19w.x = -5.8
	F19w.y = 1057.0345


	f20bw.x = 482.3715128
	f20bw.y = 100
	f20w.x = 958.8
	f20w.y = 1057.0345


	F21.x = 526
	f21.y = 960

'	L50.x = 195
'	L50.y = 371
'	L50.Height = 282.5
'	L50.RotX = -25

	L50.x = 195
	L50.y = 365'371
	L50.Height = 284.6
	L50.RotX = -25
	
	L50a.x = 183.5
	L50a.Y = 343.625
	L50a.Height = 319.3081043

	L52.x = 183.5
	L52.Y = 343.625
	
	F27.X = 440
	F27.Y = 1075

	F30.X = 515
	F30.Y = 1100
	
	F26w.x = -5.8
	F26w.y = 1057.0345
	F27w.x = -5.8
	F27w.y = 1057.0345

	F27P.x = 227.000
	F27P.y = 928.000

	F30p.x = 834.5968309
	F30p.y = 930.327255

	F28W.x = 958.8
	F28W.y = 1057.0345

	F30W.x = 958.8
	F30W.y = 1057.0345

	GIL.x = -5.8
	GIL.y = 1057.0345
	GIR.x = 958.8
	GIR.y = 1057.0345


	F25.x = 679
	F25.y = 386

	F29.x = 215
	F29.y = 1090

'	l52.x = 182.4121
'	l52.y = 342.7238
'	l52.height = 316.2703
'
'	l50.x = 197
'	l50.y = 369.571
'	l50.height = 280.75267
'
'
'
'


	l63.x = 140'148
	l63.y = 1935'1925.1
	L64.x = 725'713.65


	Gi_Lvl2.x = 52
	Gi_Lvl2.y = 970
	Gi_Lvl2.height = 103.7
End Sub

InitPhasers
Sub InitPhasers
	Dim x
	Dim aL, aR

	aL = Array(L79_1, L80_1, L81_1, L82_1, L83_1, L84_1, L85_1,_
		L86_1, L87_1, L88_1, L89_1, L90_1, L91_1, L92_1, L93_1, L94_1,L95_1,L96_1,L97_1,L98_1,L99_1,L100_1)
	aR = Array(L79_2, L80_2, L81_2, L82_2, L83_2, L84_2, L85_2,_
		L86_2, L87_2, L88_2, L89_2, L90_2, L91_2, L92_2, L93_2, L94_2,L95_2,L96_2,L97_2,L98_2,L99_2,L100_2)

	For each x in aL
		x.x = 0
	Next
	For each x in aR
		x.x = 955
	Next


	L79_1.y = 1385.62
	l79_1.Height = 199.917
	L79_2.y = 1385.62
	l79_2.Height = 199.917

	l80_1.y = 1343.248
	l80_2.y = 1343.248
	l80_1.Height = 207.236
	l80_2.Height = 207.236

	l81_1.y = 1300.8753692
	l81_2.y = 1300.8753692
	l81_1.Height = 214.5545642
	l81_2.Height = 214.5545642

	L82_1.y = 1258.5028296
	L82_2.y = 1258.5028296
	L82_1.Height = 221.8735726
	L82_2.Height = 221.8735726

	L83_1.y = 1216.13029
	L83_2.y = 1216.13029
	L83_1.Height = 229.1925811
	L83_2.Height = 229.1925811

	L84_1.y = 1173.7577503
	L84_2.y = 1173.7577503
	L84_1.Height = 236.5115896
	L84_2.Height = 236.5115896

	L85_1.y = 1131.385
	L85_2.y = 1131.385
	L85_1.Height = 243.831
	L85_2.Height = 243.831'''

	L86_1.y = 1089.013
	L86_2.y = 1089.013
	L86_1.Height = 251.15
	L86_2.Height = 251.15

	L87_1.y = 1046.6401314
	L87_2.y = 1046.6401314
	L87_1.Height = 258.469
	L87_2.Height = 258.469

	L88_1.y = 1004.2675918
	L88_2.y = 1004.2675918
	L88_1.Height = 265.7876234
	L88_2.Height = 265.7876234

	L89_1.y = 961.8950522
	L89_2.y = 961.8950522
	L89_1.Height = 273.1066319
	L89_2.Height = 273.1066319

	L90_1.y = 919.5225125
	L90_2.y = 919.5225125
	L90_1.Height = 280.4256403
	L90_2.Height = 280.4256403

	L91_1.y = 877.1499729
	L91_2.y = 877.1499729
	L91_1.Height = 287.7446488
	L91_2.Height = 287.7446488

	L92_1.y = 834.7774333
	L92_2.y = 834.7774333
	L92_1.Height = 295.0636573
	L92_2.Height = 295.0636573

	L93_1.y = 792.4048936
	L93_2.y = 792.4048936
	L93_1.Height = 302.3826657
	L93_2.Height = 302.3826657

	L94_1.y = 750.032354
	L94_2.y = 750.032354
	L94_1.Height = 309.7016742
	L94_2.Height = 309.7016742

	L95_1.y = 707.6598144
	L95_2.y = 707.6598144
	L95_1.Height = 317.0206827
	L95_2.Height = 317.0206827

	L96_1.y = 665.2872747
	L96_2.y = 665.2872747
	L96_1.Height = 324.3396911
	L96_2.Height = 324.3396911

	L97_1.y = 622.9147351
	L97_2.y = 622.9147351
	L97_1.Height = 331.6586996
	L97_2.Height = 331.6586996

	L98_1.y = 580.5421955
	L98_2.y = 580.5421955
	L98_1.Height = 338.9777081
	L98_2.Height = 338.9777081

	L99_1.y = 538.1696558
	L99_2.y = 538.1696558
	L99_1.Height = 346.2967165
	L99_2.Height = 346.2967165

	L100_1.y = 495.7971162
	L100_2.y = 495.7971162
	L100_1.Height = 353.615725 
	L100_2.Height = 353.615725 


	'Side Ramp Phasers
	dim xOffset : xoffset = 75
	'ambients
	WarpAmbient1.X = 161.68448901 + xOffset
	WarpAmbient1.y = 648.16642475
	WarpAmbient1.Height = 137
	WarpAmbient1.RotY = - 19

	WarpAmbient2.X = 149.46 + xOffset
	WarpAmbient2.y = 552.245
	WarpAmbient2.Height = 171.5
	WarpAmbient2.RotY = - 19.924

	WarpAmbient3.X = 158.4 + xOffset
	WarpAmbient3.y = 446.45
	WarpAmbient3.Height = 208.355
	WarpAmbient3.RotY = -19.74

	WarpAmbient4.X = 178.14 + xOffset
	WarpAmbient4.y = 339.714
	WarpAmbient4.Height = 243.1
	WarpAmbient4.RotY = -16.21399

	WarpAmbient5.X = 221.87 + xOffset
	WarpAmbient5.y = 235.8
	WarpAmbient5.Height = 252.544
	WarpAmbient5.RotY = -0.189


	L70_1.x = 137.136
	L70_1.y = 655.557
	L70_1.Height = 87.521
	l70_1.roty = -109

	L70_2.x = 126.604
	L70_2.y = 604.975
	L70_2.Height = 106.238
	l70_2.roty = -96
	
	L71_1.x = 124.548
	L71_1.y = 554.177
	L71_1.Height = 123.257
	l71_1.roty = -90
	
	L71_2.x = 127.497
	L71_2.y = 502.715
	L71_2.Height = 142.719
	l71_2.roty = -85
	
	L72_1.x = 133.88
	L72_1.y = 451.491
	L72_1.Height = 161.506
	l72_1.roty = -81.5
	
	L72_2.x = 142.943
	L72_2.y = 399.34
	L72_2.Height = 178.265
	l72_2.roty = -79	

	L73_1.x = 154.389
	L73_1.y = 348.022
	L73_1.Height = 194.677
	l73_1.roty = -74.5
	
	L73_2.x = 170.564
	L73_2.y = 297.373
	L73_2.Height = 206.259
	l73_2.roty = -68
	
	L74_1.x = 195.738
	L74_1.y = 247.988
	L74_1.Height = 211.327
	l74_1.roty = -60	'problems	tight
	
	L74_2.x = 222.427
	L74_2.y = 202.773
	L74_2.Height = 210.808
	l74_2.roty = -56
	
	L75_1.x = 257.309
	L75_1.y = 160.71
	L75_1.Height = 206.347
	l75_1.roty = -47
	
	L75_2.x = 298.043
	L75_2.y = 121
	L75_2.Height = 199.716
	l75_2.roty = -41
	
	L76_1.x = 337
	L76_1.y = 87
	L76_1.Height = 190.69
	l76_1.roty = -37
	
	L76_2.x = 384
	L76_2.y = 56
	L76_2.Height = 179.695
	l76_2.roty = -29
	
	L77_1.x = 429
	L77_1.y = 34
	L77_1.Height = 167.96
	l77_1.roty = -21
	
	L77_2.x = 480
	L77_2.y = 18
	L77_2.Height = 158.755
	l77_2.roty = -12
	
	dim aLA, aRa
	aLA = Array(L79_3, L80_3, L81_3, L82_3, L83_3, L84_3, L85_3,_
		L86_3, L87_3, L88_3, L89_3, L90_3, L91_3, L92_3, L93_3, L94_3,L95_3,L96_3,L97_3,L98_3,L99_3,L100_3)
	aRA = Array(L79_4, L80_4, L81_4, L82_4, L83_4, L84_4, L85_4,_
		L86_4, L87_4, L88_4, L89_4, L90_4, L91_4, L92_4, L93_4, L94_4,L95_4,L96_4,L97_4,L98_4,L99_4,L100_4)

	for x = 0 to uBound(aL)
		aLa(x).x = aL(x).x
		aLa(x).y = aL(x).y
		aLa(x).height = aL(x).Height
		
		aLa(x).y = aLa(x).y + 40.3323
		aLa(x).Height = aLa(x).Height + 46.75885773

		aRa(x).x = aR(x).x
		aRa(x).y = aR(x).y
		aRa(x).height = aR(x).Height
		
		aRa(x).y = aRa(x).y + 40.3323
		aRa(x).Height = aRa(x).Height + 46.75885773

		aLa(X).rOTy = 80.12206552 - 90
		aRa(X).rOTy = 80.12206552 - 90
	next
End Sub



'Ballshadow routine by Ninuzzu

Dim BallShadow
BallShadow = Array (BallShadow1, BallShadow2, BallShadow3, BallShadow4, BallShadow5, BallShadow6)

Sub UpdateBallShadow()	'called by -1 lamptimer
	On Error Resume Next
    Dim BOT, b
    BOT = GetBalls
	dim CenterPoint : CenterPoint = 441.7425'Table1.Width/2

	' render the shadow for each ball
    For b = 0 to UBound(BOT)
		If BOT(b).X < CenterPoint Then
			BallShadow(b).X = ((BOT(b).X) - (50/6) + ((BOT(b).X - (CenterPoint))/7)) + 10
		Else
			BallShadow(b).X = ((BOT(b).X) + (50/6) + ((BOT(b).X - (CenterPoint))/7)) - 10
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




'Setup Flipper End Points 
Dim EndPointL, EndPointR
EndPointL = 376.2168
EndPointR = 489.7015




'==,ggggggggggg,=========================================================================
'dP"""88""""""Y8,              ,dPYb,                                I8              
'Yb,  88      `8b              IP'`Yb                                I8              
' `"  88      ,8P              I8  8I                         gg  88888888           
'     88aaaad8P"               I8  8'                         ""     I8              
'     88"""""       ,ggggg,    I8 dP    ,gggg,gg   ,gggggg,   gg     I8    gg     gg 
'     88           dP"  "Y8ggg I8dP    dP"  "Y8I   dP""""8I   88     I8    I8     8I 
'     88          i8'    ,8I   I8P    i8'    ,8I  ,8'    8I   88    ,I8,   I8,   ,8I 
'     88         ,d8,   ,d8'  ,d8b,_ ,d8,   ,d8b,,dP     Y8,_,88,_ ,d88b, ,d8b, ,d8I 
'     88         P"Y8888P"    8P'"Y88P"Y8888P"`Y88P      `Y88P""Y888P""Y88P""Y88P"888
'===============================================================================,d8I'====
'               														      ,dP'8I 
'Tracks balls, Adjusts ball velocity and adjusts shot angle                  ,8"  8I 
'Part 1 - Ball velocity Hack                               	                 I8   8I 
'Part 2 - Polarity and Velocity Adjustments on 3 or 5-point envelopes        `8, ,8I 
'                                                                             `Y8P"  
'Setup -                                                             
'Triggers tight to the flippers TriggerLF and TriggerRF. Timers as low as possible, but > 80ms
'Debug box TBpl 
'On Flipper Call:
'	ProcessballsL
'	...
'	ProcessballsR
'set up flipper end X coords in variables EndpointL / EndpointR (or in FTS flipper test script)


'-----------Configuration---------------
dim PolarityMod(5, 1), VelMod(5, 1), Ydiminish(3, 1), VelXmod(5, 1)

'Coord PolarityMod, 1, x, y
Sub Coord(N,A,X,Y):a(n, 0) = x :a(n,1) = y : End Sub 'Point #, Array name, XCoord, YCoord

PolarityMod(0,0) = "PolarityMod"
VelMod(0,0) = "VelMod"
Ydiminish(0,0) = "Ydiminish"
VelXmod(0,0) = "VelXmod"

'x = % Position of Flipper
'Y = Output Coefficient for calculation

polarityenabled = True'False

'Stern
m1 = 1 : m2 = 1 : m3 = 50 : m4 = 0.93 'Setup Vel Falloff Line
Coord 1, PolarityMod, 0.38, -4.5		'Early1
Coord 2, PolarityMod, 0.65, -4		'Early2
Coord 3, PolarityMod, 0.84, 0		'Middle
Coord 4, PolarityMod, 0.97, 3.2	'Late3
Coord 5, PolarityMod, 1.05,  7	'Late4

'Coord 1, VelMod, 0.30,	0.9		'Early1
'Coord 2, VelMod, 0.596, 1'0.97		'Early2
'Coord 3, VelMod, 0.782, 1			'Middle
'Coord 4, VelMod, 0.941,  0.95'0.9		'Late3
'Coord 5, VelMod, 1.1, 	0.825'0.85		'Late4

'Stern, with slight velocity adjustment for 0.8 EOStorque (2mass/5000strength)
Coord 1, VelMod, 0.30,	0.885'0.9		'Early1
Coord 2, VelMod, 0.596, 0.989'1		'Early2
Coord 3, VelMod, 0.782, 0.991			'Middle
Coord 4, VelMod, 0.941,  0.95'0.9		'Late3
Coord 5, VelMod, 1.1, 	0.825'0.85		'Late4

'WPC Steep (71.1/120)
'm1 = 1 : m2 = 1 : m3 = 10 : m4 = 0.913
'Coord 1, PolarityMod, 0.38, -3.5		'Early1
'Coord 2, PolarityMod, 0.596, -5		'Early2
'Coord 3, PolarityMod, 0.8, -2		'Middle
'Coord 4, PolarityMod, 0.97, -1.5	'Late3
'Coord 5, PolarityMod, 1.05,  0	'Late4
'kinda improper. Adjust speed so that these cap out at 1!
'Coord 1, VelMod, 0.30,	0.9		'Early1
'Coord 2, VelMod, 0.596, 0.95'0.97		'Early2
'Coord 3, VelMod, 0.745, 0.965			'Middle
'Coord 4, VelMod, 0.941,  0.95'0.9		'Late3
'Coord 5, VelMod, 1.1, 	0.95'0.85		'Late4


Coord 1, Ydiminish, RightFlipper.Y-65,	 0		' Earliest Flipper (keep at 1)
Coord 2, Ydiminish, RightFlipper.Y-11, 	1		' Mid Point
Coord 3, Ydiminish, RightFlipper.Y,	 1	' Latest Flipper



'Part 1 - Overall Velocity Hack
'***************************
Dim LFon, RFon, RF1on, SpeedLimit, M1, M2, M3, M4

Sub TriggerLF_Timer(): LFon = False : me.TimerEnabled = 0 : End Sub
Sub TriggerRF_Timer(): RFon = False : me.TimerEnabled = 0 : End Sub
Sub TriggerRF1_Timer(): RF1on = False : me.TimerEnabled = 0 : End Sub
Sub TriggerRF1_UnHit(): if RF1on then FlipSpeedHack m1, m2, m3, m4, False End If : End Sub

Sub FlipSpeedHack(X1, Y1, X2, Y2, CutoffBool)	'Two points	
	if CutoffBool then if activeball.vely > 0 then exit sub	'if ball is going Down, exit sub (inappropriate for upper flippers)
	Dim FinalSpeed : FinalSpeed = BallSpeed(ActiveBall) : if FinalSpeed < x1 then Exit Sub
	Dim VelCoef : VelCoef = SlopeIt(FinalSpeed,X1,Y1,X2,Y2)
		if VelCoef < Y1 then VelCoef = Y1	'Clamp Low
		if VelCoef > Y2 then VelCoef = Y2	'Clamp High
		activeball.velx = activeball.velx * VelCoef
		activeball.vely = activeball.vely * VelCoef
		
		'Dim DebugString : DebugString = "Flip" & vbnewline
		'FalloffDebugBox TBflipper, Finalspeed, BallSpeed(ActiveBall), VelCoef, DebugString
End Sub
Sub TBflipper_Timer():me.timerenabled = False : me.text = Empty : End Sub


'=====================================
'Part 2 
'Ball Tracking Polarity Correction
'=====================================

'0.09a
'	-	Improved Envelope Functions
'	-	fixed greater than / less than errors
'0.09b - script cleanup, removed unused stuff

Dim Lballstack(9, 5)
Dim Rballstack(9, 5)
'0 = Object reference
'1 = ballID kept in integer (trigger unhit compares this to activeball.ID for wiping ball from stack)
'2 = Ball X pos (set by flip for Polarity correction, wiped on trigger unhit)
'3 = Ball Y pos (TODO)
'4 = Ball X vel
'5 = Partial Flip Coefficient  (kept in 0, 5 only)

Initballstacks
Sub Initballstacks() : dim x: for x = 0 to 9 : Set Lballstack(x,0) = Nothing : Set Rballstack(x,0) = Nothing : next : End Sub

'Left Flipper ====================================

' TypeName fails on Nothing
' https://bugs.winehq.org/show_bug.cgi?id=55969
' so we specifically return "Nothing" for Nothing
Function FixedTypeName(obj)
	If obj Is Nothing Then
		FixedTypeName = "Nothing"
	Else
		FixedTypeName = TypeName(obj)
	End If
End Function

Sub TriggerLF_Hit()	'add a ball to the stack
'	tb.text = activeball.mass
	dim x : for x = 0 to 9
		if FixedTypeName(Lballstack(x, 0)) = "Nothing" then 
			Set Lballstack(x, 0) = activeball
			Lballstack(x, 1) = activeball.id
			exit For
		End If
	Next
End Sub

Sub TriggerLF_UnHit() 'proc Polarity Correction, then wipe X coords from column 2
	if LFon then 	'FalloffNF
		dim x : for x = 0 to 9	'If X position is set, call Polarity Correction for that object
			if Lballstack(x, 2) > 0 then 
				PolarityCorrect Lballstack(x, 0), Lballstack(x, 2), Lballstack(x, 3), Lballstack(x, 4), Lballstack(0, 5), 0
			End If
		Next
		for x = 0 to 9 	'wipe X Positions
			Lballstack(x, 2) = Empty
		Next
		FlipSpeedHack m1, m2, m3, m4, True
	End If 
	for x = 0 to 9 	'Remove ball from stack...
		if activeball.id = Lballstack(x, 1) then Set Lballstack(x, 0) = Nothing
	Next
End Sub

Sub ProcessballsL()	'note X position of balls in flipper area
	TriggerLF.TimerEnabled = 1
	LFon = True
	dim x : for x = 0 to 9 'Count X positions of balls in array
		if FixedTypeName(Lballstack(x, 0)) = "IBall" then
			Lballstack(x, 2) = Lballstack(x, 0).X
			Lballstack(x, 3) = Lballstack(x, 0).Y
			Lballstack(x, 4) = Lballstack(x, 0).VelX
		End If
	Next
	'dim totalrotation, currentangler, b
	'CurrentAngler = (LeftFlipper.StartAngle - LeftFlipper.CurrentAngle)
	'TotalRotation = (LeftFlipper.StartAngle - LeftFlipper.EndAngle)
	dim b
	b = ((LeftFlipper.StartAngle - LeftFlipper.CurrentAngle) / (LeftFlipper.StartAngle - LeftFlipper.EndAngle))
	b = abs(b-1)	'invert
	Lballstack(0, 5) = b	'Partial Flip Coefficient 
	'tb.text = LeftFlipper.StartAngle - LeftFlipper.EndAngle & vbnewline & leftflipper.CurrentAngle & vbnewline & b
end Sub

'Right Flipper ====================================

Sub TriggerRF_Hit()	'add a ball to the stack
	dim x : for x = 0 to 9
		if FixedTypeName(Rballstack(x, 0)) = "Nothing" then 
			Set Rballstack(x, 0) = activeball
			Rballstack(x, 1) = activeball.id
			exit For
		End If
	Next
End Sub

Sub TriggerRF_UnHit() 'proc Polarity Correction, then wipe X coords from column 2
	if RFon then 	'FalloffNF
		dim x : for x = 0 to 9	'If X position is set, call Polarity Correction for that object
			if Rballstack(x, 2) > 0 then 
				PolarityCorrect Rballstack(x, 0), Rballstack(x, 2), Rballstack(x, 3) , Rballstack(x, 4), Rballstack(0, 5), 1
			End If
		Next
		for x = 0 to 9 	'wipe X Positions
			Rballstack(x, 2) = Empty
		Next
		FlipSpeedHack m1, m2, m3, m4, True
	End If 
	for x = 0 to 9 	'Remove ball from stack...
		if activeball.id = Rballstack(x, 1) then Set Rballstack(x, 0) = Nothing
	Next
End Sub

Sub tbBS_Timer()	'debug textbox
'	on error resume next
	dim y(9), x : for x = 0 to 9
		y(x) = FixedTypeName(Rballstack(x, 0))
		if FixedTypeName(Rballstack(x, 0)) = "IBall" then y(x) = y(x) & " " & Rballstack(x, 0).ID
		y(x) = y(x) & " " & Rballstack(x, 2)
	Next
	me.text = "Ball 1: " & y(0) & " " & Rballstack(0,1) & vbnewline & _
			  "Ball 2: " & y(1) & " " & Rballstack(1,1) & vbnewline & _
			  "Ball 3: " & y(2) & " " & Rballstack(2,1) & vbnewline & _
			  "Ball 4: " & y(3) & " " & Rballstack(3,1) & vbnewline & _
			  "Ball 5: " & y(4) & " " & Rballstack(4,1) & vbnewline & _
			  "Ball 6: " & y(5) & " " & Rballstack(5,1) & vbnewline & _
			  "Ball 7: " & y(6) & " " & Rballstack(6,1) & vbnewline & _
			  "Ball 8: " & y(7) & " " & Rballstack(7,1) & vbnewline & _
			  "Ball 9: " & y(8) & " " & Rballstack(8,1) & vbnewline & _
			  "Ball10: " & y(9) & " " & Rballstack(9,1) & vbnewline & _
			  "..."
End Sub

Sub ProcessballsR()	'note X position of balls in flipper area
	TriggerRF.TimerEnabled = 1
	RFon = True
	dim x : for x = 0 to 9 'Count X positions of balls in array
		if FixedTypeName(Rballstack(x, 0)) = "IBall" then
			Rballstack(x, 2) = Rballstack(x, 0).X
			Rballstack(x, 3) = Rballstack(x, 0).Y
			Rballstack(x, 4) = Rballstack(x, 0).VelX
		End If
	Next
	dim b
	b = ((RightFlipper.StartAngle - RightFlipper.CurrentAngle) / (RightFlipper.StartAngle - RightFlipper.EndAngle))
	b = abs(b-1)	'invert
	Rballstack(0, 5) = b	'Partial Flip Coefficient 
	'tb.text = RightFlipper.StartAngle - RightFlipper.EndAngle & vbnewline & RightFlipper.CurrentAngle & vbnewline & b
end Sub


'Puts an input X through a five-point, four line envelope with flat clamping at the ends
'Function Procedures: Input X, 2D array, special (if True, inverts first two lines for polarity script)
'This 2d Array should start at 1 and end at 5. 
'0,0 is used to hold an identifying string for debug purposes
'This 2d Array should hold X data in (x, 0) & Y data in (x, 1)
'Input X, Output Y   
Function FivePointEnvelope(xInput, yArray)', special)
	dim y, testF
	If xInput < yArray(2,0) Then	'Setup X Points 	'please keep array X coords sequential!
		y = SlopeIt(xInput, yArray(1,0), yArray(1,1), yArray(2,0), yArray(2, 1)	)
		If yArray(1,1) > yArray(2,1) then 
			if y > yArray(1,1) then y = yArray(1,1) 'Clamp Low End	
		elseif yArray(1,1) <= yArray(2,1) then
			if y <= yArray(1,1) then y = yArray(1,1) 'Clamp Low End				
		End If
		testF = yArray(0,0) & " L1 (early1) " & "x= " & xInput & " y= " & y
	elseif xInput < yArray(3,0) Then	'l2
		y = SlopeIt(xInput, yArray(2,0), yArray(2,1), yArray(3,0), yArray(3,1)	)
		testF =  yArray(0,0) & " L2 (early2)" & "x= " & xInput & " y= " & y
	Elseif xInput < yArray(4,0) Then	'l3
		y = SlopeIt(xInput, yArray(3,0), yArray(3,1), yArray(4,0), yArray(4,1)	)
		testF = yArray(0,0) & " L3 (late3)" & "x= " & xInput & " y= " & y
	Elseif xInput >= yArray(4,0) Then	'l4
		y = SlopeIt(xInput, yArray(4,0), yArray(4,1), yArray(5,0), yArray(5,1)	)		
		If yArray(5,1) > yArray(4,1) then 	 'Clamp High End	
			if y > yArray(5,1) then y = yArray(5,1)
		elseif yArray(5,1) <= yArray(4,1) then 
			if y <= yArray(5,1) then y = yArray(5,1)		
		End If		
		testF = yArray(0,0) & " L4 (late4) " & " x= " & xInput & " y= " & y
	Else
	debug.print "5error: " & yArray(0,0) & ", xinput = " & xInput : y = 1
	End If
	FivePointEnvelope = y
End Function

dim TestSpecial

Function ThreePointEnvelope(xInput, yArray)
	dim y, test
	If xInput < yArray(2,0) Then
		y = SlopeIt(xInput, yArray(1,0), yArray(1,1), yArray(2,0), yArray(2,1)	)
		If yArray(1,1) > yArray(2,1) then  'Clamp Low End	
			if y > yArray(1,1) then y = yArray(1,1)
		elseif yArray(1,1) <= yArray(2,1) then
			if y < yArray(1,1) then y = yArray(1,1)		
		End If
		test = "L1 (earliest) " & yArray(0,0) & " x= " & xInput & " y= " & y
	Elseif xInput >= yArray(2,0) Then	'l2
		y = SlopeIt(xInput, yArray(2,0), yArray(2,1), yArray(3,0), yArray(3,1)	)
		If yArray(3,1) > yArray(2,1) then 	 'Clamp High End	
			if y > yArray(3,1) then y = yArray(3,1)
		elseif yArray(3,1) <= yArray(2,1) then 
			if y < yArray(3,1) then y = yArray(3,1)		
		End If	
		test = "L3 (latest) " & yArray(0,0) & " x= " & xInput & " y= " & y
	Else
	'debug.print "3error: " & yArray(0,0) & ", xinput = " & xInput
	y = 1
	End If
	'debug.print test
	ThreePointEnvelope = y
End Function

dim PolarityEnabled : PolarityEnabled = True	'debug
Sub PolarityCorrect(object, xpos, ypos, xvel, PartialFLipCoef, LR)	'Corrects angle/velocity using ball data captured at flip
	if FixedTypeName(object) = "Nothing" then Exit Sub 'Bug - This happens when the ball wavers in and out of trigger maybe
	if object.vely > 0 then TBpl.text = "exit sub" : exit sub
	dim TestVar : 	TestVar = "Cutoff"	'debug string
	dim lrcoef : if lr = 1 then lrcoef = -1 else lrcoef = 1 end if	'Direction Coef- could be used to compress the script. readability tho

	dim Y 	 'output (% ball-on-flipper position, 0=base 1=tip)
	if xpos <0.15 then TBpl.text = "xpos<0.15, exit sub " & vbnewline & " y =" & round(y,3) : exit sub 'Cutoff super early
	Select Case LR	'return position of ball on flipper as a % (0=base, 1=tip)
		case 0 : y = SlopeIt(xpos, LeftFlipper.X, 0, EndpointL, 1) 	'base flipper -> 0
		case 1 : y = SlopeIt(xpos, RightFlipper.X, 0, EndpointR, 1)	'End flipper -> 1
	End Select
	if y > 1.05 then y = 1.05 	'Clamp high End
	

	''''''''''''''declare Polarity Correction + safeties'''''''''''''''
	dim AddX	'Polarity correction
	dim Ycoef:Ycoef = 1	'Safety coef #1 - Cut down Correction if the ball is sufficiently above the flipper base
	if Y > 0.65 then ycoef = ThreePointEnvelope(ypos, Ydiminish)	'Calculate Safety coef #1- if ball is above the flipper
	'PartialFLipCoef -  'Safety coef #2 - handled by processballs, another safety coefficient

	
	if not PolarityEnabled then 'If Disabled, Exit Sub Here
		TBpl.text = "%" & round(y,5) & vbnewline & "PolarityEnabled = " & PolarityEnabled
		Exit Sub	
	End If
	
	''''''''''''''''''''Apply Velocity Correction''''''''''''''
	dim velcoef 'Overall Velocity coefficient
	Velcoef = FivePointEnvelope(y, VelMod)	'five point velocity envelope based on Y (% on flipper)
	Object.Velx = Object.VelX * velcoef
	Object.Vely = Object.VelY * velcoef	

	''''''''''''''''''''Apply Polarity Correction''''''''''''''
	AddX = FivePointEnvelope(y, PolarityMod)*lrcoef	'AddX - Find polarity correction	
	object.VelX = object.VelX + 1*(AddX*ycoef*PartialFlipcoef)	'gogo			

	'''''''''''''''''''''''Debug Strings''''''''''''''''''''''
	Select Case LR
		case 0 : TestVar =  "Left:" & round(1*(AddX*ycoef*PartialFlipcoef),3) 'debug string  'left flipper
		case 1 : TestVar =  "Right:" & round(1*(AddX*ycoef*PartialFlipcoef),3) 'debug string  'Right Flipper
	End Select
	'debug stuff
	dim d1, d2, d3
	if ycoef < 1 then d1 = "ycoef: " & round(ycoef,5) & vbnewline
	if y = 1.05 then d2 = "(MAX)"
	if PartialFlipcoef < 1 then d3 = "PartialFlipcoef: " & round(PartialFlipcoef,4) & vbnewline
	TBpl.text =  "%" & round(y,3) & d2 & vbnewline & _ 
				 TestVar & vbnewline & _
				 d1 & d3 & vbnewline & _ 
				 " "
End Sub

Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

'======================================
Function SlopeIt(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m
	x = input
	m = (Y2 - Y1) / (X2 - X1)
	b = Y2 - m*X2

	Y = M*x+b
	SlopeIt = Y
End Function
'======================================

' Ramp Sounds
Sub RampSoundR_Hit()		' - Entry / Reject
 	If activeball.vely < -10 then 
		WireRampOn True
	End If
End Sub

Sub RampSoundRw_Hit()		' - Wireform Start
	WireRampOff
	WireRampOn False
End Sub

Sub RampSoundL_Hit()		' - Entry / Reject
 	If activeball.vely < -10 then 
		WireRampOn True
	End If
End Sub

Sub RampSoundLw_Hit()		' - Wireform Start
	WireRampOff
	WireRampOn False
End Sub

Sub RampSoundS_Hit()		' - Entry / Reject
 	If activeball.velx < -10 then 
		WireRampOn True
	End If
End Sub

Sub LeftRampMiddleStart_Hit
	WireRampOn True
End Sub

'******************************************************
' 	ZFLE:  FLEEP MECHANICAL SOUNDS
'******************************************************

Function PSlope(Input, X1, Y1, X2, Y2)		'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1					  'volume level; range [0, 1]
NudgeLeftSoundLevel = 1				 'volume level; range [0, 1]
NudgeRightSoundLevel = 1				'volume level; range [0, 1]
NudgeCenterSoundLevel = 1			   'volume level; range [0, 1]
StartButtonSoundLevel = 0.1			 'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr   'volume level; range [0, 1]
PlungerPullSoundLevel = 1			   'volume level; range [0, 1]
RollingSoundFactor = 1.1 / 5

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010		'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635		'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0					   'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45					'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel		'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel	   'sound helper; not configurable
SlingshotSoundLevel = 0.95					  'volume level; range [0, 1]
BumperSoundFactor = 4.25						'volume multiplier; must not be zero
KnockerSoundLevel = 1						   'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2		  'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055 / 5			 'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075 / 5			   'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.375 / 5			'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025		   'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025		   'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8	  'volume level; range [0, 1]
WallImpactSoundFactor = 0.075				   'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075 / 3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5 / 5			'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10	 'volume multiplier; must not be zero
DTSoundLevel = 0.25				 'volume multiplier; must not be zero
RolloverSoundLevel = 0.25		   'volume level; range [0, 1]
SpinnerSoundLevel = 0.5			 'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor

DrainSoundLevel = 0.8				   'volume level; range [0, 1]
BallReleaseSoundLevel = 1			   'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2	'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015	 'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025 / 5			 'volume multiplier; must not be zero

'/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////
'/////////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ////////////////////////////
' Positional sound playback methods will play a sound, depending on the X,Y position of the table element or depending on ActiveBall object position
' These are similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
' For surround setup - positional sound playback functions will fade between front and rear surround channels and pan between left and right channels
' For stereo setup - positional sound playback functions will only pan between left and right channels
' For mono setup - positional sound playback functions will not pan between left and right channels and will not fade between front and rear channels

' PlaySound full syntax - PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
' Note - These functions will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, - 1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, - 1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, aVol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
	PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	PlaySound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	PlaySound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.y * 2 / tableheight - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioFade = CSng(tmp ^ 10)
	Else
		AudioFade = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.x * 2 / tablewidth - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioPan = CSng(tmp ^ 10)
	Else
		AudioPan = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = CSng(BallVel(ball) ^ 2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = CSng((ball.velz) ^ 2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = Int(Sqr((ball.VelX ^ 2) + (ball.VelY ^ 2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * CSng(BallVel(ball) ^ 3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^ 2 * 15
End Function

Function RndInt(min, max) ' Sets a random number integer between min and max
	RndInt = Int(Rnd() * (max - min + 1) + min)
End Function

Function RndNum(min, max) ' Sets a random number between min and max
	RndNum = Rnd() * (max - min) + min
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////

Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeLeftSoundLevel * VolumeDial, - 0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
End Sub

Sub SoundPlungerPull()
	PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub

'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////

Sub KnockerSolenoid()
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////

Sub RandomSoundDrain(drainswitch)
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd * 11) + 1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd * 7) + 1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd * 10) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd * 8) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////

Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
End Sub

'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////

Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-L01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-R01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////

Sub RandomSoundFlipperUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd * 7) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd * 8) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm / 10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm / 10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd * 7) + 1), parm * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////

Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd * 4) + 1), RolloverSoundLevel
End Sub

Sub aRollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////

Sub aRubbers_Hit(idx)
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 5 Then
		RandomSoundRubberStrong 1
	End If
	If finalspeed <= 5 Then
		RandomSoundRubberWeak()
	End If
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////

Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd * 10) + 1
		Case 1
			PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 2
			PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 3
			PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 4
			PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 5
			PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 6
			PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 7
			PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 8
			PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 9
			PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 10
			PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6 * voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////

Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd * 9) + 1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////

Sub Walls_Hit(idx)
	RandomSoundWall()
End Sub

Sub RandomSoundWall()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 5) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 4) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 3) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////

Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd * 13) + 1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub aMetals_Hit (idx)
	RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
	RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////

Sub RandomSoundBottomArchBallGuide()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Bounce_" & Int(Rnd * 2) + 1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////

Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd * 3) + 1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(ActiveBall.id) < 4) And cor.ballvely(ActiveBall.id) > 7 Then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////

Sub RandomSoundFlipperBallGuide()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd * 3) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd * 7) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////

Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 10 Then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft ActiveBall
	Else
		RandomSoundTargetHitWeak()
	End If
End Sub

Sub aTargets_Hit (idx)
	PlayTargetSound
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////

Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd * 9) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd * 7) + 1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////

Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd * 5) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd * 2) + 1), GateSoundLevel, ActiveBall
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, ActiveBall
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)
	SoundPlayfieldGate
End Sub

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub Arch1_hit()
	If ActiveBall.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If ActiveBall.velx <  - 8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If ActiveBall.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If ActiveBall.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd * 2) + 1), SaucerLockSoundLevel, ActiveBall
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0
			PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1
			PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////

Sub OnBallBallCollision(ball1, ball2, velocity)
	Dim snd
	Select Case Int(Rnd * 7) + 1
		Case 1
			snd = "Ball_Collide_1"
		Case 2
			snd = "Ball_Collide_2"
		Case 3
			snd = "Ball_Collide_3"
		Case 4
			snd = "Ball_Collide_4"
		Case 5
			snd = "Ball_Collide_5"
		Case 6
			snd = "Ball_Collide_6"
		Case 7
			snd = "Ball_Collide_7"
	End Select
	
	PlaySound (snd), 0, CSng(velocity) ^ 2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd * 6) + 1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd * 6) + 1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315  'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05	  'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025 * RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025 * RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025 * RelayFlashSoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025 * RelayFlashSoundLevel, obj
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  END FLEEP MECHANICAL SOUNDS
'******************************************************

'******************************************************
'	ZBRL:  BALL ROLLING AND DROP SOUNDS
'******************************************************

' Be sure to call RollingUpdate in a timer with a 10ms interval see the GameTimer_Timer() sub
Sub GameTimer_Timer
	RollingUpdate
End Sub

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 To tnob
		rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	Dim b
	Dim gBOT
	gBOT = GetBalls
	
	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 To tnob - 1
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
		'If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next
	
	' exit the sub if no balls on the table
	If UBound(gBOT) =  - 1 Then Exit Sub
	
	' play the rolling sound for each ball
	For b = 0 To UBound(gBOT)
		If BallVel(gBOT(b)) > 1 And gBOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), - 1, VolPlayfieldRoll(gBOT(b)) * BallRollVolume * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))
		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If
		
		' Ball Drop Sounds
		If gBOT(b).VelZ <  - 1 And gBOT(b).z < 55 And gBOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If gBOT(b).velz >  - 7 Then
					RandomSoundBallBouncePlayfieldSoft gBOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard gBOT(b)
				End If
			End If
		End If
		
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If
		
		' "Static" Ball Shadows
		' Comment the next If block, if you are not implementing the Dynamic Ball Shadows
		'If AmbientBallShadowOn = 0 Then
		'	If gBOT(b).Z > 30 Then
		'		BallShadowA(b).height = gBOT(b).z - BallSize / 4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
		'	Else
		'		BallShadowA(b).height = 0.1
		'	End If
		'	BallShadowA(b).Y = gBOT(b).Y + offsetY
		'	BallShadowA(b).X = gBOT(b).X + offsetX
		'	BallShadowA(b).visible = 1
		'End If
	Next
End Sub

'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************




'******************************************************
' 	ZRRL: RAMP ROLLING SFX
'******************************************************

'Ball tracking ramp SFX 1.0
'   Reqirements:
'		  * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'		  * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'		  * Create a Timer called RampRoll, that is enabled, with a interval of 100
'		  * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'		  * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'		  * To stop tracking ball
'				 * call WireRampOff
'				 * Otherwise, the ball will auto remove if it's below 30 vp units
'

Dim RampMinLoops
RampMinLoops = 4

' RampBalls
' Setup:  Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RampBalls(6,2)
Dim RampBalls(6,2)
'x,0 = ball x,1 = ID, 2 = Protection against ending early (minimum amount of updates)

'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
' Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
' Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
Dim RampType(6)

Sub WireRampOn(input)
	Waddball ActiveBall, input
	RampRollUpdate
End Sub

Sub WireRampOff()
	WRemoveBall ActiveBall.ID
End Sub

' WaddBall (Active Ball, Boolean)
Sub Waddball(input, RampInput) 'This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	Dim x
	For x = 1 To UBound(RampBalls)	'Check, don't add balls twice
		If RampBalls(x, 1) = input.id Then
			If Not IsEmpty(RampBalls(x,1) ) Then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next
	
	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 To UBound(RampBalls)
		If IsEmpty(RampBalls(x, 1)) Then
			Set RampBalls(x, 0) = input
			RampBalls(x, 1) = input.ID
			RampType(x) = RampInput
			RampBalls(x, 2) = 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			Exit Sub
		End If
		If x = UBound(RampBalls) Then	 'debug
			Debug.print "WireRampOn error, ball queue Is full: " & vbNewLine & _
			RampBalls(0, 0) & vbNewLine & _
			TypeName(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbNewLine & _
			TypeName(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbNewLine & _
			TypeName(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbNewLine & _
			TypeName(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbNewLine & _
			TypeName(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbNewLine & _
			" "
		End If
	Next
End Sub

' WRemoveBall (BallId)
Sub WRemoveBall(ID) 'This subroutine is called from the RampRollUpdate subroutine and is used to remove and stop the ball rolling sounds
	'   Debug.Print "In WRemoveBall() + Remove ball from loop array"
	Dim ballcount
	ballcount = 0
	Dim x
	For x = 1 To UBound(RampBalls)
		If ID = RampBalls(x, 1) Then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		End If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		If Not IsEmpty(Rampballs(x,1)) Then ballcount = ballcount + 1
	Next
	If BallCount = 0 Then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer()
	RampRollUpdate
End Sub

Sub RampRollUpdate()	'Timer update
	Dim x
	For x = 1 To UBound(RampBalls)
		If Not IsEmpty(RampBalls(x,1) ) Then
			If BallVel(RampBalls(x,0) ) > 1 Then ' if ball is moving, play rolling sound
				If RampType(x) Then
					PlaySound("RampLoop" & x), - 1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), - 1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2) = RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			End If
			If RampBalls(x,0).Z < 30 And RampBalls(x, 2) > RampMinLoops Then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		End If
	Next
	If Not RampBalls(0,0) Then RampRoll.enabled = 0
End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	Me.text = "on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbNewLine & _
	"1 " & TypeName(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbNewLine & _
	"2 " & TypeName(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbNewLine & _
	"3 " & TypeName(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbNewLine & _
	"4 " & TypeName(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbNewLine & _
	"5 " & TypeName(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbNewLine & _
	"6 " & TypeName(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbNewLine & _
	" "
End Sub

Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
	BallPitch = pSlope(BallVel(ball), 1, - 1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, - 4000, 60, 7000)
End Function

'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************

'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

Dim cor
Set cor = New CoRTracker

Class CoRTracker
	Public ballvel, ballvelx, ballvely
	
	Private Sub Class_Initialize
		ReDim ballvel(0)
		ReDim ballvelx(0)
		ReDim ballvely(0)
	End Sub
	
	Public Sub Update()	'tracks in-ball-velocity
		Dim str, b, AllBalls, highestID
		allBalls = GetBalls
		
		For Each b In allballs
			If b.id >= HighestID Then highestID = b.id
		Next
		
		If UBound(ballvel) < highestID Then ReDim ballvel(highestID)	'set bounds
		If UBound(ballvelx) < highestID Then ReDim ballvelx(highestID)	'set bounds
		If UBound(ballvely) < highestID Then ReDim ballvely(highestID)	'set bounds
		
		For Each b In allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

' Note, cor.update must be called in a 10 ms timer. The example table uses the GameTimer for this purpose, but sometimes a dedicated timer call RDampen is used.

Sub RDampen_Timer
	Cor.Update
End Sub

'******************************************************
' 	ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Sub TargetBouncer(aBall,defvalue)
	Dim zMultiplier, vel, vratio
	If TargetBouncerEnabled = 1 And aball.z < 30 Then
		'   debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		vel = BallSpeed(aBall)
		If aBall.velx = 0 Then vratio = 1 Else vratio = aBall.vely / aBall.velx
		Select Case Int(Rnd * 6) + 1
			Case 1
				zMultiplier = 0.2 * defvalue
			Case 2
				zMultiplier = 0.25 * defvalue
			Case 3
				zMultiplier = 0.3 * defvalue
			Case 4
				zMultiplier = 0.4 * defvalue
			Case 5
				zMultiplier = 0.45 * defvalue
			Case 6
				zMultiplier = 0.5 * defvalue
		End Select
		aBall.velz = Abs(vel * zMultiplier * TargetBouncerFactor)
		aBall.velx = Sgn(aBall.velx) * Sqr(Abs((vel ^ 2 - aBall.velz ^ 2) / (1 + vratio ^ 2)))
		aBall.vely = aBall.velx * vratio
		'   debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		'   debug.print "conservation check: " & BallSpeed(aBall)/vel
	End If
End Sub

'Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit(idx)
	TargetBouncer ActiveBall, 1
End Sub


'*******************************************
'  VR Plunger Code
'*******************************************

Sub TimerVRPlunger_Timer
  If Pincab_Plunger.Y < 92 then
       Pincab_Plunger.Y = Pincab_Plunger.Y + 5
  End If
End Sub

Sub TimerVRPlunger2_Timer
	Pincab_Plunger.Y = -8 + (5 * Plunger.Position) - 20
	TimerVRPlunger2.Enabled = 0
End Sub
