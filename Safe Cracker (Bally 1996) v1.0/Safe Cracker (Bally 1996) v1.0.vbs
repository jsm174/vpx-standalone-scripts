'************************************************************
'************************************************************
'
'  Safe Cracker (Bally 1996) / IPD No.  3782 / 4 Players
'
'   Credits:
'
'      VPX by fuzzel, flupper1, rothbauerw			
'      Contributors: acronovum, hauntfreaks    	
'	   VP9 Authors: ICPjuggla, OldSkoolGamer and Herweh
'	   Code snippets from Destruk, JPSalas, and Unclewilly
'
'************************************************************
'************************************************************

Option Explicit
Randomize

'******************************************************
' 						OPTIONS
'******************************************************

Const EnableBallControl = true	'set to false to disable the C key from taking manual control
Const VolumeDial = 5				'Change bolume of hit events
Const RollingSoundFactor = 1		'set sound level factor here for Ball Rolling Sound, 1=default level
Const FlasherFlare = 1				'0.5-1: 1 is essentially no flare, 0.5 would be a more pronounced flare.

'******************************************************
' 					STANDARD DEFINITIONS
'******************************************************

Dim Ballsize,BallMass
Ballsize = 50
Ballmass = 1.7

Const UseSolenoids = 1
Const UseLamps = 0
Const UseSync = 0
Const UseGI = 1
Const UseVPMModSol = 1
Const UseVPMColoredDMD = True

'******************************************************
' 					TABLE INIT
'******************************************************

if Version < 10400 then msgbox "This table requires Visual Pinball 10.4 beta or newer!" & vbnewline & "Your version: " & Version/1000

On Error Resume Next
	ExecuteGlobal GetTextFile("controller.vbs")
	If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01560000", "WPC.VBS", 3.50
keyStagedFlipperL = ""

Const cGameName="sc_18s11"

dim mPopperDish, bsBank, SCBall1, SCBall2, SCBall3, SCBall4, SpinnerBall

Sub Table1_Init
	vpmInit Me
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Safe Cracker (Bally 1996)"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		if ShowDT=true then
			.hidden=1		
		end if
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With

    '************  Main Timer init  ********************

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

    '************  Nudging   **************************

	vpmNudge.TiltSwitch = 14
	vpmNudge.Sensitivity = 5
	vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, LeftSlingShot, RightSlingShot)

 
    '************  Trough	**************************
	Set SCBall4 = sw35.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set SCBall3 = sw34.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set SCBall2 = sw33.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set SCBall1 = sw32.CreateSizedballWithMass(Ballsize/2,Ballmass)
	
	Controller.Switch(35) = 1
	Controller.Switch(34) = 1
	Controller.Switch(33) = 1
	Controller.Switch(32) = 1

	sw77wall.collidable = false
	Controller.Switch(41) = 1	'switch mapped backwards

	Set SpinnerBall = SpinnerKick.CreateSizedballWithMass(32/2,Ballmass/6)
	SpinnerBall.visible = False
	Spinnerkick.kick 0,0,0
	Spinnerkick.enabled = False

    ' magnet for popper hole
'	Set mPopperDish = New cvpmMagnet
'		mPopperDish.InitMagnet TopHoleMagnet, 5
'		mPopperDish.GrabCenter = 0
'		mPopperDish.MagnetOn = 1
'		mPopperDish.CreateEvents "mPopperDish"

	setup_backglass()

	Dim ii
	If FlasherFlare < 1 Then
		For Each ii In Flashers
			ii.ModulateVsAdd = FlasherFlare
		Next
	End If
end sub

'******************************************************
' 						KEYS
'******************************************************

Sub Table1_KeyDown(ByVal keycode)
	If keycode = PlungerKey Then
		Plunger.PullBack
		PlaySound "plungerpull",0,1,AudioPan(Plunger),0.25,0,0,1,AudioFade(Plunger)
	End If

	If keycode = 20 Then 'T key to add Token for testing
		vpmTimer.PulseSw 118
		coinAnimation
	End If

	

	If keycode = LeftTiltKey Then
		Nudge 90, 2
	End If

	If keycode = RightTiltKey Then
		Nudge 270, 2
	End If

	If keycode = CenterTiltKey Then
		Nudge 0, 2
	End If

	'************************   Start Ball Control 1/3
	if keycode = 46 then	 			' C Key
		If contball = 1 Then
			contball = 0
		Else
			contball = 1
		End If
	End If

	if keycode = 48 then 				'B Key
		If bcboost = 1 Then
			bcboost = bcboostmulti
		Else
			bcboost = 1
		End If
	End If
	if keycode = 203 then bcleft = 1		' Left Arrow
	if keycode = 200 then bcup = 1			' Up Arrow
	if keycode = 208 then bcdown = 1		' Down Arrow
	if keycode = 205 then bcright = 1		' Right Arrow
	'************************   End Ball Control 1/3
	' If keycode = LeftFlipperKey Then Flasherset17(255)
	'If keycode = RightFlipperKey Then Flasherset20(180)
	If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then
		Plunger.Fire
		PlaySound "plunger",0,1,AudioPan(Plunger),0.25,0,0,1,AudioFade(Plunger)
	End If

	'************************   Start Ball Control 2/3
	if keycode = 203 then bcleft = 0		' Left Arrow
	if keycode = 200 then bcup = 0			' Up Arrow
	if keycode = 208 then bcdown = 0		' Down Arrow
	if keycode = 205 then bcright = 0		' Right Arrow
	'************************   End Ball Control 2/3

	If vpmKeyUp(keycode) Then Exit Sub

End Sub

'******************************************************
' 						SOLENOIDS
'******************************************************

SolCallBack(1)  		= "SolLeftKickBack"
SolCallBack(2)  		= "SolTokenRelease ""L"", 82, "
SolCallback(3)  		= "SolResetVariTarget"
SolCallBack(4)  		= "SolTokenRelease ""R"", 81, "
SolCallback(5)  		= "SolBankKick"
SolCallback(6) 			= "SolPopperKickUp"
SolCallback(7) 			= "SolRampDiverter"
SolCallBack(8)  		= "SolRightKickBack"
SolCallback(9)	 		= "ReleaseBall"

SolCallback(15) 		= "SolUpperLeftTargetsUp"
SolCallback(16) 		= "SolUpperRightTargetsUp"
SolCallback(25)         = "SolPopperEject"
SolCallback(27) 		= "SolLowerLeftTargetsUp"
SolCallback(28) 		= "SolLowerRightTargetsUp"

SolCallback(35) 		= "SolPlunger"
SolCallback(36) 		= "SolLockupRelease"

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

SolCallback(23) 		= "SolFlasherStripSeq1"
SolCallback(24) 		= "SolFlasherStripSeq2"

'******************************************************
'					FLIPPERS
'******************************************************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, .67, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
        LeftFlipper.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
        LeftFlipper.RotateToStart
    End If
End Sub
'
Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, .67, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
        RightFlipper.RotateToEnd
		RightUpperFlipper.RotateToEnd
    Else
        PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
        RightFlipper.RotateToStart
		RightUpperFlipper.RotateToStart
    End If
End Sub


'******************************************************
'					PLUNGER
'******************************************************

Sub SolPlunger(Enabled)
    If Enabled Then
		AutoPlunger.Fire
		PlaySoundAt SoundFX("AutoPlunger",DOFContactors), AutoPlunger
    End If
End Sub

'******************************************************
'						SWITCHES
'******************************************************
' orbit
Sub sw15_Hit():VPMTimer.PulseSw 15:PlaySoundAt "Sensor", sw15: End Sub
Sub sw28_Hit():VPMTimer.PulseSw 28:PlaySoundAt "Sensor", sw28: End Sub

' top left and right lane
Sub sw67_Hit():vpmTimer.PulseSw 67: End Sub
Sub sw78_Hit():vpmTimer.PulseSw 78: End Sub

' return lanes
Sub sw25_Hit()   : vpmTimer.PulseSw 25: End Sub
Sub sw26_Hit():VPMTimer.PulseSw 26:PlaySoundAt "Sensor", sw26: End Sub
Sub sw27_Hit():VPMTimer.PulseSw 27:PlaySoundAt "Sensor", sw27: End Sub

' outlanes
Sub sw16_Hit():VPMTimer.PulseSw 16:PlaySoundAt "Sensor", sw16: End Sub
Sub sw17_Hit():VPMTimer.PulseSw 17:PlaySoundAt "Sensor", sw17: End Sub

' right kickback switch
Sub sw41_Hit():Controller.Switch(41) = 0:end Sub
Sub sw41_Unhit():Controller.Switch(41) = 1:end sub

' ramp switches
Sub sw83_Hit():Controller.Switch(83) = 1:end sub
Sub sw83_Unhit():Controller.Switch(83) = 0:end sub

Sub sw84_Hit():Controller.Switch(84) = 1:end sub
Sub sw84_Unhit():Controller.Switch(84) = 0:end sub


Sub t51_Hit():vpmTimer.PulseSw 51:End Sub
Sub t52_Hit():vpmTimer.PulseSw 52:End Sub
Sub t53_Hit():vpmTimer.PulseSw 53:End Sub
Sub t54_Hit():vpmTimer.PulseSw 54:End Sub
Sub t55_Hit():vpmTimer.PulseSw 55:End Sub

Sub t61_dropped():Controller.Switch(61) = 1:PlaySoundAt "droptarget", t61:End Sub
Sub t62_dropped():Controller.Switch(62) = 1:PlaySoundAt "droptarget", t62:End Sub
Sub t63_dropped():Controller.Switch(63) = 1:PlaySoundAt "droptarget", t63:End Sub

Sub t64_dropped():Controller.Switch(64) = 1:PlaySoundAt "droptarget", t64:End Sub
Sub t65_dropped():Controller.Switch(65) = 1:PlaySoundAt "droptarget", t65:End Sub
Sub t66_dropped():Controller.Switch(66) = 1:PlaySoundAt "droptarget", t66:End Sub

Sub t71_dropped():Controller.Switch(71) = 1:PlaySoundAt "droptarget", t71:End Sub
Sub t72_dropped():Controller.Switch(72) = 1:PlaySoundAt "droptarget", t72:End Sub
Sub t73_dropped():Controller.Switch(73) = 1:PlaySoundAt "droptarget", t73:End Sub

Sub t74_dropped():Controller.Switch(74) = 1:PlaySoundAt "droptarget", t74:End Sub
Sub t75_dropped():Controller.Switch(75) = 1:PlaySoundAt "droptarget", t75:End Sub
Sub t76_dropped():Controller.Switch(76) = 1:PlaySoundAt "droptarget", t76:End Sub

Sub SolUpperLeftTargetsUp(Enabled)
	If Enabled Then
		PlaySoundAt SoundFX("drop_reset_c",DOFContactors), t61
		Controller.Switch(61) = 0
		Controller.Switch(62) = 0
		Controller.Switch(63) = 0
		t61.isdropped = false
		t62.isdropped = false
		t63.isdropped = false
	End If
End Sub

Sub SolUpperRightTargetsUp(Enabled)
	If Enabled Then
		PlaySoundAt SoundFX("drop_reset_r",DOFContactors), t64
		Controller.Switch(64) = 0
		Controller.Switch(65) = 0
		Controller.Switch(66) = 0
		t64.isdropped = false
		t65.isdropped = false
		t66.isdropped = false
	End If
End Sub

Sub SolLowerLeftTargetsUp(Enabled)
	If Enabled Then
		PlaySoundAt SoundFX("drop_reset_l",DOFContactors), t71
		Controller.Switch(71) = 0
		Controller.Switch(72) = 0
		Controller.Switch(73) = 0
		t71.isdropped = false
		t72.isdropped = false
		t73.isdropped = false
	End If
End Sub

Sub SolLowerRightTargetsUp(Enabled)
	If Enabled Then
		PlaySoundAt SoundFX("drop_reset_r",DOFContactors), t74
		Controller.Switch(74) = 0
		Controller.Switch(75) = 0
		Controller.Switch(76) = 0
		t74.isdropped = false
		t75.isdropped = false
		t76.isdropped = false
	End If
End Sub

'******************************************************
'					BUMPERS
'******************************************************

Sub Bumper1_Hit()
	PlaySoundAt SoundFX("fx_bumper1",DOFContactors), bumper1
	vpmTimer.PulseSw 44
End Sub

Sub Bumper2_Hit()
	PlaySoundAt SoundFX("fx_bumper2",DOFContactors), bumper2
	vpmTimer.PulseSw 46
End Sub

Sub Bumper3_Hit()
	PlaySoundAt SoundFX("fx_bumper3",DOFContactors), bumper3
	vpmTimer.PulseSw 45
End Sub

'******************************************************
'						TROUGH 
'******************************************************

Sub sw34_Hit():Controller.Switch(34) = 1:UpdateTrough:End Sub
Sub sw34_UnHit():Controller.Switch(34) = 0:UpdateTrough:End Sub
Sub sw33_Hit():Controller.Switch(33) = 1:UpdateTrough:End Sub
Sub sw33_UnHit():Controller.Switch(33) = 0:UpdateTrough:End Sub
Sub sw32_Hit():Controller.Switch(32) = 1:UpdateTrough:End Sub
Sub sw32_UnHit():Controller.Switch(32) = 0:UpdateTrough:End Sub
Sub sw31_Hit():Controller.Switch(31) = 1:UpdateTrough:End Sub
Sub sw31_UnHit():Controller.Switch(31) = 0:UpdateTrough:End Sub


Sub UpdateTrough()
	UpdateTroughTimer.Interval = 300
	UpdateTroughTimer.Enabled = 1
End Sub

Sub UpdateTroughTimer_Timer()
'	If sw31.BallCntOver = 0 Then sw32.kick 60, 9
	If sw32.BallCntOver = 0 Then sw33.kick 60, 9
	If sw33.BallCntOver = 0 Then sw34.kick 60, 9
	If sw34.BallCntOver = 0 Then sw35.kick 60, 9
	Me.Enabled = 0
End Sub

'******************************************************
'					DRAIN & RELEASE
'******************************************************

Sub sw35_Hit() 'Drain
	UpdateTrough
	Controller.Switch(35) = 1
	PlaySoundAT "drain", sw35
End Sub

Sub sw35_UnHit()  'Drain
	Controller.Switch(35) = 0
End Sub

Sub ReleaseBall(enabled)
	If enabled Then 
		PlaySoundAt SoundFX("ballrelease",DOFContactors), sw32
		sw32.kick 60, 9
		vpmTimer.PulseSw 31
	End If
End Sub

Sub sw18_hit()
	Controller.Switch(18) = 1
End Sub

Sub sw18_unhit()
	Controller.Switch(18) = 0
End Sub


' ===============================================================================================
' roof ball trap
' ===============================================================================================

Dim RoofWalkStep

Sub RoofKickerScoop_Hit()
End Sub

Sub RoofTrigger_Hit()
		vpmTimer.PulseSw 11
end sub


' ===============================================================================================
' ball in back and bank kickout
' ===============================================================================================

Sub sw77_Hit()	
	sw77wall.collidable = true
	PlaySound "ball_bounce_low"
	Controller.Switch(77) = 1
End Sub

Sub SolBankKick(Enabled)
	If Enabled Then
		PlaySoundAt SoundFX("SolenoidOn",DOFContactors), sw77
		sw77.kickz 179, 20, 0, 130
		PlaySound "ball_bounce_low"
		Controller.Switch(77) = 0
		sw77wall.collidable = false
	End If
End Sub


Sub LocateBalls()
	Debug.print "Ball 1: x - " & SCBall1.x & " y - " & SCBall1.y & " z - " & SCBall1.z
	Debug.print "Ball 2: x - " & SCBall2.x & " y - " & SCBall2.y & " z - " & SCBall2.z
	Debug.print "Ball 3: x - " & SCBall3.x & " y - " & SCBall3.y & " z - " & SCBall3.z
	Debug.print "Ball 4: x - " & SCBall4.x & " y - " & SCBall4.y & " z - " & SCBall4.z
End Sub

'******************************************************
' 						SLINGS
'******************************************************

Dim RStep, Lstep
Sub RightSlingShot_Slingshot
	vpmTimer.pulsesw 48
    PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 2:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 3:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.pulsesw 47
    PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 2:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 3:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

'******************************************************
' 						KICKBACKS
'******************************************************

Sub SolLeftKickBack(Enabled)
	If Enabled Then LeftKickBack.Enabled = Enabled
End Sub

Sub LeftKickBack_Hit()
	PlaySoundAt SoundFX("SolenoidOn",DOFContactors), LeftKickBack
	LeftKickBack.Kick -1.5, 30 + Rnd() * 13
	LeftKickBack.Enabled = False
End Sub


Sub RightKickBackTopTrigger_Hit()
	StopSound "metalrolling"
	RightKickBackSlowDown.Enabled = True
End Sub
Sub RightKickBackSlowDown_Hit()
	RightKickBackSlowDown.Kick 180,0.1
End Sub

Sub SolRightKickBack(Enabled)
	If enabled=True then
		RightKickBack.Enabled = True
	end if
End Sub

Sub RightKickBack_Hit()
	PlaySound "SolenoidOn"
	RightKickBackSlowDown.Enabled = False
	RightKickBack.Kick 0, 50
	RightKickBack.Enabled 		= False
End Sub


' ===============================================================================================
' ramp diverter
' ===============================================================================================

Dim diverterAng:diverterAng=0
Dim diverterStep:diverterStep=-1

InitRampDiverter

Sub InitRampDiverter()
	diverterAng=0
	diverterStep=-5
	pDiverter.rotz=diverterAng
	diverterWall.IsDropped = True
	diverterTimer.Enabled 	= False
End Sub

Sub SolRampDiverter(Enabled)
	diverterWall.IsDropped = Not Enabled
	If Enabled Then
		diverterStep=-8
		PlaySoundAt SoundFX("SolenoidOn",DOFContactors), pDiverter
	Else
		diverterStep=10
		PlaySoundAt SoundFX("SolenoidOff",DOFContactors), pDiverter
	End If
	diverterTimer.Enabled 	= True
End Sub

Sub DiverterTimer_Timer()
	diverterAng = diverterAng + diverterStep
	if diverterAng<=-25 Then
		diverterTimer.Enabled 	= False
		diverterAng = -25
	end if
	if diverterAng>=0 Then
		diverterTimer.Enabled 	= False
		diverterAng = 0
	end if
	pDiverter.rotz = diverterAng
End Sub

' ===============================================================================================
' lock up
' ===============================================================================================
dim lockBallsCount:lockBallsCount=0
sub lockTrigger_Hit
	lockBallsCount=lockBallsCount+1
	if lockBallsCount=1 then
		Controller.Switch(36)=1
	end if
	if lockBallsCount=2 then
		Controller.Switch(37)=1
	end if
end Sub

dim pinAng:pinAng=0
dim pinStep:pinStep=10
Sub SolLockupRelease(Enabled)
	lockPinWall.IsDropped = Enabled
	if enabled=True Then
		pinStep=10
	Else	
		pinStep=-13
	end if
	lockPinWall.TimerEnabled=True
End Sub

Sub lockPinWall_Timer
	pinAng = pinAng + pinStep
	If pinAng>=45 Then
		pinAng=45
		lockPinWall.TimerEnabled=False
		Controller.Switch(36)=0
		Controller.Switch(37)=0
		lockBallsCount=0
	end if
	if pinAng<=0 Then
		pinAng=0
		lockPinWall.TimerEnabled=False
	end If
	pLockPin.rotx=pinAng
end Sub

'******************************************************
'	top popper eject and kick up
'******************************************************


Dim PopperBall, PBStep
Sub sw68_Hit()
	Set PopperBall 		  		= Activeball
	Playsound "metalhit2"
	Controller.Switch(68) 		= 1
	'mPopperDish.MagnetOn  		= 0		
End Sub

Sub SolPopperKickUp(Enabled)
	If Enabled Then
		If Controller.Switch(68) Then
			PlaySoundAt SoundFX("AutoPlunger",DOFContactors), sw68			
			PBStep 			   	= 0
			sw68.TimerInterval 	= 15
			sw68.TimerEnabled  	= True
		End If
	End If
End Sub

Sub SolPopperEject(Enabled)
	If Enabled Then
		If Controller.Switch(68) Then		
			sw68.Kick 185 + Rnd() * 10, 9
			PlaySoundAt SoundFX("popper_ball",DOFContactors), sw68
			PBStep 				= 99
			sw68.TimerInterval 	= 15
			sw68.TimerEnabled  	= True
		End if
	End If
End Sub

Sub sw68_Timer()
	Select Case PBStep
		Case 0
			PopperBall.z = 50
		Case 1
			PopperBall.z = 80 : PopperBall.y = PopperBall.y + 5 : Controller.Switch(68) = 0 
		Case 2, 3, 4, 5, 6
			PopperBall.z = PopperBall.z + 25
		Case 7
			PopperBall.z = 240 : PopperBall.x = PopperBall.x - 25 : sw68.kick 270,4 : Me.TimerEnabled = False ': mPopperDish.MagnetOn = 1: mPopperDish.MagnetOn = 1
		Case 99
			Me.TimerEnabled = False : Controller.Switch(68) = 0 ': mPopperDish.MagnetOn = 1
	End Select
	PBStep = PBStep + 1
End Sub

'******************************************************
' 						VARI TARGET
'******************************************************

Const VariSpring = 0.6  '0.1 - 0.9 with smaller numbers representing a stiffer spring 
Dim PrevTransY

sub SolResetVariTarget(enabled)
	if enabled Then
		ResetVariTarget.enabled = true
		PlaySoundAt SoundFX("SolenoidOn", DOFContactors), pVari
	end if
end sub

Sub ResetVariTarget_Timer()
	pVari.TransY = pVari.TransY + 20
	If pVari.TransY > 0 then
		pVari.TransY = 0
		me.enabled = False
		variTargetKicker.enabled = False
	End If
End Sub

Sub VariTargetTimer_Timer()
    Dim BOT, b, TransYDist, inLeftKickBack 
	inLeftKickBack = 0
    BOT = GetBalls
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

    For b = 0 to UBound(BOT)
        If BOT(b).Z < 35 And BOT(b).Z > 20 Then
			If InRect(BOT(b).x, BOT(b).y,130,415,198,398,254,608,184,625) Then
				TransYDist = DistancePL(BOT(b).x, BOT(b).y, 148, 434.5, 196.5, 422.5) - (Ballsize/2) - 140
	
				If TransYDist < pVari.TransY Then
					pVari.TransY = TransYDist
					If BOT(b).VelY < 0 Then
						BOT(b).VelY = BOT(b).VelY * VariSpring
						PlaysoundAtExisting "metalhit2", BOT(b)
					End If
				End If

				If pVari.TransY < -140 Then
					pVari.TransY = -140
					variTargetKicker.enabled = true
					BOT(b).VelY = 0					
				End If

				PrevTransY = pVari.TransY
			End If
			
			'Left Kickback
			If InRect(BOT(b).x, BOT(b).y,50,1005,100,1005,100,1055,50,1055) Then
				inLeftKickback = 1
			End If	
		End If	
    Next

	if inLeftKickBack then
		Controller.Switch(42) = 1
	else
		Controller.Switch(42) = 0
	end if


	If pVari.TransY = 0 Then
		Controller.Switch(56) 	= 1
		Controller.Switch(57) 	= 0
		Controller.Switch(58) 	= 0
	ElseIf pVari.TransY < -19 and pVari.TransY > -40 Then
		Controller.Switch(56) 	= 1
		Controller.Switch(57) 	= 1
		Controller.Switch(58) 	= 0
	ElseIf pVari.TransY < -39 and pVari.TransY > -60 Then
		Controller.Switch(56) 	= 1
		Controller.Switch(57) 	= 1
		Controller.Switch(58) 	= 0
'		Controller.Switch(56) 	= 0
'		Controller.Switch(57) 	= 1
'		Controller.Switch(58) 	= 0
	ElseIf pVari.TransY < -59 and pVari.TransY > -80 Then
		Controller.Switch(56) 	= 0
		Controller.Switch(57) 	= 1
		Controller.Switch(58) 	= 1
	ElseIf pVari.TransY < -79 and pVari.TransY > -100 Then
		Controller.Switch(56) 	= 0
		Controller.Switch(57) 	= 1
		Controller.Switch(58) 	= 1
'		Controller.Switch(56) 	= 0
'		Controller.Switch(57) 	= 0
'		Controller.Switch(58) 	= 1
	ElseIf pVari.TransY < -99 and pVari.TransY > -120 Then
		Controller.Switch(56) 	= 1
		Controller.Switch(57) 	= 0
		Controller.Switch(58) 	= 1
	ElseIf pVari.TransY < -119 and pVari.TransY > -140 Then
		Controller.Switch(56) 	= 1
		Controller.Switch(57) 	= 0
		Controller.Switch(58) 	= 1
'		Controller.Switch(56) 	= 0
'		Controller.Switch(57) 	= 0
'		Controller.Switch(58) 	= 0
	ElseIf pVari.TransY < -139 and pVari.TransY > -160 Then
		Controller.Switch(56) 	= 1
		Controller.Switch(57) 	= 1
		Controller.Switch(58) 	= 1
	Else
		Controller.Switch(56) 	= 1
		Controller.Switch(57) 	= 0
		Controller.Switch(58) 	= 0
	End If
End Sub

Sub variTrigger_Hit()
		vpmTimer.PulseSw 12
end Sub


 '*****************************************************************
 'Functions
 '*****************************************************************

'*** PI returns the value for PI

Function PI()

	PI = 4*Atn(1)

End Function

'*** Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy)
	Dim AB, BC, CD, DA
	AB = (bx*py) - (by*px) - (ax*py) + (ay*px) + (ax*by) - (ay*bx)
	BC = (cx*py) - (cy*px) - (bx*py) + (by*px) + (bx*cy) - (by*cx)
	CD = (dx*py) - (dy*px) - (cx*py) + (cy*px) + (cx*dy) - (cy*dx)
	DA = (ax*py) - (ay*px) - (dx*py) + (dy*px) + (dx*ay) - (dy*ax)
 
	If (AB <= 0 AND BC <=0 AND CD <= 0 AND DA <= 0) Or (AB >= 0 AND BC >=0 AND CD >= 0 AND DA >= 0) Then
		InRect = True
	Else
		InRect = False       
	End If
End Function

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		Atn2 = Sgn(dy) * (PI - Atn(Abs(dy / dx)))
	ElseIf dy = 0 Then
		Atn2 = 0
	Else
		Atn2 = Sgn(dy) * Pi / 2
	End If
End Function


'******************************************************
' 						COIN ANIM
'******************************************************
dim moveDown
sub coinAnimation
	coin.visible=1
	coin.x = 431
	coin.y = 0
	coin.z = 250
	coin.rotx=180
	coin.rotz=180+90
	moveDown=1
	Select Case Int(Rnd*4)+1
		Case 1 : coin.image="coin1"
		Case 2 : coin.image="coin2"
		Case 3 : coin.image="coin3"
		Case 4 : coin.image="coin4"
	End Select

	CoinTimer.enabled=1
end sub

Sub CoinTimer_Timer
	if moveDown then
		coin.y = coin.y + 20
		PlaySound "fx_ballrolling8", -1, 1, AudioPan(coin), 0, 0, 1, 0, AudioFade(coin)
	end if

	If Table1.ShowDT = False Then
		If coin.y > 1200 Then	
			If coin.z > 150 then coin.z = coin.z - 20
		End If
	End If

	if coin.y>1700 And movedown then
		moveDown=0
		StopSound "fx_ballrolling8"
		PlaysoundAt "coin_spinning", coin
	end if
	if moveDown=0 Then
		coin.rotz=coin.rotz+10
		if coin.rotz>1080 Then
			coin.visible=0
			CoinTimer.enabled=0
			StopSound "coin_spinning"
		end if
	end if
end Sub

Sub SolTokenRelease(tube, switch, Enabled)
	If Enabled Then
		coinAnimation
		vpmTimer.PulseSw 43
		vpmTimer.PulseSw 118
	End If
	Controller.Switch(switch) = Enabled
End Sub


'**************************************************************************************************
'**************************************************************************************************
'**************************************************************************************************

Dim discPosition, discSpinSpeed, discLastPos, SpinCounter, maxvel
dim spinAngle, degAngle, startAngle, postSpeedFactor
dim discX, discY
startAngle = 7
discX = 142.9
discY = 780.8
PostSpeedFactor = 90

Const cDiscSpeedMult = 77.5 '90             ' Affects speed transfer to object (deg/sec)
Const cDiscFriction = 2 '2.0              ' Friction coefficient (deg/sec/sec)
Const cDiscMinSpeed = 1               ' Object stops at this speed (deg/sec)
Const cDiscRadius = 50
Const cBallSpeedDampeningEffect = 0.45 ' The ball retains this fraction of its speed due to energy absorption by hitting the disc.

Sub SpinnerBallTimer_Timer()

	Dim oldDiscSpeed
	oldDiscSpeed = discSpinSpeed

	discPosition = discPosition + discSpinSpeed * Me.Interval / 1000
	discSpinSpeed = discSpinSpeed * (1 - cDiscFriction * Me.Interval / 1000)

	Do While discPosition < 0 : discPosition = discPosition + 360 : Loop
	Do While discPosition > 360 : discPosition = discPosition - 360 : Loop

	If Abs(discSpinSpeed) < cDiscMinSpeed Then
		discSpinSpeed = 0
	End If

	degAngle = -180 + startAngle + discPosition


	spinAngle = PI * (degAngle) / 180
	
	SpinnerBall.x = discX + (cDiscRadius * Cos(spinAngle))
	SpinnerBall.y = discY + (cDiscRadius * Sin(spinAngle))
	SpinnerBall.z = 25

	pdisc1.objrotz = discPosition

	If ABS(discSpinSpeed*sin(spinAngle)/postSpeedFactor) < 0.05 Then
		SpinnerBall.velx = 0.05
	Else
		SpinnerBall.velx = - discSpinSpeed*sin(spinAngle)/postSpeedFactor
	End If

	If Abs(discSpinSpeed*cos(spinAngle)/postSpeedFactor) < 0.05 Then
		SpinnerBall.vely = 0.05
	Else
		SpinnerBall.vely = discSpinSpeed*cos(spinAngle)/postSpeedFactor		'0.05
	End If

	SpinnerBall.velz = 0

'	if SQR(((discSpinSpeed*sin(spinAngle)/postSpeedFactor)^2) + ((discSpinSpeed*cos(spinAngle)/postSpeedFactor)^2)) > maxvel Then
'		maxvel = SQR(((discSpinSpeed*sin(spinAngle)/postSpeedFactor)^2) + ((discSpinSpeed*cos(spinAngle)/postSpeedFactor)^2))
'		debug.print maxvel
'	End If
'	debug.print NormAngle(spinAngle)*180/PI & " xvel: " & spinnerball.velx & " yvel: " & spinnerball.vely
'	spinnerball.visible=true

'	If discPosition > 90 and discPosition <= 180 Then
'		SpinCounter = discPosition - 90
'	ElseIf discPosition > 180 and discPosition <= 270 Then
'		SpinCounter = discPosition - 180
'	ElseIf discPosition > 270 and discPosition <= 360 Then
'		SpinCounter = discPosition - 270
'	Else
'		SpinCounter = discPosition
'	End If
'
'	If spinCounter > 15 and SpinCounter <= 30 Then
'		spinCounter = spinCounter - 15
'	ElseIf spinCounter > 30 and SpinCounter <= 45 Then
'		spinCounter = spinCounter - 30
'	ElseIf spinCounter > 45 and SpinCounter <= 60 Then
'		spinCounter = spinCounter - 45
'	ElseIf spinCounter > 60 and SpinCounter <= 75 Then
'		spinCounter = spinCounter - 60
'	ElseIf spinCounter > 75 and SpinCounter <= 90 Then
'		spinCounter = spinCounter - 75
'	End If
'
'	If SpinCounter >= 0 and SpinCounter < 3.75 Then
'		Controller.Switch(85) = 1
'		Controller.Switch(86) = 1
'	ElseIf SpinCounter >= 3.75 and SpinCounter < 7.5 Then
'		Controller.Switch(85) = 0
'		Controller.Switch(86) = 1
'	ElseIf SpinCounter >= 7.5 and SpinCounter < 11.25 Then
'		Controller.Switch(85) = 0
'		Controller.Switch(86) = 0
'	Else
'		Controller.Switch(85) = 1
'		Controller.Switch(86) = 0
'	End If

End Sub


Dim oldSwitchPos, switchCount

Sub SpinDiscSwitches_Timer()

	dim discPosDiff

	discPosDiff = oldswitchpos - discPosition

	if discPosDiff > 270 then discPosDiff = discPosDiff - 360
	if discPosDiff < -270 then discPosDiff = discPosDiff + 360

	if discPosDiff >= 3.75 Then
		oldSwitchPos = discPosition
		switchCount = switchCount - 1
	elseif discPosDiff < -3.75 Then
		oldSwitchPos = discPosition
		switchCount = switchCount + 1
	end if

	If switchCount > 4 Then switchCount = 1
	If switchCount < 1 Then switchCount = 4

	Select Case switchCount
		Case 1:Controller.Switch(85) = 1
		Case 2:Controller.Switch(86) = 1
		Case 3:Controller.Switch(85) = 0
		Case 4:Controller.Switch(86) = 0
	End Select


End Sub



'********************************************
' Ball Collision, spinner collision and Sound
'********************************************

Sub OnBallBallCollision(ball1, ball2, velocity)
	dim collAngle,bvelx,bvely,hitball
	If ball1.radius < 23 or ball2.radius < 23 then
		
		If ball1.radius < 23 Then
			collAngle = GetCollisionAngle(ball1.x,ball1.y,ball2.x,ball2.y)
			bvelx = ball2.velx
			bvely = ball2.vely
			set hitball = ball2
		else 
			collAngle = GetCollisionAngle(ball2.x,ball2.y,ball1.x,ball1.y)
			bvelx = ball1.velx
			bvely = ball1.vely
			set hitball = ball1
		End If

		dim discAngle
		discAngle = NormAngle(spinAngle)

		Dim mball, mdisc, rdisc, idisc
		
		discSpinSpeed = discSpinSpeed + sqr(bVelX ^2 + bVelY ^2) * sin(collAngle - discAngle) * cDiscSpeedMult

		PlaySound "rubber_hit_1", 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 1, 0, AudioFade(ball1)
	Else
		PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
	End If
End Sub

Function GetCollisionAngle(ax, ay, bx, by)
	Dim ang
	Dim collisionV:Set collisionV = new jVector
	collisionV.SetXY ax - bx, ay - by
	GetCollisionAngle = collisionV.ang
End Function

Function NormAngle(angle)
	NormAngle = angle
	Dim pi:pi = 3.14159265358979323846
	Do While NormAngle>2 * pi
		NormAngle = NormAngle - 2 * pi
	Loop
	Do While NormAngle <0
		NormAngle = NormAngle + 2 * pi
	Loop
End Function
 
Class jVector
     Private m_mag, m_ang, pi
 
     Sub Class_Initialize
         m_mag = CDbl(0)
         m_ang = CDbl(0)
         pi = CDbl(3.14159265358979323846)
     End Sub
 
     Public Function add(anothervector)
         Dim tx, ty, theta
         If TypeName(anothervector) = "jVector" then
             Set add = new jVector
             add.SetXY x + anothervector.x, y + anothervector.y
         End If
     End Function
 
     Public Function multiply(scalar)
         Set multiply = new jVector
         multiply.SetXY x * scalar, y * scalar
     End Function
 
     Sub ShiftAxes(theta)
         ang = ang - theta
     end Sub
 
     Sub SetXY(tx, ty)
 
         if tx = 0 And ty = 0 Then
             ang = 0
          elseif tx = 0 And ty <0 then
             ang = - pi / 180 ' -90 degrees
          elseif tx = 0 And ty>0 then
             ang = pi / 180   ' 90 degrees
         else
             ang = atn(ty / tx)
             if tx <0 then ang = ang + pi ' Add 180 deg if in quadrant 2 or 3
         End if
 
         mag = sqr(tx ^2 + ty ^2)
     End Sub
 
     Property Let mag(nmag)
         m_mag = nmag
     End Property
 
     Property Get mag
         mag = m_mag
     End Property
 
     Property Let ang(nang)
         m_ang = nang
         Do While m_ang>2 * pi
             m_ang = m_ang - 2 * pi
         Loop
         Do While m_ang <0
             m_ang = m_ang + 2 * pi
         Loop
     End Property
 
     Property Get ang
         Do While m_ang>2 * pi
             m_ang = m_ang - 2 * pi
         Loop
         Do While m_ang <0
             m_ang = m_ang + 2 * pi
         Loop
         ang = m_ang
     End Property
 
     Property Get x
         x = m_mag * cos(ang)
     End Property
 
     Property Get y
         y = m_mag * sin(ang)
     End Property
 
     Property Get dump
         dump = "vector "
         Select Case CInt(ang + pi / 8)
             case 0, 8:dump = dump & "->"
             case 1:dump = dump & "/'"
             case 2:dump = dump & "/\"
             case 3:dump = dump & "'\"
             case 4:dump = dump & "<-"
             case 5:dump = dump & ":/"
             case 6:dump = dump & "\/"
             case 7:dump = dump & "\:"
         End Select
 
         dump = dump & " mag:" & CLng(mag * 10) / 10 & ", ang:" & CLng(ang * 180 / pi) & ", x:" & CLng(x * 10) / 10 & ", y:" & CLng(y * 10) / 10
     End Property
End Class

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

Sub PlaySoundAtExisting(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp> 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

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
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function


'*****************************************
'   rothbauerw's Manual Ball Control
'*****************************************

'************************   Start Ball Control 3/3
Sub StartBallControl_Hit()
	Set ControlBall = ActiveBall
	contballinplay = true
End Sub

Sub StopBallControl_Hit()
	contballinplay = false
End Sub	

Dim bcup, bcdown, bcleft, bcright
Dim contball, contballinplay, ControlBall
Dim bcvel, bcyveloffset, bcboostmulti, bcboost

bcboost = 1		'Do Not Change - default setting
bcvel = 4		'Controls the speed of the ball movement
bcyveloffset = -0.01 	'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
bcboostmulti = 3	'Boost multiplier to ball veloctiy (toggled with the B key) 

Sub BallControlTimer_Timer()
	If Contball and ContBallInPlay and EnableBallControl then
		If bcright = 1 Then
			ControlBall.velx = bcvel*bcboost
		ElseIf bcleft = 1 Then
			ControlBall.velx = - bcvel*bcboost
		Else
			ControlBall.velx=0
		End If

		If bcup = 1 Then
			ControlBall.vely = -bcvel*bcboost
		ElseIf bcdown = 1 Then
			ControlBall.vely = bcvel*bcboost
		Else
			ControlBall.vely= bcyveloffset
		End If
	End If
End Sub
'************************   End Ball Control 3/3

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 6 ' total number of balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub BallRollingUpdate_Timer()
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
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 and BOT(b).radius > 23 Then
			StopSound("plasticroll" & b)
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b))*RollingSoundFactor, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        ElseIf BallVel(BOT(b) ) > 1 AND BOT(b).z > 30 and BOT(b).radius > 23 Then
			StopSound("fx_ballrolling" & b)
            rolling(b) = True
            PlaySound("plasticroll" & b), -1, Vol(BOT(b))*RollingSoundFactor*2, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
		Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
				StopSound("plasticroll" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub


'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	batleftshadow.objRotZ = LeftFlipper.currentangle
	batrightshadow.objRotZ = RightFlipper.currentangle
	batrightuppershadow.objRotZ = RightUpperFlipper.currentangle
	pLeftFlipper.objrotz = LeftFlipper.CurrentAngle
	pRightFlipper.objrotz = RightFlipper.CurrentAngle
	pRightUpperFlipper.objrotz = RightUpperFlipper.CurrentAngle
End Sub

'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6)

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
        If BOT(b).Z > 20 and BOT(b).Z < 40 and BOT(b).radius > 23 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub

'************************************
' What you need to add to your table
'************************************

' a timer called RollingTimer. With a fast interval, like 10
' one collision sound, in this script is called fx_collide
' as many sound files as max number of balls, with names ending with 0, 1, 2, 3, etc
' for ex. as used in this script: fx_ballrolling0, fx_ballrolling1, fx_ballrolling2, fx_ballrolling3, etc


'******************************************
' Explanation of the rolling sound routine
'******************************************

' sounds are played based on the ball speed and position

' the routine checks first for deleted balls and stops the rolling sound.

' The For loop goes through all the balls on the table and checks for the ball speed and 
' if the ball is on the table (height lower than 30) then then it plays the sound
' otherwise the sound is stopped, like when the ball has stopped or is on a ramp or flying.

' The sound is played using the VOL, AUDIOPAN, AUDIOFADE and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the AUDIOPAN & AUDIOFADE functions will change the stereo position
' according to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.

Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall)*VolumeDial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Woods_Hit (idx)
	PlaySound "woodhit", 0, Vol(ActiveBall)*VolumeDial*2, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall)*VolumeDial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall)*VolumeDial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall)*VolumeDial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall)*VolumeDial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall)*VolumeDial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner", 0, .25*VolumeDial, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolumeDial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall)*VolumeDial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall)*VolumeDial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall)*VolumeDial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall)*VolumeDial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightUpperFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub


Sub DiverterFlipper_Collide(parm)
	PlaySound "metalhit2", 0, Vol(ActiveBall)*VolumeDial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall)*VolumeDial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall)*VolumeDial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall)*VolumeDial, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub


'================Light Handling==================
'		GI, Flashers, and Lamp handling
'Based on JP's VP10 fading Lamp routine, based on PD's Fading Lights
'		Mod FrameTime and GI handling by nFozzy
'================================================
'Short installation
'Keep all non-GI lamps/Flashers in a big collection called aLampsAll
'Initialize SolModCallbacks: Const UseVPMModSol = 1 at the top of the script, before LoadVPM. vpmInit me in table1_Init()
'LUT images (optional)
'Make modifications based on era of game (setlamp / flashc for games without solmodcallback, use bonus GI subs for games with only one GI control)

Dim LampState(340), FadingLevel(340), CollapseMe	
Dim FlashSpeedUp(340), FlashSpeedDown(340), FlashMin(340), FlashMax(340), FlashLevel(340)
Dim SolModValue(340)	'holds 0-255 modulated solenoid values

'These are used for fading lights and flashers brighter when the GI is darker
Dim LampsOpacity(340, 2) 'Columns: 0 = intensity / opacity, 1 = fadeup, 2 = FadeDown
Dim GIscale(4)	'5 gi strings

InitLamps

reDim CollapseMe(1)	'Setlamps and SolModCallBacks	(Click Me to Collapse)
	Sub SetLamp(nr, value)
		If value <> LampState(nr) Then
			LampState(nr) = abs(value)
			FadingLevel(nr) = abs(value) + 4
		End If
	End Sub

	Sub SetLampm(nr, nr2, value)	'set 2 lamps
		If value <> LampState(nr) Then
			LampState(nr) = abs(value)
			FadingLevel(nr) = abs(value) + 4
		End If
		If value <> LampState(nr2) Then
			LampState(nr2) = abs(value)
			FadingLevel(nr2) = abs(value) + 4
		End If
	End Sub

	Sub SetModLamp(nr, value)
		If value <> SolModValue(nr) Then
			SolModValue(nr) = value
			if value > 0 then LampState(nr) = 1 else LampState(nr) = 0
			FadingLevel(nr) = LampState(nr) + 4
		End If
	End Sub

	Sub SetModLampM(nr, nr2, value)	'set 2 modulated lamps
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
	'Flashers via SolModCallBacks
'	SolModCallBack(2) = "SetModLamp 102," 'Flasher2

'#end section
reDim CollapseMe(2) 'InitLamps 	(Click Me to Collapse)
	Sub InitLamps() 'set fading speeds and other stuff here		
		GetOpacity aLampsAll	'All non-GI lamps and flashers go in this object array for compensation script!
		Dim x
		for x = 0 to uBound(LampState)
			LampState(x) = 0	' current light state, independent of the fading level. 0 is off and 1 is on
			FadingLevel(x) = 4	' used to track the fading state
			FlashSpeedUp(x) = 0.1	'Fading speeds in opacity per MS I think (Not used with nFadeL or nFadeLM subs!)
			FlashSpeedDown(x) = 0.1
			
			FlashMin(x) = 0.001			' the minimum value when off, usually 0
			FlashMax(x) = 1				' the minimum value when off, usually 1
			FlashLevel(x) = 0.001       ' Raw Flasher opacity value. Start this >0 to avoid initial flasher stuttering.
			
			SolModValue(x) = 0			' Holds SolModCallback values
			
		Next
		
		for x = 0 to uBound(giscale)
			Giscale(x) = 1.625			' lamp GI compensation multiplier, eg opacity x 1.625 when gi is fully off
		next
			
		for x = 11 to 100 'insert fading levels (only applicable for lamps that use FlashC sub)
			FlashSpeedUp(x) = 0.015
			FlashSpeedDown(x) = 0.009
		Next		
		
		for x = 101 to 186	'Flasher fading speeds 'intensityscale(%) per 10MS
			FlashSpeedUp(x) = 1.1 * 4
			FlashSpeedDown(x) = 0.9 * 4
		next
		
		for x = 200 to 204		'GI relay on / off	fading speeds
			FlashSpeedUp(x) = 0.01
			FlashSpeedDown(x) = 0.008
			FlashMin(x) = 0
		Next
		for x = 300 to 304		'GI	8 step modulation fading speeds
			FlashSpeedUp(x) = 0.01
			FlashSpeedDown(x) = 0.008
			FlashMin(x) = 0
		Next

		UpdateGIon 0, 1:UpdateGIon 1, 1: UpdateGIon 2, 1 : UpdateGIon 3, 1:UpdateGIon 4, 1
		UpdateGI 0, 7:UpdateGI 1, 7:UpdateGI 2, 7 : UpdateGI 3, 7:UpdateGI 4, 7
	End Sub

	Sub GetOpacity(a)	'Keep lamp/flasher data in an array
		Dim x
		for x = 0 to (a.Count - 1)
			On Error Resume Next
			if a(x).Opacity > 0 then a(x).Uservalue = a(x).Opacity
			if a(x).Intensity > 0 then a(x).Uservalue = a(x).Intensity
			If a(x).FadeSpeedUp > 0 then LampsOpacity(x, 1) = a(x).FadeSpeedUp : LampsOpacity(x, 2) = a(x).FadeSpeedDown
		Next
		for x = 0 to (a.Count - 1) : LampsOpacity(x, 0) = a(x).UserValue : Next
	End Sub

	sub DebugLampsOn(input):Dim x: for x = 10 to 100 : setlamp x, input : next :  end sub

'#end section

reDim CollapseMe(3) 'LampTimer 	(Click Me to Collapse)
	LampTimer.Interval = -1	'-1 is ideal, but it will technically work with any timer interval
	LampTimer.Enabled = 1
	Dim FrameTime, InitFadeTime : FrameTime = 10	'Count Frametime
	Sub LampTimer_Timer()
		FrameTime = gametime - InitFadeTime
		Dim chgLamp, num, chg, ii
		chgLamp = Controller.ChangedLamps
		If Not IsEmpty(chgLamp) Then
			For ii = 0 To UBound(chgLamp)
				LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
				FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
			Next
		End If

		UpdateGIstuff
		UpdateLamps

		InitFadeTime = gametime
	End Sub
'#end section
reDim CollapseMe(4) 'ASSIGNMENTS: Lamps, GI, and Flashers (Click Me to Collapse)
	dim bulb

	Sub UpdateGIstuff()
		FadeGI 200
		ModGI  300	
		UpdateGIObjects 200, 300, GIs, 1	'nr, nr2, array 'Updates GI objects
		GiCompensation 200, 300, aLampsAll, GiScale(2)	'Averages two lamp strings. Ideal match the avg. Lut fading.
		FadeLUT 200, 300, "LutCont_", 27	'Lut averages three fading strings
	End Sub

	Sub UpdateLamps
	NFadeL 11, l11
	NFadeL 12, l12
	NFadeL 13, l13
	NFadeL 14, l14
	NFadeL 15, l15
	NFadeL 16, l16
	NFadeL 17, l17
	NFadeL 18, l18
	NFadeL 21, l21
	NFadeL 22, l22
	NFadeL 23, l23
	NFadeL 24, l24
	NFadeL 25, l25
	NFadeL 26, l26
	NFadeL 27, l28
	NFadeL 28, l27
	NFadeL 31, l31
	NFadeL 32, l32
	NFadeL 33, l33
	NFadeL 34, l34
	NFadeL 35, l35
	NFadeL 36, l36
	NFadeL 37, l37
	NFadeL 38, l38
	NFadeL 41, l41
	NFadeL 42, l42
	NFadeL 43, l43
	NFadeL 44, l44
	NFadeL 45, l45
	NFadeL 46, l46
	NFadeL 47, l47
	NFadeL 48, l48
	NFadeL 51, l51
	NFadeL 52, l52
	NFadeL 53, l53
	NFadeL 54, l54
	NFadeL 55, l55
	NFadeL 56, l56
	NFadeL 57, l57
	NFadeL 58, l58
	NFadeL 61, l61
	NFadeL 62, l62
	NFadeL 63, l63
	NFadeL 64, l64
	NFadeL 65, l65
	NFadeL 66, l66
	NFadeL 67, l67
	NFadeL 68, l68
	NFadeL 71, l71
	NFadeL 72, l72
	NFadeL 73, l73
	NFadeL 74, l74
	NFadeL 75, l75
	NFadeL 76, l76
	NFadeL 77, l77
	NFadeL 78, l78
	NFadeLm 81, l81a
	NFadeL 81, l81
	NFadeLm 82, l82a
	NFadeL 82, l82
	NFadeLm 83, l83a
	NFadeL 83, l83
	NBlendLm 84, bankLeftLamp
	FlashC 84, bankLeftFlasher
	NBlendLm 85, bankRightLamp 
	FlashC 85, bankRightFlasher
	NBlendLm 86, cellarLamp 
	FlashC 86, cellarFlasher	
	NBlendLm 87, roofLamp 
	FlashC 87, roofFlasher

	' board game lamps
	Flashc 91, l91
	Flashc 92, l92
	Flashc 93, l93
	Flashc 94, l94
	Flashc 95, l95
	Flashc 96, l96
	Flashc 97, l97
	Flashc 98, l98
	Flashc 101, l101
	Flashc 102, l102
	Flashc 103, l103
	Flashc 104, l104
	Flashc 105, l105
	Flashc 106, l106
	Flashc 107, l107
	Flashc 108, l108
	Flashc 111, l111
	Flashc 112, l112
	Flashc 113, l113
	Flashc 114, l114
	Flashc 115, l115
	Flashc 116, l116
	Flashc 117, l117
	Flashc 118, l118
	Flashc 121, l121
	Flashc 122, l122
	Flashc 123, l123
	Flashc 124, l124
	Flashc 125, l125
	Flashc 126, l126
	Flashc 127, l127
	Flashc 128, l128
	Flashc 131, l131
	Flashc 132, l132
	Flashc 133, l133
	Flashc 134, l134
	Flashc 135, l135
	Flashc 136, l136
	Flashc 137, l137
	Flashc 138, l138
	Flashc 141, l141
	Flashc 142, l142
	Flashc 143, l143
	Flashc 144, l144
	Flashc 145, l145
	Flashc 146, l146
	Flashc 147, l147
	Flashc 148, l148
	end sub
	
'#end section


reDim CollapseMe(5) 'Combined GI subs / functions (Click Me to Collapse)
	Set GICallback = GetRef("UpdateGIon")		'On/Off GI to NRs 200-203
	Sub UpdateGIOn(no, Enabled) : Setlamp no+200, cInt(enabled) : End Sub	


	Set GICallback2 = GetRef("UpdateGI")
	Sub UpdateGI(no, step)						'8 step Modulated GI to NRs 300-303
		Dim ii, x', i		
		
		If step = 0 then exit sub 'only values from 1 to 8 are visible and reliable. 0 is not reliable and 7 & 8 are the same so...

		SetModLamp no+300, ScaleGI(step, 0)
		LampState((no+300)) = 0
	End Sub

	Function ScaleGI(value, scaletype)	'returns an intensityscale-friendly 0->100% value out of 1>8 'it does go to 8
		Dim i
		Select Case scaletype	'select case because bad at maths 
			case 0  : i = value * (1/8)	'0 to 1
			case 25 : i = (1/28)*(3*value + 4)
			case 50 : i = (value+5)/12
			case else : i = value * (1/8)	'0 to 1
	'			x = (4*value)/3 - 85	'63.75 to 255
		End Select
		ScaleGI = i
	End Function

'	Dim LSstate : LSstate = False	'fading sub handles SFX 'Uncomment to enable
	Sub FadeGI(nr) 'in On/off		'Updates nothing but flashlevel
		Select Case FadingLevel(nr)
			Case 3 : FadingLevel(nr) = 0
			Case 4 'off
				DOF 101, DOFOff
	'			If Not LSstate then Playsound "FX_Relay_Off",0,LVL(0.1) : LSstate = True	'handle SFX
				FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * FrameTime)
				If FlashLevel(nr) < FlashMin(nr) Then
				   FlashLevel(nr) = FlashMin(nr)
				   FadingLevel(nr) = 3 'completely off
	'				LSstate = False
				End if
			Case 5 ' on
				DOF 101, DOFOn
	'			If Not LSstate then Playsound "FX_Relay_On",0,LVL(0.1) : LSstate = True	'handle SFX
				FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr) * FrameTime)
				If FlashLevel(nr) > FlashMax(nr) Then
					FlashLevel(nr) = FlashMax(nr)
					FadingLevel(nr) = 6 'completely on
	'				LSstate = False
				End if
			Case 6 : FadingLevel(nr) = 1
		End Select
	End Sub
	Sub ModGI(nr2) 'in 0->1		'Updates nothing but flashlevel	'never off
		Dim DesiredFading
		Select Case FadingLevel(nr2)
			case 3 : FadingLevel(nr2) = 0	'workaround - wait a frame to let M sub finish fading 
	'		Case 4 : FadingLevel(nr2) = 3	'off -disabled off, only gicallback1 can turn off GI(?) 'experimental
			Case 5, 4 ' Fade (Dynamic)
				DesiredFading = SolModValue(nr2)
				if FlashLevel(nr2) < DesiredFading Then '+
					FlashLevel(nr2) = FlashLevel(nr2) + (FlashSpeedUp(nr2)	* FrameTime	)
					If FlashLevel(nr2) >= DesiredFading Then FlashLevel(nr2) = DesiredFading : FadingLevel(nr2) = 1
				elseif FlashLevel(nr2) > DesiredFading Then '-
					FlashLevel(nr2) = FlashLevel(nr2) - (FlashSpeedDown(nr2) * FrameTime	)
					If FlashLevel(nr2) <= DesiredFading Then FlashLevel(nr2) = DesiredFading : FadingLevel(nr2) = 6
				End If
			Case 6
				FadingLevel(nr2) = 1
		End Select
	End Sub

	Sub UpdateGIobjects(nr, nr2, a, var)	'Just Update GI
		If FadingLevel(nr) > 1 or FadingLevel(nr2) > 1 Then
			Dim x, Output : Output = FlashLevel(nr2) * FlashLevel(nr)
			for each x in a : x.IntensityScale = Output : next
		End If
	end Sub
	
	Sub GiCompensation(nr, nr2, a, GIscaleOff)	'One NR pairing only fading
	'	tbgi.text = "GI: " & SolModValue(nr) & " " & FlashLevel(nr) & " " & FadingLevel(nr) & vbnewline & _
	'				"ModGI: " & SolModValue(nr2) & " " & FlashLevel(nr2) & " " & FadingLevel(nr2) & vbnewline & _
	'				"Solmodvalue, Flashlevel, Fading step"
		if FadingLevel(nr) > 1 or FadingLevel(nr2) > 1 Then
			Dim x, Giscaler, Output : Output = FlashLevel(nr2) * FlashLevel(nr)
			Giscaler = ((Giscaleoff-1) * (ABS(Output-1) )  ) + 1	'fade GIscale the opposite direction

			for x = 0 to (a.Count - 1) 'Handle Compensate Flashers
				On Error Resume Next
				a(x).Opacity = LampsOpacity(x, 0) * Giscaler
				a(x).Intensity = LampsOpacity(x, 0) * Giscaler
				a(x).FadeSpeedUp = LampsOpacity(x, 1) * Giscaler
				a(x).FadeSpeedDown = LampsOpacity(x, 2) * Giscaler
			Next
			'		tbbb.text = giscaler & " on:" & FadingLevel(nr) & vbnewline & "flash: " & output & " onmod:" & FadingLevel(nr2) & vbnewline & l37.intensity
			'		tbbb1.text = FadingLevel(nr) & vbnewline & FadingLevel(nr2)
			'	tbgi1.text = Output & " giscale:" & giscaler	'debug
		End If
		'		tbbb1.text = FLashLevel(nr) & vbnewline & FlashLevel(nr2)
	End Sub
	
	Sub GiCompensationAvg(nr, nr2, nr3, nr4, a, GIscaleOff)	'Two pairs of NRs averaged together
	'	tbgi.text = "GI: " & SolModValue(nr) & " " & FlashLevel(nr) & " " & FadingLevel(nr) & vbnewline & _
	'				"ModGI: " & SolModValue(nr2) & " " & FlashLevel(nr2) & " " & FadingLevel(nr2) & vbnewline & _
	'				"Solmodvalue, Flashlevel, Fading step"
		if FadingLevel(nr) > 1 or FadingLevel(nr2) > 1 or FadingLevel(nr3) > 1 or FadingLevel(nr4) > 1 Then
			Dim x, Giscaler, Output : Output = (((FlashLevel(nr)*FlashLevel(nr2)) + (FlashLevel(nr3)*Flashlevel(nr4))) /2)
			Giscaler = ((Giscaleoff-1) * (ABS(Output-1) )  ) + 1	'fade GIscale the opposite direction

			for x = 0 to (a.Count - 1) 'Handle Compensate Flashers
				On Error Resume Next
				a(x).Opacity = LampsOpacity(x, 0) * Giscaler
				a(x).Intensity = LampsOpacity(x, 0) * Giscaler
				a(x).FadeSpeedUp = LampsOpacity(x, 1) * Giscaler
				a(x).FadeSpeedDown = LampsOpacity(x, 2) * Giscaler
			Next
		
				
		REM tbgi1.text = "Output:" & output & vbnewline & _
					REM "GIscaler" & giscaler & vbnewline & _
					REM "..."
		End If
		REM tbgi.text = "GI0 " & flashlevel(200) & " " & flashlevel(300) & vbnewline & _
					REM "GI1 " & flashlevel(201) & " " & flashlevel(301) & vbnewline & _
					REM "GI2 " & flashlevel(202) & " " & flashlevel(302) & vbnewline & _
					REM "GI3 " & flashlevel(203) & " " & flashlevel(303) & vbnewline & _
					REM "GI4 " & flashlevel(204) & " " & flashlevel(304) & vbnewline & _
					REM "..."
	End Sub	
	
	Sub GiCompensationAvgM(nr, nr2, nr3, nr4, nr5, nr6, a, GIscaleOff)	'Three pairs of NRs averaged together
	'	tbgi.text = "GI: " & SolModValue(nr) & " " & FlashLevel(nr) & " " & FadingLevel(nr) & vbnewline & _
	'				"ModGI: " & SolModValue(nr2) & " " & FlashLevel(nr2) & " " & FadingLevel(nr2) & vbnewline & _
	'				"Solmodvalue, Flashlevel, Fading step"
		if FadingLevel(nr) > 1 or FadingLevel(nr2) > 1 Then
			Dim x, Giscaler, Output
			Output = (((FlashLevel(nr)*FlashLevel(nr2)) + (FlashLevel(nr3)*Flashlevel(nr4)) + (FlashLevel(nr5)*FlashLevel(nr6)))/3)
			
			Giscaler = ((Giscaleoff-1) * (ABS(Output-1) )  ) + 1	'fade GIscale the opposite direction

			for x = 0 to (a.Count - 1) 'Handle Compensate Flashers
				On Error Resume Next
				a(x).Opacity = LampsOpacity(x, 0) * Giscaler
				a(x).Intensity = LampsOpacity(x, 0) * Giscaler
				a(x).FadeSpeedUp = LampsOpacity(x, 1) * Giscaler
				a(x).FadeSpeedDown = LampsOpacity(x, 2) * Giscaler
			Next
			'		tbbb.text = giscaler & " on:" & FadingLevel(nr) & vbnewline & "flash: " & output & " onmod:" & FadingLevel(nr2) & vbnewline & l37.intensity
			'		tbbb1.text = FadingLevel(nr) & vbnewline & FadingLevel(nr2)
			'	tbgi1.text = Output & " giscale:" & giscaler	'debug
		End If
		'		tbbb1.text = FLashLevel(nr) & vbnewline & FlashLevel(nr2)
	End Sub	

	Sub FadeLUT(nr, nr2, LutName, LutCount)	'fade lookuptable NOTE- this is a bad idea for darkening your table as
		If FadingLevel(nr) >2 or FadingLevel(nr2) > 2 Then				'-it will strip the whites out of your image
			Dim GoLut
			GoLut = cInt(LutCount * (FlashLevel(nr)*FlashLevel(nr2)	)	)
			Table1.ColorGradeImage = LutName & GoLut
	'		tbgi2.text = Table1.ColorGradeImage & vbnewline & golut	'debug
		End If
	End Sub
	
	Sub FadeLUTavg(nr, nr2, nr3, nr4, LutName, LutCount)	'FadeLut for two GI strings (WPC)
		If FadingLevel(nr) >2 or FadingLevel(nr2) > 2 or FadingLevel(nr3) >2 or FadingLevel(nr4) > 2 Then
			Dim GoLut
			GoLut = cInt(LutCount * (((FlashLevel(nr)*FlashLevel(nr2)) + (FlashLevel(nr3)*Flashlevel(nr4))) /2)	)
			Table1.ColorGradeImage = LutName & GoLut
			REM tbgi2.text = Table1.ColorGradeImage & vbnewline & golut	'debug
		End If
	End Sub
	
	Sub FadeLUTavgM(nr, nr2, nr3, nr4, nr5, nr6, LutName, LutCount)	'FadeLut for three GI strings (WPC)
		If FadingLevel(nr) >2 or FadingLevel(nr2) > 2 or FadingLevel(nr3) >2 or FadingLevel(nr4) > 2 or _ 
		FadingLevel(nr5) >2 or FadingLevel(nr6) > 2 Then
			Dim GoLut
			GoLut = cInt(LutCount * (((FlashLevel(nr)*FlashLevel(nr2)) + (FlashLevel(nr3)*Flashlevel(nr4)) + (FlashLevel(nr5)*FlashLevel(nr6)))/3)	)	'what a mess
			Table1.ColorGradeImage = LutName & GoLut
	'		tbgi2.text = Table1.ColorGradeImage & vbnewline & golut	'debug
		End If
	End Sub
	
'#end section

reDim CollapseMe(6) 'Fading subs 	 (Click Me to Collapse)
	Sub nModFlash(nr, object, scaletype, offscale)	'Fading with modulated callbacks
		Dim DesiredFading
		Select Case FadingLevel(nr)
			case 3 : FadingLevel(nr) = 0	'workaround - wait a frame to let M sub finish fading 
			Case 4	'off
				If Offscale = 0 then Offscale = 1
				FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * FrameTime	) * offscale
				If FlashLevel(nr) < 0 then FlashLevel(nr) = 0 : FadingLevel(nr) = 3
				Object.IntensityScale = ScaleLights(FlashLevel(nr),0 )
			Case 5 ' Fade (Dynamic)
				DesiredFading = ScaleByte(SolModValue(nr), scaletype)
				if FlashLevel(nr) < DesiredFading Then '+
					FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr)	* FrameTime	)
					If FlashLevel(nr) >= DesiredFading Then FlashLevel(nr) = DesiredFading : FadingLevel(nr) = 1
				elseif FlashLevel(nr) > DesiredFading Then '-
					FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * FrameTime	)
					If FlashLevel(nr) <= DesiredFading Then FlashLevel(nr) = DesiredFading : FadingLevel(nr) = 6
				End If
				Object.Intensityscale = ScaleLights(FlashLevel(nr),0 )
			Case 6 : FadingLevel(nr) = 1
		End Select
	End Sub

	Sub nModFlashM(nr, Object)
		Select Case FadingLevel(nr)
			Case 3, 4, 5, 6 : Object.Intensityscale = ScaleLights(FlashLevel(nr),0 )
		End Select
	End Sub

	Sub Flashc(nr, object)	'FrameTime Compensated. Can work with Light Objects (make sure state is 1 though)
		Select Case FadingLevel(nr)
			Case 3 : FadingLevel(nr) = 0
			Case 4 'off
				FlashLevel(nr) = FlashLevel(nr) - (FlashSpeedDown(nr) * FrameTime)
				If FlashLevel(nr) < FlashMin(nr) Then
					FlashLevel(nr) = FlashMin(nr)
				   FadingLevel(nr) = 3 'completely off
				End if
				Object.IntensityScale = FlashLevel(nr)
			Case 5 ' on
				FlashLevel(nr) = FlashLevel(nr) + (FlashSpeedUp(nr) * FrameTime)
				If FlashLevel(nr) > FlashMax(nr) Then
					FlashLevel(nr) = FlashMax(nr)
					FadingLevel(nr) = 6 'completely on
				End if
				Object.IntensityScale = FlashLevel(nr)
			Case 6 : FadingLevel(nr) = 1
		End Select
	End Sub

	Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
		select case FadingLevel(nr)
			case 3, 4, 5, 6 : Object.IntensityScale = FlashLevel(nr)
		end select
	End Sub
	
	Sub NFadeL(nr, object)	'Simple VPX light fading using State
    Select Case FadingLevel(nr)
        Case 3:object.state = 0:FadingLevel(nr) = 0
        Case 4:object.state = 0:FadingLevel(nr) = 3
        Case 5:object.state = 1:FadingLevel(nr) = 6
        Case 6:object.state = 1:FadingLevel(nr) = 1
    End Select
	End Sub

	Sub NFadeLm(nr, object) ' used for multiple lights
		Select Case FadingLevel(nr)
			Case 3:object.state = 0
			Case 4:object.state = 0
			Case 5:object.state = 1
			Case 6:object.state = 1
		End Select
	End Sub

	Sub NBlendLm(nr, object) ' used for multiple lights
		Select Case FadingLevel(nr)
			Case 3:object.BlendDisableLighting = 0
			Case 4:object.BlendDisableLighting = 0.1
			Case 5:object.BlendDisableLighting = 0.8
			Case 6:object.BlendDisableLighting = 0.4
		End Select
	End Sub


'#End Section

reDim CollapseMe(7) 'Fading Functions (Click Me to Collapse)
	Function ScaleLights(value, scaletype)	'returns an intensityscale-friendly 0->100% value out of 255
		Dim i
		Select Case scaletype	'select case because bad at maths 	'TODO: Simplify these functions. B/c this is absurdly bad.
			case 0  : i = value * (1 / 255)	'0 to 1
			case 6  : i = (value + 17)/272  '0.0625 to 1 
			case 9  : i = (value + 25)/280  '0.089 to 1
			case 15 : i = (value / 300) + 0.15
			case 20 : i = (4 * value)/1275 + (1/5)
			case 25 : i = (value + 85) / 340
			case 37 : i = (value+153) / 408 	'0.375 to 1
			case 40 : i = (value + 170) / 425
			case 50 : i = (value + 255) / 510	'0.5 to 1
			case 75 : i = (value + 765) / 1020	'0.75 to 1
			case Else : i = 10
		End Select
		ScaleLights = i
	End Function

	Function ScaleByte(value, scaletype)	'returns a number between 1 and 255
		Dim i
		Select Case scaletype
			case 0 : i = value * 1	'0 to 1
			case 9 : i = (5*(200*value + 1887))/1037 'ugh 
			case 15 : i = (16*value)/17 + 15
			Case 63 : i = (3*(value + 85))/4
			case else : i = value * 1	'0 to 1
		End Select
		ScaleByte = i
	End Function

Sub theend() : End Sub

Function BallSpeed()
	On Error Resume Next
	BallSpeed = SQR(ActiveBall.VelX ^ 2 + ActiveBall.VelY ^ 2)
End Function



REM Troubleshooting : 
REM Flashers/gi are intermittent or aren't showing up
REM Ensure flashers start visible, light objects start with state = 1

REM No lamps or no GI
REM Make sure these constants are set up this way
REM Const UseSolenoids = 1
REM Const UseLamps = 0
REM Const UseGI = 1

REM SolModCallback error
REM Ensure you have the latest scripts. Clear out any loose scripts in your tables that might be causing conflicts.

REM Table1 Error
REM Rename the table to Table1 or find/Replace table1 with whatever the table's name is

REM SolModCallbacks aren't sending anything
REM Two important things to get SolModCallbacks to initialize properly:
REM Put this at the top of the script, before LoadVPM
REM Const UseVPMModSol = 1
REM Put this in the table1_Init() section
REM vpmInit me

SolModCallback(17) = "Flasherset17"
SolModCallback(18) = "Flasherset18"
SolModCallback(19) = "Flasherset19"
SolModCallback(20) = "Flasherset20"
SolModCallback(21) = "Flasherset21"
SolModCallback(22) = "Flasherset22"

Dim FlashLevel17, FlashLevel18, FlashLevel18a, FlashLevel19, FlashLevel20, FlashLevel21, FlashLevel22

Sub Flasherset17(value)
	If value < 160 Then value = 160
	If value > Flashlevel17 * 255 Then 
		FlashLevel17 = value / 255
		If not Flasherflash17.TimerEnabled Then 
			Flasherflash17.TimerEnabled = True
			Flasherflash17.visible = 1
			Flasherlit17.visible = 1
			FlasherLight17.state = 1
		End If
		FlasherFlash17_Timer
	End If
 End Sub

Sub Flasherset18(value)
	FlashLevel18a = value
	If value < 160 Then value = 160
	If value > Flashlevel18 * 255 Then 
		FlashLevel18 = value / 255
		If not Flasherflash18.TimerEnabled Then 
			Flasherflash18.TimerEnabled = True
			Flasherflash18.visible = 1
			Flasherlit18.visible = 1
			FlasherLight18.state = 1
			lF18.state = 1
		End If
		FlasherFlash18_Timer
	End If
 End Sub

Sub Flasherset19(value)
	If value < 160 Then value = 160
	If value > Flashlevel19 * 255 Then 
		FlashLevel19 = value / 255
		If not Flasherflash19.TimerEnabled Then 
			Flasherflash19.TimerEnabled = True
			Flasherflash19.visible = 1
			Flasherlit19.visible = 1
			FlasherLight19.state = 1
		End If
		FlasherFlash19_Timer
	End If
 End Sub

Sub Flasherset20(value)
	If value < 160 Then value = 160
	If value > Flashlevel20 * 255 Then 
		FlashLevel20 = value / 255
		If not Flasherflash20.TimerEnabled Then 
			Flasherflash20.TimerEnabled = True
			Flasherflash20.visible = 1
			Flasherlit20.visible = 1
			FlasherLight20.state = 1
		End If
		FlasherFlash20_Timer
	End If
 End Sub

Sub Flasherset21(value)
	If value < 160 Then value = 160
	If value > Flashlevel21 * 255 Then 
		FlashLevel21 = value / 255
		If not Flasherflash21.TimerEnabled Then 
			Flasherflash21.TimerEnabled = True
			Flasherflash21.visible = 1
			Flasherlit21.visible = 1
			FlasherLight21.state = 1
		End If
		FlasherFlash21_Timer
	End If
 End Sub

Sub Flasherset22(value) 
	If value < 160 Then value = 160
	If value > Flashlevel22 * 255 Then 
		FlashLevel22 = value / 255
		If not Flasherflash22.TimerEnabled Then 
			Flasherflash22.TimerEnabled = True
			Flasherflash22.visible = 1
			Flasherlit22.visible = 1
			FlasherLight22.state = 1
		End If
		FlasherFlash22_Timer
	End If
End Sub

Sub FlasherFlash17_Timer()
	dim flashx3, matdim
	flashx3 = FlashLevel17^3
	Flasherflash17.opacity = 1200 * flashx3^0.8
	Flasherlit17.BlendDisableLighting = 4 * FlashLevel17
	Flasherbase17.BlendDisableLighting =  0.1 + flashx3
	Flasherlight17.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel17)
	Flasherlit17.material = "domelit" & matdim
	FlashLevel17 = FlashLevel17 * 0.9 - 0.01
	If FlashLevel17 < 0 Then 
		Flasherflash17.TimerEnabled = False
		Flasherflash17.visible = 0
		Flasherlit17.visible = 0
		FlasherLight17.state = 0
	End If
End Sub

Sub FlasherFlash18_Timer()
	dim flashx3, matdim
	flashx3 = FlashLevel18^3
	Flasherflash18.opacity = 1200 * flashx3^0.8
	Flasherlit18.BlendDisableLighting = 4 * FlashLevel18
	Flasherbase18.BlendDisableLighting =  0.1 + flashx3
	Flasherlight18.IntensityScale = flashx3
	lF18.IntensityScale = ScaleLights(FlashLevel18a, 0)
	matdim = Round(10 * FlashLevel18)
	Flasherlit18.material = "domelit" & matdim
	FlashLevel18 = FlashLevel18 * 0.9 - 0.01
	If FlashLevel18 < 0 Then 
		Flasherflash18.TimerEnabled = False
		Flasherflash18.visible = 0
		Flasherlit18.visible = 0
		FlasherLight18.state = 0
		lF18.state = 0
	End If
End Sub

Sub FlasherFlash19_Timer()
	dim flashx3, matdim
	flashx3 = FlashLevel19^3
	Flasherflash19.opacity = 1000 * flashx3^0.8
	Flasherlit19.BlendDisableLighting = 4 * FlashLevel19
	Flasherbase19.BlendDisableLighting =  0.1 + flashx3
	Flasherlight19.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel19)
	Flasherlit19.material = "domelit" & matdim
	FlashLevel19 = FlashLevel19 * 0.9 - 0.01
	If FlashLevel19 < 0 Then 
		Flasherflash19.TimerEnabled = False
		Flasherflash19.visible = 0
		Flasherlit19.visible = 0
		FlasherLight19.state = 0
	End If
End Sub

Sub FlasherFlash20_Timer()
	dim flashx3, matdim
	flashx3 = FlashLevel20 * FlashLevel20 * FlashLevel20
	Flasherflash20.opacity = 1000 * flashx3^0.8
	Flasherlit20.BlendDisableLighting = 4 * FlashLevel20
	Flasherbase20.BlendDisableLighting =  0.1 + flashx3
	Flasherlight20.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel20)
	Flasherlit20.material = "domelit" & matdim
	FlashLevel20 = FlashLevel20 * 0.9 - 0.01
	If FlashLevel20 < 0 Then 
		Flasherflash20.TimerEnabled = False
		Flasherflash20.visible = 0
		Flasherlit20.visible = 0
		FlasherLight20.state = 0
	End If
End Sub

Sub FlasherFlash21_Timer()
	dim flashx3, matdim
	flashx3 = FlashLevel21 * FlashLevel21 * FlashLevel21
	Flasherflash21.opacity = 800 * flashx3^0.8
	Flasherlit21.BlendDisableLighting = 4 * FlashLevel21
	Flasherbase21.BlendDisableLighting =  0.1 + flashx3
	Flasherlight21.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel21)
	Flasherlit21.material = "domelit" & matdim
	FlashLevel21 = FlashLevel21 * 0.9 - 0.01
	If FlashLevel21 < 0 Then 
		Flasherflash21.TimerEnabled = False
		Flasherflash21.visible = 0
		Flasherlit21.visible = 0
		FlasherLight21.state = 0
	End If
End Sub

Sub FlasherFlash22_Timer()
	dim flashx3, matdim
	flashx3 = FlashLevel22 * FlashLevel22 * FlashLevel22
	Flasherflash22.opacity = 1000 * flashx3^0.8
	Flasherlit22.BlendDisableLighting = 4 * FlashLevel22
	Flasherbase22.BlendDisableLighting =  0.1 + flashx3
	Flasherlight22.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel22)
	Flasherlit22.material = "domelit" & matdim
	FlashLevel22 = FlashLevel22 * 0.9 - 0.01
	If FlashLevel22 < 0 Then 
		Flasherflash22.TimerEnabled = False
		Flasherflash22.visible = 0
		Flasherlit22.visible = 0
		FlasherLight22.state = 0
	End If
End Sub



' ***************************************************************************
'                    BASIC FSS(DMD,SS,EM) SETUP CODE
' ****************************************************************************

const USEEM = 0
const USESS = 0
Dim xoff,yoff,zoff,xrot,zscale, xcen,ycen

sub setup_backglass()

	xoff =415
	yoff =0
	zoff =570
	xrot = -90

	If Not Table1.ShowFSS and Table1.ShowDT Then
		xoff = xoff + 1000
		yoff = yoff + 300 
		zoff = zoff - 700
		xrot = -90
		Flasher3.visible = 0
		Flasher5.visible = 0
		Flasher4.visible = 0
		Flasher6.visible = 0
		Flasher7.visible = 0
		Flasher10.visible = 0
		Flasher11.visible = 0
		Flasher13.visible = 0
		Flasher15.visible = 0
		Flasher16.visible = 0
		Flasher17.visible = 0
		Flasher19.visible = 0

		scbgframe.visible = 0
		scbgframe2.visible = 1

		DMD1.opacity = 0
		scbgFrameGlow.visible = 0
	
		Flasher20.visible = 0
		Flasher21.visible = 0
		Flasher22.visible = 0
		Flasher23.visible = 0
		FlMirror.visible = 0

	Elseif Table1.ShowFSS Then
		scbgframe2.visible = false
	Else 
		zoff = zoff + 500
		DMD1.opacity = 0
		scbgFrameGlow.visible = 0

		Flasher20.visible = 0
		Flasher21.visible = 0
		Flasher22.visible = 0
		Flasher23.visible = 0
		FlMirror.visible = 0
	End If

	scbghigh.x = xoff
	scbghigh.y = yoff
	scbghigh.height = zoff
	scbghigh.rotx = xrot

	scbghighsol23.x = xoff
	scbghighsol23.y = yoff
	scbghighsol23.height = zoff
	scbghighsol23.rotx = xrot
	scbghighsol23.visible = 0

	scbghighsol24.x = xoff
	scbghighsol24.y = yoff
	scbghighsol24.height = zoff
	scbghighsol24.rotx = xrot
	scbghighsol24.visible = 0

	scbghigh1.x = xoff
	scbghigh1.y = yoff
	scbghigh1.height = zoff + 200
	scbghigh1.rotx = xrot

	scbghigh2.x = xoff
	scbghigh2.y = yoff
	scbghigh2.height = zoff
	scbghigh2.rotx = xrot

	scbgframe.x = xoff
	scbgframe.y = yoff 
	scbgframe.height = zoff
	scbgframe.rotx = xrot

	scbgframe2.x = xoff + 4
	scbgframe2.y = yoff 
	scbgframe2.height = zoff + 4
	scbgframe2.rotx = xrot

	DMD.x = xoff + 20
	DMD.y = yoff
	DMD.height = zoff - 45
	DMD.rotx = xrot

	scbgFrameGlow.x = xoff
	scbgFrameGlow.y = yoff 
	scbgFrameGlow.height = zoff + 900
	scbgFrameGlow.rotx = xrot

	'DMD mirror
	DMD1.x = xoff
	DMD1.y = yoff + 600
	DMD1.height = zoff - 460
	DMD1.rotx = xrot +180
	DMD1.visible =1

	center_graphix()

end sub


Dim BGArr 
BGArr=Array (l1, l2, l3, l4, l5, l6, l7, l8, l9,l10, l19,l20,l29, l91, l92, l93, l94, l95, l96, l97, l98, l101, l102, l103, l104, l105, l106, l107, l108,_
l111, l112, l113, l114, l115, l116, l117, l118, l121, l122, l123, l124, l125, l126, l127,_
l128, l131, l132, l133, l134, l135, l136, l137, l138, l141, l142, l143, l144, l145, l146, l147, l148,_
Flasher6,Flasher7,Flasher4,Flasher5,Flasher10,Flasher11,Flasher3,Flasher13,Flasher19,Flasher15,Flasher16,Flasher17,Flasher18)


Sub center_graphix()
	Dim xx,yy,yfact,xfact,xobj
	zscale = 0.0000001

	xcen =(888 /2) - (33 / 2)
	ycen = (793 /2 ) + (236 /2)


	yfact =140 'y fudge factor (ycen was wrong so fix)
	xfact =15

	For Each xobj In BGArr
		xx =xobj.x 
		
		xobj.x = (xoff -xcen) + xx +xfact
		yy = xobj.y ' get the yoffset before it is changed
		xobj.y =yoff 

		If(yy < 0.) then
			yy = yy * -1
		end if

	
		xobj.height =( zoff - ycen) + yy - (yy * zscale) + yfact
	
		xobj.rotx = xrot
		'xobj.visible =1 ' for testing
	Next
end sub

Sub SolFlasherStripSeq1(enabled)
	if enabled then
		scbghighsol23.visible = 1
		if Table1.ShowFSS Then FlMirrorSol23.visible = 1
	Else
		scbghighsol23.visible = 0
		FlMirrorSol23.visible = 0
	end If
end Sub

Sub SolFlasherStripSeq2(enabled)
	if enabled then
		scbghighsol24.visible = 1
		if Table1.ShowFSS Then FlMirrorSol24.visible = 1
	Else
		scbghighsol24.visible = 0
		FlMirrorSol24.visible = 0
	end If
end Sub

Dim nxx, DNS
DNS = Table1.NightDay
'Dim OPSValues: OPSValues=Array (100,50,20,10 ,5,4 ,3 ,2 ,1, 0,0)
'Dim DNSValues: DNSValues=Array (1.0,0.5,0.1,0.05,0.01,0.005,0.001,0.0005,0.0001, 0.00005,0.00000)
Dim OPSValues: OPSValues=Array (100,50,20,10 ,9,8 ,7 ,6 ,5, 4,3,2,1,0)
Dim DNSValues: DNSValues=Array (1.0,0.5,0.1,0.05,0.01,0.005,0.001,0.0005,0.0001, 0.00005,0.00001, 0.000005, 0.000001)
Dim SysDNSVal: SysDNSVal=Array (1.0,0.9,0.8,0.7,0.6,0.5,0.5,0.5,0.5, 0.5,0.5)
Dim DivValues: DivValues =Array (1,2,4,8,16,32,32,32,32, 32,32)
Dim DivValues2: DivValues2 =Array (1,1.5,2,2.5,3,3.5,4,4.5,5, 5.5,6)
Dim DivValues3: DivValues3 =Array (1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.1)
Dim RValUP: RValUP=Array (30,60,90,120,150,180,210,240,255,255,255,255)
Dim GValUP: GValUP=Array (30,60,90,120,150,180,210,240,255,255,255,255)
Dim BValUP: BValUP=Array (30,60,90,120,150,180,210,240,255,255,255,255)
Dim RValDN: RValDN=Array (255,210,180,150,120,90,60,30,10,10,10)
Dim GValDN: GValDN=Array (255,210,180,150,120,90,60,30,10,10,10)
Dim BValDN: BValDN=Array (255,210,180,150,120,90,60,30,10,10,10)
Dim FValUP: FValUP=Array (35,40,45,50,55,60,65,70,75,80,85,90,95,100,105)
Dim FValDN: FValDN=Array (100,85,80,75,70,65,60,55,50,45,40,35,30)
Dim MVSAdd: MVSAdd=Array (0.9,0.9,0.8,0.8,0.7,0.7,0.6,0.6,0.5,0.5,0.4,0.3,0.2,0.1)
Dim ReflDN: ReflDN=Array (60,55,50,45,40,35,30,28,26,24,22,20,19,18,16,15,14,13,12,11,10)
Dim DarkUP: DarkUP=Array (1,2,3,4,5,6,6,6,6,6,6,6,6,6,6,6,6,6)

Dim DivLevel: DivLevel = 35
Dim DNSVal: DNSVal = Round(DNS/10)
Dim DNShift: DNShift = 1

' PLAYFIELD GLOBAL INTENSITY ILLUMINATION FLASHERS
Flasher22.opacity = OPSValues(DNSVal + DNShift) / DivValues(DNSVal)
Flasher22.intensityscale = DNSValues(DNSVal + DNShift) /DivValues(DNSVal)
Flasher23.opacity = OPSValues(DNSVal + DNShift) /DivValues(DNSVal)
Flasher23.intensityscale = DNSValues(DNSVal + DNShift) /DivValues(DNSVal)
Flasher20.opacity = OPSValues(DNSVal + DNShift) /DivValues(DNSVal)
Flasher20.intensityscale = DNSValues(DNSVal + DNShift) /DivValues(DNSVal)

'BACKBOX & BACKGLASS ILLUMINATION
scbgframe.ModulateVsAdd = MVSAdd(DNSVal)
scbgframe.Color = RGB(RValUP(DNSVal),GValUP(DNSVal),BValUP(DNSVal))
scbgframe.Amount = FValUP(DNSVal) / DivValues(DNSVal)

scbgframe2.ModulateVsAdd = MVSAdd(DNSVal)
scbgframe2.Color = RGB(RValUP(DNSVal),GValUP(DNSVal),BValUP(DNSVal))
scbgframe2.Amount = FValUP(DNSVal) / DivValues(DNSVal)

scbghigh.Color = RGB(RValDN(DNSVal),GValDN(DNSVal),BValDN(DNSVal))
scbghigh.Amount = FValDN(DNSVal)  / DivValues(DNSVal)
scbghigh1.Color = RGB(RValDN(DNSVal),GValDN(DNSVal),BValDN(DNSVal))
scbghigh1.Amount = FValDN(DNSVal) / DivValues(DNSVal)

scbghigh.intensityscale = 0.8
scbghigh1.intensityscale = 0.8
scbgframe.intensityscale = 0.4
scbgframe2.intensityscale = 0.4
scbghigh2.intensityscale = 1.7
scbghighsol23.intensityscale = 4.0
scbghighsol24.intensityscale = 4.0


'**************************************************************************************
'               [CC Color Correction] Textures must be CC corrected to work
'**************************************************************************************

Dim DivGlobal: DivGlobal = 0.6
Dim DivClrCor: DivClrCor = 0.6

Dim BlueDiv:BlueDiv  = 1.0* DivGlobal
Dim RedDiv:RedDiv  = 0.9* DivGlobal
Dim RedGreenDiv:RedGreenDiv  = 1.0* DivGlobal
Dim FullSpecDiv:FullSpecDiv  = 0.1* DivGlobal
Dim CCFull: CCFull = 1.0 * DivClrCor
Dim CCBlue: CCBlue = 1.0 * DivClrCor
Dim CCRedGreen: CCRedGreen = 0.9 * DivClrCor
Dim CCRed: CCRed = 0.9 * DivClrCor

Dim FlValues : FlValues=Array (1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0)
Dim AMTValues: AMTValues=Array (500,3000,4000,8000,16000,32000,64000,128000,128000,128000,128000,128000)
Dim BiasValues: BiasValues=Array (-100,-90,-80,-70,-60,-50,-40,-30,-20,-10,-9,-8)
Dim MixBlueChan: MixBlueChan=Array (8.0*BlueDiv, 16.0*BlueDiv, 34.0*BlueDiv, 64.0*BlueDiv, 72.0*BlueDiv, 80.0*BlueDiv, 84.0*BlueDiv, 82.0*BlueDiv, 80.0*BlueDiv, 78.0*BlueDiv, 76.0*BlueDiv)  
Dim MixRedChan: MixRedChan=Array (16.0 * RedDiv, 16.0* RedDiv, 34.0* RedDiv, 64.0* RedDiv, 72.0* RedDiv, 80.0* RedDiv, 84.0* RedDiv, 82.0* RedDiv, 80.0* RedDiv, 78.0* RedDiv, 76.0* RedDiv)  
Dim MixRedGreenChan: MixRedGreenChan=Array (8.0*RedGreenDiv, 16.0*RedGreenDiv, 34.0*RedGreenDiv, 64.0*RedGreenDiv, 72.0*RedGreenDiv, 80.0*RedGreenDiv, 84.0*RedGreenDiv, 82.0*RedGreenDiv, 80.0*RedGreenDiv, 78.0*RedGreenDiv, 76.0*RedGreenDiv)  
Dim MixFullSpectrum: MixFullSpectrum=Array (8.0*FullSpecDiv, 16.0*FullSpecDiv, 34.0*FullSpecDiv, 64.0*FullSpecDiv, 72.0*FullSpecDiv, 73.0*FullSpecDiv, 74.0*FullSpecDiv, 74.0*FullSpecDiv, 74.0*FullSpecDiv, 74.0*FullSpecDiv, 74.0*FullSpecDiv)  
Dim CCFSValues: CCFSValues=Array (100.0*CCFull, 1.0*CCFull, 0.1*CCFull, 0.01*CCFull, 0.001*CCFull, 0.0001*CCFull, 0.0001*CCFull, 0.0001*CCFull, 0.0001*CCFull,0.0001*CCFull,0.0001*CCFull)  
Dim CCBlueValues: CCBlueValues=Array (0.9*CCBlue, 1.0*CCBlue, 1.2*CCBlue, 1.4*CCBlue, 1.5*CCBlue, 1.55*CCBlue, 1.56*CCBlue, 1.565*CCBlue, 1.568*CCBlue, 1.569*CCBlue, 1.569*CCBlue)  

Dim FlData : FlData=Array (Flasher22,Flasher23,Flasher20,Flasher21,FlMirror,Empty,Empty,Empty)

'Full blue channel
FlData(0).amount = AMTValues(DNSVal ) * DivValues3(DNSVal)
FlData(0).DepthBias = BiasValues(DNSVal) * 100.0
FlData(0).intensityscale = FlData(0).intensityscale * MixBlueChan(DNSVal) '0.0008'
FlData(0).opacity = CCBlueValues(DNSVal) '* CCFSValues(DNSVal)'
'Half red/green channel
FlData(1).amount = AMTValues(DNSVal ) * DivValues3(DNSVal)
FlData(1).DepthBias = BiasValues(DNSVal) * 100.0
FlData(1).intensityscale = FlData(1).intensityscale * MixRedGreenChan(DNSVal)'* (MixRedChan(DNSVal) * SysDNSVal(DNSVal)) '0.0008'
FlData(1).opacity = CCRed
'Full red/green channel
FlData(2).amount = AMTValues(DNSVal ) * DivValues3(DNSVal)
FlData(2).DepthBias = BiasValues(DNSVal) * 100.0
FlData(2).intensityscale = FlData(2).intensityscale * MixRedGreenChan(DNSVal) '0.0008'
FlData(2).opacity = CCRedGreen
'Full Spectrum
FlData(3).amount = AMTValues(DNSVal ) * (DivValues3(DNSVal) * 0.1)
FlData(3).DepthBias = BiasValues(DNSVal) * 100.0
FlData(3).intensityscale = FlData(3).intensityscale * MixFullSpectrum(DNSVal) '0.0008'
FlData(3).opacity = FlData(3).opacity * CCFSValues(DNSVal)

'Full Spectrum Mirror
FlData(4).amount = AMTValues(DNSVal ) * (DivValues3(DNSVal) * 0.1)
FlData(4).DepthBias = BiasValues(DNSVal) * 100.0
FlData(4).intensityscale = FlData(4).intensityscale * (MixFullSpectrum(DNSVal) /3.0)'0.0008'
FlData(4).opacity = FlData(4).opacity * CCFSValues(DNSVal)


FlValues(0) = FlData(0).intensityscale
FlValues(1) = FlData(1).intensityscale
FlValues(2) = FlData(2).intensityscale
FlValues(3) = FlData(3).intensityscale
FlValues(4) = FlData(4).intensityscale
' ***************************************************************************
' ***************************************************************************
Const Forward = 190 ' 150
Const Height = 200 '250
Const RotX = -5 ' -5
'Full
FlData(0).y = FlData(0).y + Forward
FlData(0).Height = Height
FlData(0).RotX = Rotx
' upper half
FlData(1).y = FlData(1).y + Forward
FlData(1).Height = Height + 50
FlData(1).RotX = Rotx
'full
FlData(2).y = FlData(2).y + Forward
FlData(2).Height = Height
FlData(2).RotX = Rotx
' half
FlData(3).y = FlData(3).y + Forward
FlData(3).Height = Height+ 50
FlData(3).RotX = Rotx

FlData(4).y = FlData(4).y + Forward
FlData(4).Height = Height
FlData(4).RotX = Rotx

if 0 then
'FlMirror.y = FlData().y + Forward
FlMirror.Height = Height
FlMirror.RotX = Rotx
FlMirrorSol23.Height = Height
FlMirrorSol23.RotX = Rotx
FlMirrorSol24.Height = Height
FlMirrorSol24.RotX = Rotx
end if


Sub SetSMLiDNS(object, enabled)
	If enabled > 0 then
	object.intensity = enabled * SysDNSVal(DNSVal) /DivValues2(DNSVal)
	Else
	object.intensity =0
	end if	
End Sub

Sub SetSMFlDNS(object, enabled)

	If enabled > 0 then
	object.opacity = enabled / DivValues2(DNSVal) 
	else 
	object.opacity = 0
	end if
End Sub


Sub SetSLiDNS(object, enabled)
	If enabled then
	object.intensity = 1 * SysDNSVal(DNSVal) /DivValues2(DNSVal)
	Else
	object.intensity =0
	end if	
End Sub

Sub SetSFlDNS(object, enabled)

	If enabled then
	object.opacity = 1 * OPSValues(DNSVal) / DivLevel
	else 
	object.opacity = 0
	end if
End Sub


Sub SetDNSFlash(nr, object)
    Select Case LightState(nr)
        Case 0 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                LightState(nr) = -1 'completely off, so stop the fading loop
            End if
            Object.IntensityScale = FlashLevel(nr) * SysDNSVal(DNSVal) /DivValues2(DNSVal)
        Case 1 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                LightState(nr) = -1 'completely on, so stop the fading loop
            End if
            Object.IntensityScale = FlashLevel(nr) * SysDNSVal(DNSVal) /DivValues2(DNSVal)
    End Select
End Sub


Sub SetDNSFlashm(nr, object) 'multiple flashers, it just sets the intensity
    Object.IntensityScale = FlashLevel(nr) * SysDNSVal(DNSVal) /DivValues2(DNSVal)
End Sub

Sub SetDNSFlashex(object, value) 'it just sets the intensityscale for non system lights
    Object.IntensityScale = value * SysDNSVal(DNSVal) /DivValues2(DNSVal)
End Sub
