Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="pb_l5",UseSolenoids=2,UseLamps=0,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin"


'///////////////////////-----General Sound Options-----///////////////////////
'// VolumeDial:
'// VolumeDial is the actual global volume multiplier for the mechanical sounds.
'// Values smaller than 1 will decrease mechanical sounds volume.
'// Recommended values should be no greater than 1.
Const VolumeDial = 0.8



'///////////////////////-----Options-----///////////////////////

Dim VRRoom
Dim Scratches
Dim PFGlass


'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow, 1 = enable dynamic ball shadow

'VR Room
VRRoom = 0					  	' 0 = VR OFF, 1 = Minimal VR Room

'Playfield Glass and Glass Scratches
PFGlass = 0						' 0 = No glass, 1 = Glass
Scratches = 0					' 0 = No glass Scratches,  1 = Glass Scratches


'///////////////////////////////////////////////////////////////////////

Dim UseVPMDMD
Dim DesktopMode: DesktopMode = True
If VRRoom = 1 then UseVPMDMD = true Else UseVPMDMD = DesktopMode


LoadVPM "01560000", "S11.VBS", 3.26


Dim Ballsize,BallMass
BallSize = 50
BallMass = 1

Dim NullFader : set NullFader = new NullFadingObject
Dim FadeLights : Set FadeLights = New LampFader


'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim bsTrough, bsSaucer, bsREye, bsLEye, dtDTBank, mVisor

Sub Table1_Init
	SetLocale(1033)
	solRampDwn(True)
	PFGI(True)

	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Pinbot"&chr(13)&"bord"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
			.hidden = 0
		 On Error Resume Next
		 .Run GetPlayerHWnd
		 If Err Then MsgBox Err.Description
		 On Error Goto 0
	 End With
	 On Error Goto 0

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

	vpmNudge.TiltSwitch=1
	vpmNudge.Sensitivity=3
	vpmNudge.TiltObj=Array(sw52_Bumper1,sw48_Bumper2,sw53_Bumper3,LeftSlingshot,RightSlingshot)

		   Set bsTrough = New cvpmBallStack
			   bsTrough.InitSw 16,17,18,0,0,0,0,0
			   bsTrough.InitKick BallRelease, 160, 8
			   'bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
			   bsTrough.Balls = 2

		  Set bsSaucer = New cvpmBallStack
			  bsSaucer.InitSaucer sw38,38, 155, 12
			  bsSaucer.KickForceVar = 2
			  bsSaucer.KickAngleVar = 2
			  'bsSaucer.InitExitSnd SoundFX("Popper_ball",DOFContactors), SoundFX("Solenoid",DOFContactors)

		  Set bsLEye = New cvpmBallStack
			  bsLEye.InitSaucer sw25,25, 165, 12
			  bsLEye.KickForceVar = 2
			  bsLEye.KickAngleVar = 2
			  'bsLEye.InitExitSnd SoundFX("Popper_ball",DOFContactors), SoundFX("Solenoid",DOFContactors)

		  Set bsREye = New cvpmBallStack
			  bsREye.InitSaucer sw26,26, 195, 12
			  bsREye.KickForceVar = 2
			  bsREye.KickAngleVar = 2
			  'bsREye.InitExitSnd SoundFX("Popper_ball",DOFContactors), SoundFX("Solenoid",DOFContactors)

		   set dtDTBank = new cvpmdroptarget
			   dtDTBank.InitDrop Array(sw51, sw50, sw49), Array(51, 50, 49)
			   dtDTBank.Initsnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

	 Set mVisor = New cvpmMech
	 With mVisor
		 .MType = vpmMechOneSol + vpmMechReverse + vpmMechLinear
		 .Sol1 = 13
		 .Length = 120
		 .Steps = 58
		 .AddSw 46, 0, 0
		 .AddSw 47, 58, 58
		 .Callback = GetRef("UpdateVisor")
		 .Start
	 End With
	IF VRRoom = 1 Then
		setup_backglass()
	End If

End Sub

Sub Table1_exit()
	SaveLUT
	Controller.Stop
End sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Dim BIPL : BIPL = 0

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyCode = PlungerKey Then Plunger.Pullback:SoundPlungerPull()' plunger
	If keycode = LeftTiltKey Then Nudge 90, 5:SoundNudgeLeft() : WobbleBall.vely = -20
	If keycode = RightTiltKey Then Nudge 270, 5:SoundNudgeRight() : WobbleBall.vely = -20
	If keycode = CenterTiltKey Then Nudge 0, 3:SoundNudgeCenter() : WobbleBall.vely = -20 
	if keycode = StartGameKey then soundStartButton()

	If keycode = LeftFlipperKey Then 
		FlipperActivate LeftFlipper, LFPress
		If VRRoom = 1 Then
			If CoindoorIsOpen = 1 Then 'AXS Coindoor
				Playsound "ball_bounce"
				ManualPageNumber = ManualPageNumber - 1
				if ManualPageNumber = -1 then ManualPageNumber = 20   ' Set your instruction manual page number here.
				VRManualInstructions.Image = "Manual_Page" & ManualPageNumber
			End If
		End If
	End If

	If keycode = RightFlipperKey Then 
		FlipperActivate RightFlipper, RFPress 
		If VRRoom = 1 Then
			If CoindoorIsOpen = 1 Then 'AXS Coindoor			
				Playsound "ball_bounce"
				ManualPageNumber = ManualPageNumber + 1
				if ManualPageNumber = 21 then ManualPageNumber = 0  ' Set your instruction manual page number +1 here.
				VRManualInstructions.Image = "Manual_Page" & ManualPageNumber
			End If
		End If
	End IF

    If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
            Select Case Int(rnd*3)
                    Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
                    Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
                    Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
            End Select
    End If

	If KeyDownHandler(keycode) Then Exit Sub

    ' Manual Ball Control
	If keycode = 46 Then	 				' C Key
		If EnableBallControl = 1 Then
			EnableBallControl = 0
		Else
			EnableBallControl = 1
		End If
	End If
    If EnableBallControl = 1 Then
		If keycode = 48 Then 				' B Key
			If BCboost = 1 Then
				BCboost = BCboostmulti
			Else
				BCboost = 1
			End If
		End If
		If keycode = 203 Then BCleft = 1	' Left Arrow
		If keycode = 200 Then BCup = 1		' Up Arrow
		If keycode = 208 Then BCdown = 1	' Down Arrow
		If keycode = 205 Then BCright = 1	' Right Arrow
	End If
	If VRRoom = 1 Then
		If keycode = LeftFlipperKey Then 
			Primary_flipper_button_left.X = 2100.804 + 8
		End If
		If keycode = RightFlipperKey Then
			Primary_flipper_button_right.X = 2110.103 - 8
		End If
		If keycode = PlungerKey Then  
			TimerVRPlunger.Enabled = True
			TimerVRPlunger2.Enabled = False
		End If
	End If

 	If Keycode = LeftMagnaSave Then
				LUTSet = LUTSet  - 1
		if LutSet < 0 then LUTSet = 15
				lutsetsounddir = 1
			If LutToggleSound then
				If lutsetsounddir = 1 And LutSet <> 15 Then
					Playsound "click", 0, 1, 0.1, 0.25 
				End If
			If lutsetsounddir = -1 And LutSet <> 15 Then
					Playsound "click", 0, 1, 0.1, 0.25 
				End If
				LutSlctr.enabled = true
			SetLUT
			ShowLUT
		End If
	End If

 	If Keycode = RightMagnaSave Then
				LUTSet = LUTSet  + 1
		if LutSet > 15 then LUTSet = 0
				lutsetsounddir = 1
			If LutToggleSound then
				If lutsetsounddir = 1 Then
					Playsound "click", 0, 1, 0.1, 0.25 
				End If
			If lutsetsounddir = -1 Then
					Playsound "click", 0, 1, 0.1, 0.25 
				End If
				LutSlctr.enabled = true
			SetLUT
			ShowLUT
		End If
	End If

	'********************************************************************************
	'             COINDOOR OPEN AND CLOSE BUTTON ROUTINE - RASCAL & RAWD
	'********************************************************************************
	If keycode = 207 then ' use 'keyCoinDoor' instead of '207' for Rom driven tables.
		If VRRoom = 1 Then
			If DoorReady = true then
				DoorReady = false 
				If DoorMove = -1 then 
					PlaySound "CoindoorOpen":DCO = True:CoindoorIsOpen = 1 
				Else 
					PlaySound "page2":MA = True:CoindoorIsOpen = 0:ManualPageNumber = 0
					VRManualInstructions.Image = "Manual_Page" & ManualPageNumber
				End If
			End If
		End If
	End If
	'*********************************************************************************

	'AXS Coindoor
	If VRRoom = 1 Then
		If keycode = 8 and CoindoorIsOpen = 1 Then CoindoorB1.transy = CoindoorB1.transy + 10: playsound "Coindoor_Button"	
		If keycode = 9 and CoindoorIsOpen = 1 Then CoindoorB2.transy = CoindoorB2.transy + 10: playsound "Coindoor_Button"
		If keycode = 10 and CoindoorIsOpen = 1 Then CoindoorB3.transy = CoindoorB3.transy + 10: playsound "Coindoor_Button"
		If keycode = 11 and CoindoorIsOpen = 1 Then CoindoorB4.transy = CoindoorB4.transy + 10: playsound "Coindoor_Button"
	End If
End Sub


Sub Table1_KeyUp(ByVal KeyCode)
	
    If KeyCode = PlungerKey Then
            Plunger.Fire
            If BIPL = 1 Then
				SoundPlungerReleaseBall()                    'Plunger release sound when there is a ball in shooter lane
            Else
				SoundPlungerReleaseNoBall()                       'Plunger release sound when there is no ball in shooter lane
            End If
    End If

	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress

	If KeyUpHandler(keycode) Then Exit Sub
    'Manual Ball Control
	If EnableBallControl = 1 Then
		If keycode = 203 Then BCleft = 0	' Left Arrow
		If keycode = 200 Then BCup = 0		' Up Arrow
		If keycode = 208 Then BCdown = 0	' Down Arrow
		If keycode = 205 Then BCright = 0	' Right Arrow
	End If
	
	If VRRoom = 1 Then
		If keycode = LeftFlipperKey Then 
			Primary_flipper_button_left.X = 2100.804
		End If
		If keycode = RightFlipperKey Then
			Primary_flipper_button_right.X = 2110.103
		End If
		If keycode = PlungerKey Then  
			TimerVRPlunger.Enabled = False
			TimerVRPlunger2.Enabled = True
		End If
		If keycode = 8 and CoindoorIsOpen = 1 Then CoindoorB1.transy = CoindoorB1.transy - 10: playsound "Coindoor_ButtonOff"
		If keycode = 9 and CoindoorIsOpen = 1 Then CoindoorB2.transy = CoindoorB2.transy - 10: playsound "Coindoor_ButtonOff"	
		If keycode = 10 and CoindoorIsOpen = 1 Then CoindoorB3.transy = CoindoorB3.transy - 10: playsound "Coindoor_ButtonOff"
		If keycode = 11 and CoindoorIsOpen = 1 Then	CoindoorB4.transy = CoindoorB4.transy - 10: playsound "Coindoor_ButtonOff"	
	End If
End Sub


'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************
SolCallback(1) = "bsTrough.SolIn"
SolCallback(2) = "bsTrough.SolOut"
SolCallback(3) = "bsSaucer.SolOut"
SolCallback(4) = "ResetDrops" 'Drop Targets
SolCallback(5) = "solRampUp"
SolCallback(6) = "solRampDwn"
SolCallback(7) = "bsLEye.SolOut"
SolCallback(8) = "bsREye.SolOut"
'9 is the backbox robot face
SolCallback(10) = "SetLamp 101," 'RightEye Flash
'11 is the backbox GI
SolCallback(12) = "PFGI"
'SolCallback(13) = "visormotor"
'14 is the solenoid select relay

'15 is the third topper flasher
'16 is the fourth topper flasher (center)
'17 is the bottom bumper
SolCallback(18) = "SetLamp 104," 'LeftEye Flash
'19 is the middle bumper
'20 is the left slingshot
'21 is the right slingshot
'22 is the top bumper
SolCallback(23) = "vpmNudge.SolGameOn"
SolCallback(25) =  "SolKnocker"
'SolCallback(26) = "SetLamp 105," 'Flashers Top
'27 is the left backbox flipper
'28 is the right backbox flipper
'SolCallback(29) = "SetLamp 106," 'Flashers Lower
SolCallback(30) = "SetLamp 107," 'Bumper Flasher
SolCallback(31) = "SetLamp 108," 'Flashers Left
SolCallback(32) = "SetLamp 109," 'sun flasher

SolCallback(29) = "Sol29"
SolCallback(26) = "Sol26"  
SolCallback(15) = "Sol15"
SolCallback(16) = "Sol16"


Sub Sol29(Enabled)
	If Enabled Then
		SetLamp 106, 1
		If VRRoom = 1 Then
			Flashertopper29.visible = 1 
			Flashertopper29L.visible = 1 
			topper.blenddisablelighting=0.25
			FlashertopperWallLight.visible = 1
		End If
	Else
		SetLamp 106, 0
		If VRRoom = 1 Then
			Flashertopper29.visible = 0	
			Flashertopper29L.visible = 0
			topper.blenddisablelighting=0.15
			FlashertopperWallLight.visible = 0
		End If
	End If
End Sub

Sub Sol26(Enabled)
	If Enabled Then
		SetLamp 105, 1
		If VRRoom = 1 Then
			Flashertopper26.visible = 1 
			Flashertopper26L.visible = 1 
			topper.blenddisablelighting=0.25
			FlashertopperWallLight.visible = 1 
		End If
	Else
		SetLamp 105, 0
		If VRRoom = 1 Then
			Flashertopper26.visible = 0	
			Flashertopper26l.visible = 0
			topper.blenddisablelighting=0.20
			FlashertopperWallLight.visible = 0
			FlashertopperWallLight.visible = 0
		End If
	End If 
End Sub

Sub Sol15(Enabled)
	If Enabled Then
		If VRRoom = 1 Then
			Flashertopper15.visible = 1 
			Flashertopper15l.visible = 1
			topper.blenddisablelighting=0.25
			FlashertopperWallLight.visible = 1 
		End If
	Else
		If VRRoom = 1 Then
			Flashertopper15.visible = 0	
			Flashertopper15l.visible = 0
			topper.blenddisablelighting=0.15
			FlashertopperWallLight.visible = 0 
			FlashertopperWallLight.visible = 0
		End If
	End If 
End Sub

Sub Sol16(Enabled)
	If Enabled Then
		If VRRoom = 1 Then
			Flashertopper16.visible = 1 
			Flashertopper16l.visible = 1 
			topper.blenddisablelighting=0.25
			FlashertopperWallLight.visible = 1 
		End If
	Else
		If VRRoom = 1 Then
			Flashertopper16.visible = 0	
			Flashertopper16l.visible = 0
			topper.blenddisablelighting=0.15
			FlashertopperWallLight.visible = 0 
			FlashertopperWallLight.visible = 0
		End If
	End If 
End Sub


'******************************************************
'				NFOZZY'S FLIPPERS
'******************************************************

Const ReflipAngle = 20

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
            LF.Fire  'leftflipper.rotatetoend
    
            If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
                    RandomSoundReflipUpLeft LeftFlipper
            Else 
                    SoundFlipperUpAttackLeft LeftFlipper
                    RandomSoundFlipperUpLeft LeftFlipper
            End If                
	Else
            LeftFlipper.RotateToStart
            If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
                    RandomSoundFlipperDownLeft LeftFlipper
            End If
            FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub
  
Sub SolRFlipper(Enabled)

    If Enabled Then
            RF.Fire 'rightflipper.rotatetoend

            If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
                    RandomSoundReflipUpRight RightFlipper
            Else 
                    SoundFlipperUpAttackRight RightFlipper
                    RandomSoundFlipperUpRight RightFlipper
            End If
    Else
            RightFlipper.RotateToStart
            If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
                    RandomSoundFlipperDownRight RightFlipper
            End If        
            FlipperRightHitParm = FlipperUpSoundLevel
    End If
End Sub

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
end sub

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 1.5 	'1.0 FEOST
Const EOSAnew = 2		'0.2
Const EOSRampup = 0	'0.5 
Const SOSRampup = 8.5 '2.5 
Const LiveCatch = 8
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.0225

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

Sub FlipperActivate(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity

	Flipper.eostorque = EOST 						'new
	Flipper.eostorqueangle = EOSA 				'new
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA
	Flipper.eostorque = EOST*EOSReturn/FReturn 	'EOST

	
	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim BOT, b
		BOT = GetBalls
			
		For b = 0 to UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)	'-1 for Right Flipper

	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 
			Flipper.endangle = FEndAngle - 3*Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0 
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) and FlipperPress = 1 then
		if FCount = 0 Then FCount = GameTime

'		if GameTime - FCount < LiveCatch Then
'			Flipper.Elasticity = LiveElasticity
'		elseif GameTime - FCount < LiveCatch * 2 Then
'			Flipper.Elasticity = 0.1
'		Else
'			Flipper.Elasticity = FElasticity
'		end if

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup			
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST						'EOST
			Flipper.eostorqueangle = EOSA				'EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If

	End If
End Sub

Const LiveDistanceMin = 30  'minimum distance in vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)	'-1 for Right Flipper

	if GameTime - FCount < LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		If ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = 0
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
	Else
		If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm, 1
	End If 
End Sub

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub

'******************************************************
'				FLIPPER AND RUBBER CORRECTION
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		'safety coefficient (diminishes polarity correction only)
		'x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 0	'don't mess with these
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1

		x.enabled = True
		'x.DebugOn = True : stickL.visible = True : tbpl.visible = True : vpmSolFlipsTEMP.DebugOn = True
		x.TimeDelay = 80
	Next

	'rf.report "Velocity"
	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.1, 	1.07
	addpt "Velocity", 2, 0.2, 	1.15
	addpt "Velocity", 3, 0.3, 	1.25
	addpt "Velocity", 4, 0.41, 1.05
	addpt "Velocity", 5, 0.65, 	1.0'0.982
	addpt "Velocity", 6, 0.702, 0.968
	addpt "Velocity", 7, 0.95,  0.968
	addpt "Velocity", 8, 1.03, 	0.945

	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.05, -3.7	
	AddPt "Polarity", 2, 0.33, -3.7
	AddPt "Polarity", 3, 0.37, -3.7
	AddPt "Polarity", 4, 0.41, -3.7
	AddPt "Polarity", 5, 0.45, -3.7 
	AddPt "Polarity", 6, 0.576,-3.7
	AddPt "Polarity", 7, 0.66, -2.3
	AddPt "Polarity", 8, 0.743, -1
	AddPt "Polarity", 9, 0.81, -1
	AddPt "Polarity", 10, 0.88, 0

'	'rf.report "Polarity"
'	AddPt "Polarity", 0, 0, -2.7
'	AddPt "Polarity", 1, 0.16, -2.7	
'	AddPt "Polarity", 2, 0.33, -2.7
'	AddPt "Polarity", 3, 0.37, -2.7	'4.2
'	AddPt "Polarity", 4, 0.41, -2.7
'	AddPt "Polarity", 5, 0.45, -2.7 '4.2
'	AddPt "Polarity", 6, 0.576,-2.7
'	AddPt "Polarity", 7, 0.66, -1.8'-2.1896
'	AddPt "Polarity", 8, 0.743, -0.5
'	AddPt "Polarity", 9, 0.81, -0.5
'	AddPt "Polarity", 10, 0.88, 0


'	AddPt "Polarity", 0, 0, 0
'	AddPt "Polarity", 1, 0.05, -5.5
'	AddPt "Polarity", 2, 0.4, -5.5
'	AddPt "Polarity", 3, 0.8, -5.5	
'	AddPt "Polarity", 4, 0.85, -5.25
'	AddPt "Polarity", 5, 0.9, -4.25
'	AddPt "Polarity", 6, 0.95, -3.75
'	AddPt "Polarity", 7, 1, -3.25
'	AddPt "Polarity", 8, 1.05, -2.25
'	AddPt "Polarity", 9, 1.1, -1.5
'	AddPt "Polarity", 10, 1.15, -1
'	AddPt "Polarity", 11, 1.2, -0.5
'	AddPt "Polarity", 12, 1.25, 0
'	AddPt "Polarity", 13, 1.3, 0

	LF.Object = LeftFlipper	
	LF.EndPoint = EndPointLp	'you can use just a coordinate, or an object with a .x property. Using a couple of simple primitive objects
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

'Trigger Hit - .AddBall activeball
'Trigger UnHit - .PolarityCorrect activeball

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub

'Methods:
'.TimeDelay - Delay before trigger shuts off automatically. Default = 80 (ms)
'.AddPoint - "Polarity", "Velocity", "Ycoef" coordinate points. Use one of these 3 strings, keep coordinates sequential. x = %position on the flipper, y = output
'.Object - set to flipper reference. Optional.
'.StartPoint - set start point coord. Unnecessary, if .object is used.

'Called with flipper - 
'ProcessBalls - catches ball data. 
' - OR - 
'.Fire - fires flipper.rotatetoend automatically + processballs. Requires .Object to be set to the flipper.

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart,FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)
	
	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub
	
	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : FlipperEnd = aInput.x: FlipperEndY = aInput.y: End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property	
	Public Property Get EndPointY: EndPointY = FlipperEndY : End Property

	
	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		if gametime > 100 then Report aChooseArray
	End Sub 

	Public Sub Report(aChooseArray) 	'debug, reports all coords in tbPL.text
		if not DebugOn then exit sub
		dim a1, a2 : Select Case aChooseArray
			case "Polarity" : a1 = PolarityIn : a2 = PolarityOut
			Case "Velocity" : a1 = VelocityIn : a2 = VelocityOut
			Case "Ycoef" : a1 = YcoefIn : a2 = YcoefOut 
			case else :tbpl.text = "wrong string" : exit sub
		End Select
		dim str, x : for x = 0 to uBound(a1) : str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		tbpl.text = str
	End Sub
	
	Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if : Next  : End Sub

	Private Sub RemoveBall(aBall)
		dim x : for x = 0 to uBound(balls)
			if TypeName(balls(x) ) = "IBall" then 
				if aBall.ID = Balls(x).ID Then
					balls(x) = Empty
					Balldata(x).Reset
				End If
			End If
		Next
	End Sub
	
	Public Sub Fire() 
		Flipper.RotateToEnd
		processballs
	End Sub

	Public Property Get Pos 'returns % position a ball. For debug stuff.
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next		
	End Property

	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function	'Timer shutoff for polaritycorrect
	
	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				RemoveBall aBall
				exit Sub
			end if

			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)						'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
	 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
	
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
				'debug.print BallPos & " " & AddX & " " & Ycoef & " "& PartialFlipcoef & " "& VelCoef
				'playsound "knocker"
			End If
		End If
		RemoveBall aBall
	End Sub
End Class


'================================
'Helper Functions


Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)	'Shuffle objects in a temp array
		if not IsEmpty(aArray(x) ) Then
			if IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	if offset < 0 then offset = 0
	redim aArray(aCount-1+offset)	'Resize original array
	for x = 0 to aCount-1		'set objects back into original array
		if IsObject(a(x)) then 
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub


Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

Function NullFunctionZ(aEnabled):End Function	'1 argument null function placeholder	 TODO move me or replac eme

Class spoofball 
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius 
	Public Property Let Data(aBall)
		With aBall
			x = .x : y = .y : z = .z : velx = .velx : vely = .vely : velz = .velz
			id = .ID : mass = .mass : radius = .radius
		end with
	End Property
	Public Sub Reset()
		x = Empty : y = Empty : z = Empty  : velx = Empty : vely = Empty : velz = Empty 
		id = Empty : mass = Empty : radius = Empty
	End Sub
End Class

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function


'****************************************************************************
'PHYSICS DAMPENERS

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
End Sub

dim RubbersD : Set RubbersD = new Dampener	'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 0.935 '0.96	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.935 '0.96
RubbersD.addpoint 2, 5.76, 0.942 '0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener	'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'#########################    Adjust these values to increase or lessen the elasticity

dim FlippersD : Set FlippersD = new Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False	
FlippersD.addpoint 0, 0, 1.1	
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn 'tbpOut.text
	public name, Threshold 	'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		if gametime > 100 then Report
	End Sub

	public sub Dampen(aBall)
		if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		'playsound "fx_knocker"
		if debugOn then TBPout.text = str
	End Sub


	public sub Dampenf(aBall, parm, ver)
		if ver = 2 Then
			If parm < 10 And parm > 2 And Abs(aball.angmomz) < 15 And aball.vely < 0 then
				aball.angmomz = aball.angmomz * 1.2
				aball.vely = aball.vely * (1.1 + (parm/50))
			Elseif parm <= 2 and parm > 0.2 And aball.vely < 0 Then
				if (aball.velx > 0 And aball.angmomz > 0) Or (aball.velx < 0 And aball.angmomz < 0) then
			        	aball.angmomz = aball.angmomz * -0.7
				Else
					aball.angmomz = aball.angmomz * 1.2
				end if
				aball.vely = aball.vely * (1.2 + (parm/10))
			End if
		Else
			dim RealCOR, DesiredCOR, str, coef
			DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
			RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
			coef = desiredcor / realcor 
			If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
				aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
			End If
		End If
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub


	Public Sub Report() 	'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub
	

End Class

''Tracks ball velocity for judging bounce calculations & angle
''apologies to JimmyFingers is this is what his script does. I know his tracks ball velocity too but idk how it works in particular
'dim cor : set cor = New CoRTracker
'cor.debugOn = False
''cor.update() - put this on a low interval timer
'Class CoRTracker
'	public DebugOn 'tbpIn.text
'	public ballvel
'
'	Private Sub Class_Initialize : redim ballvel(0) : End Sub 
'	'TODO this would be better if it didn't do the sorting every ms, but instead every time it's pulled for COR stuff
'	Public Sub Update()	'tracks in-ball-velocity
'		dim str, b, AllBalls, highestID : allBalls = getballs
'		'if uBound(allballs) < 0 then if DebugOn then str = "no balls" : TBPin.text = str : exit Sub else exit sub end if: end if
'		for each b in allballs
'			if b.id >= HighestID then highestID = b.id
'		Next
'
'		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
'
'		for each b in allballs
'			ballvel(b.id) = BallSpeed(b)
''			if DebugOn then 
''				dim s, bs 'debug spacer, ballspeed
''				bs = round(BallSpeed(b),1)
''				if bs < 10 then s = " " else s = "" end if
''				str = str & b.id & ": " & s & bs & vbnewline 
''				'str = str & b.id & ": " & s & bs & "z:" & b.z & vbnewline 
''			end if
'		Next
'		'if DebugOn then str = "ubound ballvels: " & ubound(ballvel) & vbnewline & str : if TBPin.text <> str then TBPin.text = str : end if
'	End Sub
'End Class



'******************************************************
'                TRACK ALL BALL VELOCITIES
'                 FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
        public ballvel, ballvelx, ballvely

        Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

        Public Sub Update()        'tracks in-ball-velocity
                dim str, b, AllBalls, highestID : allBalls = getballs

                for each b in allballs
                        if b.id >= HighestID then highestID = b.id
                Next

                if uBound(ballvel) < highestID then redim ballvel(highestID)        'set bounds
                if uBound(ballvelx) < highestID then redim ballvelx(highestID)        'set bounds
                if uBound(ballvely) < highestID then redim ballvely(highestID)        'set bounds

                for each b in allballs
                        ballvel(b.id) = BallSpeed(b)
                        ballvelx(b.id) = b.velx
                        ballvely(b.id) = b.vely
                Next
        End Sub
End Class

Sub RDampen_Timer()
        Cor.Update
End Sub

' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	'Clamp if on the boundry lines
	'if L=1 and Y < yLvl(LBound(yLvl) ) then Y = yLvl(lBound(yLvl) )
	'if L=uBound(xKeyFrame) and Y > yLvl(uBound(yLvl) ) then Y = yLvl(uBound(yLvl) )
	'clamp 2.0
	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function


dim GiIsOff

'Playfield GI
Sub PFGI(Enabled)
	If Enabled Then  ' GI state inverted, enabled = OFF
		dim xx
		For each xx in GI:xx.State = 0: Next
		For each xx in rubbers_textured: xx.image = "rubbasGIOFF": Next
        PlaySound "fx_relay"
		GIIsOff=true
		helmet_prim.blenddisablelighting=0.05
		targetwrapper_prim.blenddisablelighting=0.05
		sw28_prim.blenddisablelighting=0
		sw29_prim.blenddisablelighting=0
		sw30_prim.blenddisablelighting=0
		sw31_prim.blenddisablelighting=0
		sw32_prim.blenddisablelighting=0
		metalramp_prim.image="metalrampGIOFF"
		psw51.image="drop1off"
		psw50.image="drop2off"
		psw49.image="drop3off"
		upperrubbers.image="upperpfrubbersoff"
		rubberposts.image="rubberpostsoff"
		r18_prim.image="18off"
		laneguideL.image="metallaneguideloff"
		laneguideR.image="metallaneguideroff"
		metalsPegs.image="metalpostsoff"
		outerr.image="outerrightoff"
		outerl.image="outerleftoff"
		outerb.image="outerbackoff"
		outerinner_prim.image="woodguideoff"
		plasticrightsling.image="rightslingoff"
		plasticleftsling.image="leftslingoff"
		plasticleftoutlane.image="leftoutlaneplasticoff"
		plasticrightoutlane.image="rightoutlaneplasticoff"
		plasticrightflash.image="rightflasherplasticoff"
		plasticleftflashlow.image="leftflashplasticoff"
		plasticleftflashhigh.image="plasticflash2off"
		plasticleftcorner.image="plasticleftcorneroff"
		plasticrightcork.image="rightcorkplasticoff"
		plasticrighttargets.image="plastictargetsoff"
		corkscrew_prim.image="corkscrewoff"
		prim_upperpf.image="upperpfoff"
		acorns.image="acornsoff"
		screws3.image="screws3off"
		screws1.image="screws1off"
		brackets.image="bracketsoff"'
		rubberrk.image="upperpfrubbersoff"
		rubberrk1.image="upperpfrubbersoff"
		rubberrk2.image="upperpfrubbersoff"
		IF VRRoom = 1 Then
			outerrVR.image="outerrightoff"
			outerlVR.image="outerleftoff"
			outerlVR1.image="outerleftoffcut"
		End If
	Else
		For each xx in GI:xx.State = 1: Next
		For each xx in rubbers_textured: xx.image = "rubbasGION": Next
        PlaySound "fx_relay"
		GIIsOff=false
		helmet_prim.blenddisablelighting=.25
		targetwrapper_prim.blenddisablelighting=0.14
		sw28_prim.blenddisablelighting=0.1
		sw29_prim.blenddisablelighting=0.1
		sw30_prim.blenddisablelighting=0.1
		sw31_prim.blenddisablelighting=0.1
		sw32_prim.blenddisablelighting=0.1
		metalramp_prim.image="metalrampGION"
		psw51.image="drop1on"
		psw50.image="drop2on"
		psw49.image="drop3on"
		upperrubbers.image="upperpfrubberson"
		rubberposts.image="rubberpostson"
		r18_prim.image="18on"
		laneguideL.image="metallaneguidelon"
		laneguideR.image="metallaneguideron"
		metalsPegs.image="metalpostson"
		outerr.image="outerrighton"
		outerl.image="outerlefton"
		If VRRoom = 1 Then
			outerrVR.image="outerrighton"
			outerlVR.image="outerlefton"
			outerlVR1.image="outerleftoncut"	
		End If
		outerb.image="outerbackon"
		outerinner_prim.image="woodguideon"
		plasticrightsling.image="rightslingon"
		plasticleftsling.image="leftslingon"
		plasticleftoutlane.image="leftoutlaneplasticon"
		plasticrightoutlane.image="rightoutlaneplasticon"
		plasticrightflash.image="rightflasherplasticon"
		plasticleftflashlow.image="leftflashplasticon"
		plasticleftflashhigh.image="plasticflash2on"
		plasticleftcorner.image="plasticleftcorneron"
		plasticrightcork.image="rightcorkplasticon"
		plasticrighttargets.image="plastictargetson"
		corkscrew_prim.image="corkscrewon"
		prim_upperpf.image="upperpfon"
		acorns.image="acornson"
		screws3.image="screws3on"
		screws1.image="screws1on"
		brackets.image="bracketson"
		rubberrk.image="upperpfrubberson"
		rubberrk1.image="upperpfrubberson"
		rubberrk2.image="upperpfrubberson"
	End If
	' Update lamps that may swap textures when GI changes here
	SetLamp 110, Not Enabled
	SetLamp 111, Enabled
	UpdateLamp101
	UpdateLamp104
	UpdateLamp105
	UpdateLamp106
	UpdateLamp107
	UpdateLamp108
End Sub


'**********************************************************************************************************

'Solenoid Controlled toys
'**********************************************************************************************************


'Sub RampCallback(lvl)
'	lramp_prim.rotx=-12 * lvl
'	lrampmetal_prim.rotx=-12 * lvl
'	lrampflap_prim.rotx=-12 * lvl
'	lramplever_prim.transy=-40 * lvl
'	lramplever_prim.transz=-16 * lvl
'	If lvl < 0.33 Then
'		rightramp.Collidable = False
'		rightrampmid.Collidable = False
'		rightrampup.Collidable = true
'		lramp_prim1.visible = False
'		lramp_prim2.visible = True
'	ElseIf lvl < 0.8 Then
'		rightramp.Collidable = False
'		rightrampmid.Collidable = True
'		rightrampup.Collidable = True
'	Else
'		rightramp.Collidable = True
'		rightrampmid.Collidable = False
'		rightrampup.Collidable = False
'		lramp_prim1.visible = True
'		lramp_prim2.visible = False
'	End If
'End Sub


Sub SolKnocker(Enabled)
        If enabled Then
                KnockerSolenoid 'Add knocker position object
        End If
End Sub
 
Dim RampDir, RampLvl, RampSpeed
RampLvl = 1
RampSpeed = 0.05
	
Sub RampTimer_Timer()
	RampLvl = RampLvl + RampDir

	If RampLvl < 0 and RampDir < 0 Then
		RampLvl = 0
		me.enabled = False
	ElseIf RampLvl > 1 and RampDir > 0 Then
		RampLvl = 1
		me.enabled = False
	ElseIf RampLvl < 0.33 Then
		rightramp.Collidable = False
		rightrampmid.Collidable = False
		rightrampup.Collidable = true
		lramp_prim1.visible = True
		lramp_prim2.visible = False
		lramp_prim3.visible = False
		Controller.Switch(44)=0
	ElseIf RampLvl < 0.8 Then
		rightramp.Collidable = False
		rightrampmid.Collidable = True
		rightrampup.Collidable = True
		lramp_prim1.visible = False
		lramp_prim2.visible = True
		lramp_prim3.visible = False
	Else
		rightramp.Collidable = True
		rightrampmid.Collidable = False
		rightrampup.Collidable = False
		lramp_prim1.visible = False
		lramp_prim2.visible = False
		lramp_prim3.visible = True
		Controller.Switch(44)=1
	End If


	lramp_prim.rotx=-12 * RampLvl
	lrampmetal_prim.rotx=-12 * RampLvl
	lrampflap_prim.rotx=-12 * RampLvl
	lramplever_prim.transy=-40 * RampLvl
	lramplever_prim.transz=-16 * RampLvl
End Sub


Sub solRampUp(Enabled)
	If Enabled then
		Playsoundat "topdiverteron", lramp_prim 
 		'Controller.Switch(44)=0
		'SetLamp 120, 0
		RampDir = -RampSpeed
		RampTimer.enabled = True


		Dim BOT, b
		BOT = GetBalls

		If UBound(BOT) = -1 Then Exit Sub

		For b = 0 to UBound(BOT)
			If lrampflap_prim.rotx < -10 and InRect(BOT(b).x, BOT(b).y, 74, 640, 165,610,213,746,118,778) Then
				BOT(b).velz = 40
				BOT(b).velx = BOT(b).velx + 1.5
				BOT(b).vely = BOT(b).vely + 5
			Elseif lrampflap_prim.rotx < -10 and InRect(BOT(b).x, BOT(b).y, 25, 505, 117,475,165,610,74,640) Then
				BOT(b).velz = 30
				BOT(b).velx = BOT(b).velx +1
				BOT(b).vely = BOT(b).vely + 3
			Elseif lrampflap_prim.rotx < -10 and InRect(BOT(b).x, BOT(b).y, 35, 503, 30,451,118,446,120,477) Then
				BOT(b).velz = 15
				BOT(b).velx = BOT(b).velx + 0.5
				BOT(b).vely = BOT(b).vely + 1.5
			End If
		Next
 	end if
End Sub

Sub solRampDwn(Enabled)
 	If Enabled then
 		'Controller.Switch(44)=1
		'SetLamp 120, 1
		RampDir = RampSpeed
		RampTimer.enabled = True
		Playsoundat "topdiverteroff", lramp_prim 
 	end if
End Sub
 
 'VISOR
Sub UpdateVisor(currpos, currspeed, lastpos)
	dim xx
	For each xx in Visor:xx.RotX  = currpos * 12 / 58 : Next
	For each xx in targetbank:xx.TransZ  = -currpos: Next
	if currpos > 50 Then
		targetslow.collidable = false
	elseif currpos > 42 then
		targetslow.collidable = true
		targetsmid.collidable = false
	elseif currpos > 14 then 
		targetsmid.collidable = true
		targetslow.collidable = false
		targetshigh.collidable=false	
		for each xx in targetbanktargets:xx.Collidable = false:Next
	elseif currpos < 14 Then	
		targetsmid.collidable=false
		targetshigh.collidable=true
		for each xx in targetbanktargets:xx.Collidable = true:Next
	end if 
	xx = CInt((currpos * 9) / 29)
	if xx > 8 then xx = 8
	targetwrapper_prim.image = "targetbank" & chr(97+xx)
	'debug.print currpos
	If currspeed <> 0 then
		PlaySound SoundFX("gunmotor",DOFGear), 0, 1, AudioPan(sw30), 0,0, 1, 0, AudioFade( sw30)
	Else
		Stopsound "gunmotor"
	end if
 End Sub


'**********************************************************************************************************

 ' Drain hole and kickers
Sub Drain_Hit
	RandomSoundDrain Drain
	RF.PolarityCorrect Activeball
	LF.PolarityCorrect Activeball
	bsTrough.addball me
End Sub
Sub BallRelease_UnHit : RandomSoundBallRelease BallRelease End Sub

Sub sw25_Hit:bsLEye.AddBall 0 : SoundSaucerLock : End Sub
Sub sw25_UnHit: SoundSaucerKick 1, sw25 : End Sub
Sub sw26_Hit:bsREye.AddBall 0 : SoundSaucerLock : End Sub
Sub sw26_UnHit: SoundSaucerKick 1, sw26 : End Sub
Sub sw38_Hit:bsSaucer.AddBall 0 : SoundSaucerLock : End Sub
Sub sw38_UnHit: SoundSaucerKick 1, sw38 : End Sub

dim TargetTransY, TargetDelay
TargetTransY = -8
TargetDelay = 75

'Standup targets
Sub sw19_Hit:vpmTimer.PulseSw 19:End Sub
Sub sw28_Hit:vpmTimer.PulseSw 28:playsoundat"target", sw28:sw28_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw28_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw28_prim.transy=0'":End Sub
Sub sw29_Hit:vpmTimer.PulseSw 29:sw29_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw29_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw29_prim.transy=0'":End Sub
Sub sw30_Hit:vpmTimer.PulseSw 30:sw30_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw30_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw30_prim.transy=0'":End Sub
Sub sw31_Hit:vpmTimer.PulseSw 31:sw31_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw31_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw31_prim.transy=0'":End Sub
Sub sw32_Hit:vpmTimer.PulseSw 32:sw32_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw32_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw32_prim.transy=0'":End Sub
Sub sw33_Hit:vpmTimer.PulseSw 33:sw33_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw33_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw33_prim.transy=0'":End Sub
Sub sw34_Hit:vpmTimer.PulseSw 34:sw34_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw34_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw34_prim.transy=0'":End Sub
Sub sw35_Hit:vpmTimer.PulseSw 35:sw35_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw35_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw35_prim.transy=0'":End Sub
Sub sw36_Hit:vpmTimer.PulseSw 36:sw36_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw36_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw36_prim.transy=0'":End Sub
Sub sw37_Hit:vpmTimer.PulseSw 37:sw37_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw37_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw37_prim.transy=0'":End Sub
Sub sw45_Hit:vpmTimer.PulseSw 45:End Sub

'Combo Target Hits
Sub sw2829_Hit:sw28_Hit:sw29_Hit:End Sub
Sub sw2930_Hit:sw29_Hit:sw30_Hit:End Sub
Sub sw3031_Hit:sw30_Hit:sw31_Hit:End Sub
Sub sw3132_Hit:sw31_Hit:sw32_Hit:End Sub

Sub sw3334_Hit:sw33_Hit:sw34_Hit:End Sub
Sub sw3435_Hit:sw34_Hit:sw35_Hit:End Sub
Sub sw3536_Hit:sw35_Hit:sw36_Hit:End Sub
Sub sw3637_Hit:sw36_Hit:sw37_Hit:End Sub

'Drop Targets
Sub Sw51_Hit:DTHit 51:End Sub
Sub Sw50_Hit:DTHit 50:End Sub
Sub Sw49_Hit:DTHit 49 End Sub

'Wire Triggers
Sub sw12_Hit:Controller.Switch(12) = 1:End Sub
Sub sw12_UnHit:Controller.Switch(12) = 0:End Sub
Sub sw13_Hit:Controller.Switch(13) = 1:End Sub
Sub sw13_UnHit:Controller.Switch(13) = 0:End Sub
Sub sw14_Hit:Controller.Switch(14) = 1:End Sub
Sub sw14_UnHit:Controller.Switch(14) = 0:End Sub
Sub sw15_Hit:Controller.Switch(15) = 1:End Sub
Sub sw15_UnHit:Controller.Switch(15) = 0:End Sub
Sub sw20_Hit:Controller.Switch(20) = 1: BIPL=1 : End Sub
Sub sw20_UnHit:Controller.Switch(20) = 0: BIPL=0 : End Sub
Sub sw22_Hit:me.TimerInterval=100:me.TimerEnabled = 1::End Sub  'Debug.Print "sw22":
Sub sw22_Timer:vpmTimer.PulseSw 22:me.TimerEnabled = 0:End Sub 
Sub sw23_Hit:vpmTimer.PulseSw 23:End Sub  'Debug.Print "sw23":
Sub sw24_Hit:vpmTimer.PulseSw 24:End Sub  'Debug.Print "sw24":
Sub sw39_Hit:Controller.Switch(39) = 1:End Sub
Sub sw39_UnHit:Controller.Switch(39) = 0:End Sub
Sub sw40_Hit:Controller.Switch(40) = 1:End Sub
Sub sw40_UnHit:Controller.Switch(40) = 0:End Sub

'Bumpers
Sub sw48_bumper2_Hit : vpmTimer.PulseSw(48) : RandomSoundBumperTop sw48_bumper2: End Sub
Sub sw52_bumper1_Hit : vpmTimer.PulseSw(52) : RandomSoundBumperMiddle sw52_bumper1: End Sub
Sub sw53_bumper3_Hit : vpmTimer.PulseSw(53) : RandomSoundBumperBottom sw53_bumper3: End Sub

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep, LaStep, LaaStep, LbStep, LcStep, LccStep, LcccStep, RaStep, RbStep, RcStep, RccStep, RcccStep, ReStep, RfStep, RffStep, RgStep, RhStep, RiStep, RjStep, RkStep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 55
    RandomSoundSlingshotRight SLING1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -29
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -12
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 54
    RandomSoundSlingshotLeft SLING2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -31
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -16
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub WallLA_Hit
    RubberLA.Visible = 0
    RubberLA3.Visible = 1
    LAstep = 0
End Sub

Sub WallLAa_Hit
	vpmTimer.PulseSw 56
    RubberLA.Visible = 0
    RubberLA1.Visible = 1
    LAastep = 0
End Sub
 

Sub WallLb_Hit
    RubberLb.Visible = 0
    RubberLb1.Visible = 1
    Lbstep = 0
End Sub

Sub WallLc_Hit
    RubberLc.Visible = 0
    RubberLc1.Visible = 1
    Lcstep = 0
End Sub

Sub Walllcc_Hit
	vpmTimer.PulseSw 60
    RubberLc.Visible = 0
    RubberLc3.Visible = 1
    lccstep = 0
End Sub
 

Sub Walllccc_Hit
	vpmTimer.PulseSw 60
    RubberLc.Visible = 0
    RubberLc5.Visible = 1
    lcccstep = 0
End Sub
 

Sub Wallra_Hit
    Rubberra.Visible = 0
    Rubberra1.Visible = 1
    rastep = 0
End Sub

Sub Wallrb_Hit
	vpmTimer.PulseSw 59
    Rubberrb.Visible = 0
    Rubberrb1.Visible = 1
    rbstep = 0
End Sub

Sub Wallrc_Hit
    Rubberrc.Visible = 0
    Rubberrc1.Visible = 1
    rcstep = 0
End Sub

Sub Wallrcc_Hit
    Rubberrc.Visible = 0
    Rubberrc3.Visible = 1
    rccstep = 0
End Sub
 
Sub Wallrccc_Hit
    Rubberrc.Visible = 0
    Rubberrc5.Visible = 1
    rcccstep = 0
End Sub

Sub Wallre_Hit
    Rubberre.Visible = 0
    Rubberre1.Visible = 1
    restep = 0
End Sub

Sub Wallrf_Hit
    Rubberrf.Visible = 0
    Rubberrf1.Visible = 1
    rfstep = 0
End Sub

Sub Wallrg_Hit
    Rubberrg.Visible = 0
    Rubberrg1.Visible = 1
    rgstep = 0
End Sub

Sub Wallrh_Hit
    Rubberrh.Visible = 0
    Rubberrh1.Visible = 1
    rhstep = 0
End Sub
 

Sub Wallri_Hit
    Rubberri.Visible = 0
    Rubberri1.Visible = 1
    ristep = 0
End Sub

Sub Wallrj_Hit
    Rubberrj.Visible = 0
    Rubberrj1.Visible = 1
    rjstep = 0
End Sub
 

Sub Wallrk_Hit
    Rubberrk.Visible = 0
    Rubberrk1.Visible = 1
    rkstep = 0
End Sub


Sub WallTimer_Timer
	If lastep < 5 Then
		Select Case LAstep
			Case 3:RubberLA3.Visible = 0:RubberLA4.Visible = 1
			Case 4:RubberLA4.Visible = 0:RubberLA.Visible = 1
		End Select
		LAstep = LAstep + 1
	End If

	If laastep < 5 Then
		Select Case LAastep
			Case 3:RubberLA1.Visible = 0:RubberLA2.Visible = 1
			Case 4:RubberLA2.Visible = 0:RubberLA.Visible = 1
		End Select
		LAastep = LAastep + 1
	End If

	If lbstep < 5 Then
		Select Case Lbstep
			Case 3:RubberLb1.Visible = 0:RubberLb2.Visible = 1
			Case 4:RubberLb2.Visible = 0:RubberLb.Visible = 1
		End Select
		Lbstep = Lbstep + 1
	End If

	If lcstep < 5 Then
		Select Case Lcstep
			Case 3:RubberLc1.Visible = 0:RubberLc2.Visible = 1
			Case 4:RubberLc2.Visible = 0:RubberLc.Visible = 1
		End Select
		Lcstep = Lcstep + 1
	End If

	If lccstep < 5 Then
		Select Case lccstep
			Case 3:RubberLc3.Visible = 0:RubberLc4.Visible = 1
			Case 4:RubberLc4.Visible = 0:RubberLc.Visible = 1
		End Select
		lccstep = lccstep + 1
	End If

	If lcccstep < 5 Then
		Select Case lcccstep
			Case 3:RubberLc5.Visible = 0:RubberLc6.Visible = 1
			Case 4:RubberLc6.Visible = 0:RubberLc.Visible = 1
		End Select
		lcccstep = lcccstep + 1
	End If


	If rastep < 5 Then
		Select Case rastep
			Case 3:Rubberra1.Visible = 0:Rubberra2.Visible = 1
			Case 3:Rubberra2.Visible = 0:Rubberra3.Visible = 1
			Case 4:Rubberra3.Visible = 0:Rubberra.Visible = 1
		End Select
		rastep = rastep + 1
	End If

	If rbstep < 5 Then
		Select Case rbstep
			Case 3:Rubberrb1.Visible = 0:Rubberrb2.Visible = 1
			Case 3:Rubberrb2.Visible = 0:Rubberrb3.Visible = 1
			Case 4:Rubberrb3.Visible = 0:Rubberrb.Visible = 1
		End Select
		rbstep = rbstep + 1
	End If

	If rcstep < 5 Then
		Select Case rcstep
			Case 3:Rubberrc1.Visible = 0:Rubberrc2.Visible = 1
			Case 4:Rubberrc2.Visible = 0:Rubberrc.Visible = 1
		End Select
		rcstep = rcstep + 1
	End If

	If rccstep < 5 Then
		Select Case rccstep
			Case 3:Rubberrc3.Visible = 0:Rubberrc4.Visible = 1
			Case 4:Rubberrc4.Visible = 0:Rubberrc.Visible = 1
		End Select
		rccstep = rccstep + 1
	End If

	If rcccstep < 5 Then
		Select Case rcccstep
			Case 3:Rubberrc5.Visible = 0:Rubberrc6.Visible = 1
			Case 4:Rubberrc6.Visible = 0:Rubberrc.Visible = 1
		End Select
		rcccstep = rcccstep + 1
	End If

	If restep < 5 Then
		Select Case restep
			Case 3:Rubberre1.Visible = 0:Rubberre2.Visible = 1
			Case 3:Rubberre2.Visible = 0:Rubberre3.Visible = 1
			Case 4:Rubberre3.Visible = 0:Rubberre.Visible = 1
		End Select
		restep = restep + 1
	End If

	If rfstep < 5 Then
		Select Case rfstep
			Case 3:Rubberrf1.Visible = 0:Rubberrf2.Visible = 1
			Case 3:Rubberrf2.Visible = 0:Rubberrf3.Visible = 1
			Case 4:Rubberrf3.Visible = 0:Rubberrf.Visible = 1
		End Select
		rfstep = rfstep + 1
	End If

	If rgstep < 5 Then
		Select Case rgstep
			Case 3:Rubberrg1.Visible = 0:Rubberrg2.Visible = 1
			Case 4:Rubberrg2.Visible = 0:Rubberrg.Visible = 1
		End Select
		rgstep = rgstep + 1
	End If

	If rhstep < 5 Then
		Select Case rhstep
			Case 3:Rubberrh1.Visible = 0:Rubberrh2.Visible = 1
			Case 4:Rubberrh2.Visible = 0:Rubberrh.Visible = 1
		End Select
		rhstep = rhstep + 1
	End If


	If ristep < 5 Then
		Select Case ristep
			Case 3:Rubberri1.Visible = 0:Rubberri2.Visible = 1
			Case 4:Rubberri2.Visible = 0:Rubberri.Visible = 1
		End Select
		ristep = ristep + 1
	End If
	
	If rjstep < 5 Then
		Select Case rjstep
			Case 3:Rubberrj1.Visible = 0:Rubberrj2.Visible = 1
			Case 4:Rubberrj2.Visible = 0:Rubberrj.Visible = 1
		End Select
		rjstep = rjstep + 1
	End If

	If rkstep < 5 Then
		Select Case rkstep
			Case 3:Rubberrk1.Visible = 0:Rubberrk2.Visible = 1
			Case 4:Rubberrk2.Visible = 0:Rubberrk.Visible = 1
		End Select
		rkstep = rkstep + 1
	End If
End Sub

'***************************************************

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
FrameTimer.Interval = -1 'lamp fading speed
FrameTimer.Enabled = 1

' Called once per frame (16.6ms at 60hz)
Sub FrameTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
			 SetLamp ChgLamp(ii,0), ChgLamp(ii,1)
        Next
    End If
 	FadeLights.Update
	FlipperUpdate
	If DynamicBallShadowsOn=1 Then DynamicBSUpdate 'update ball shadows
End Sub

' Called every 1ms.
Sub OneMsec_Timer()
	FadeLights.Update1
End Sub
 
' div lamp subs

Sub InitLamps()
	'FadeLights.Callback(120)="RampCallback "
	FadeLights.obj(9) = array(l9, l9a, l9z)
	FadeLights.obj(10) = array(l10, l10a, l10z)
	FadeLights.obj(11) = array(l11, l11a, l11z)
	FadeLights.obj(12) = array(l12, l12a, l12z)
	FadeLights.obj(13) = array(l13, l13a, l13z)
	FadeLights.obj(14) = array(l14, l14a, l14z)
	FadeLights.obj(15) = array(l15, l15a, l15z)
	FadeLights.obj(16) = array(l16, l16a, l16z)
	FadeLights.obj(17) = array(l17, l17a, l17z)
	FadeLights.obj(18) = array(l18, l18a, l18z)
	FadeLights.obj(19) = array(l19, l19a, l19z)
	FadeLights.obj(20) = array(l20, l20a, l20z)
	FadeLights.obj(21) = array(l21, l21a, l21z)
	FadeLights.obj(22) = array(l22, l22a, l22z)
	FadeLights.obj(23) = array(l23, l23a, l23z)
	FadeLights.obj(24) = array(l24, l24a, l24z)
	FadeLights.obj(25) = array(l25, l25a, l25z)
	FadeLights.obj(26) = array(l26, l26a, l26z)
	FadeLights.obj(27) = array(l27, l27a, l27z)
	FadeLights.obj(28) = array(l28, l28a, l28z)
	FadeLights.obj(29) = array(l29, l29a, l29z)
	FadeLights.obj(30) = array(l30, l30a, l30z)
	FadeLights.obj(31) = array(l31, l31a, l31z)
	FadeLights.obj(32) = array(l32, l32a, l32z)
	FadeLights.obj(33) = array(l33, l33a, l33z)
	FadeLights.obj(34) = array(l34, l34a, l34z)
	FadeLights.obj(35) = array(l35, l35a, l35z)
	FadeLights.obj(36) = array(l36, l36a, l36z)
	FadeLights.obj(37) = array(l37, l37a, l37z)
	FadeLights.obj(38) = array(l38, l38a, l38z)
	FadeLights.obj(39) = array(l39, l39a, l39z)
	FadeLights.obj(40) = array(l40, l40a, l40z)
	FadeLights.obj(41) = array(l41, l41a, l41z)
	FadeLights.obj(42) = array(l42, l42a, l42z)
	FadeLights.obj(43) = array(l43, l43a, l43z)
	FadeLights.obj(44) = array(l44, l44a, l44z)
	FadeLights.obj(45) = array(l45, l45a, l45z)
	FadeLights.obj(46) = array(l46, l46a, l46z)
	FadeLights.obj(47) = array(l47, l47a, l47z)
	FadeLights.obj(48) = array(l48, l48a, l48z)
	FadeLights.obj(49) = array(l49, l49a, l49z,f49)
	FadeLights.obj(50) = array(l50, l50a, l50z)
	FadeLights.obj(51) = array(l51, l51a, l51z)
	FadeLights.obj(52) = array(l52, l52a, l52z)
	FadeLights.obj(53) = array(l53, l53a, l53z)
	FadeLights.obj(54) = array(l54, l54a, l54z)
	FadeLights.obj(55) = array(l55, l55a, l55z)
	FadeLights.obj(56) = array(l56, l56a, l56z)
	FadeLights.obj(57) = array(l57, l57a, l57z,f57)
'	FadeLights.Callback(57) = "UpdateLamp57"
	FadeLights.obj(58) = array(l58, l58a, l58z)
	'FadeLights.obj(59) = array(l59, l59a, l59z)
	FadeLights.obj(60) = array(l60, l60a, l60z)
	FadeLights.obj(61) = array(l61, l61a, l61z)
	FadeLights.obj(62) = array(l62, l62a, l62z)
	FadeLights.obj(63) = array(l63, l63a, l63z)
	FadeLights.obj(64) = array(l64, l64a, l64z)
	FadeLights.obj(101) = array(flash101) 'right eye flash
	FadeLights.obj(104) = array(flash104) 'left eye flash
	FadeLights.obj(105) = array(Flasher2TL,Flasher2TL1,Flasher2TL2,Flasher2TL3,Flasher2TR,Flasher2TR1,Flasher2TR2,Flasher2TR3)
	FadeLights.obj(106) = array(Flasher1L,Flasher1L1,FLasher1L2,Flasher1R,Flasher1R1,FLasher1R2)
	FadeLights.obj(107) = array(l107, l107a, l107z,BumpFlash,FlasherB)
	FadeLights.obj(108) = array(Flasher2L,Flasher2L1,Flasher2L2,Flasher2L3,Flasher2L4,Flasher2L5,Flasher2L6)
	FadeLights.obj(109) = array(l109, l109a, l109z)
'	FadeLights.Callback(110) = "UpdateGIFade"
'	FadeLights.Callback(111) = "UpdateGIFade"
	FadeLights.obj(110) = array(shadowson)
	FadeLights.obj(111) = array(shadowsoff)
End Sub

Sub AllLampsOff
    Dim x
    For x = 0 to 200
        SetLamp x, 0
    Next
End Sub

'Sub UpdateGIFade(lvl)
'	dim EBLight:EBLight = FadeLights.Lvl(57)
'	if lvl > 0 Then
'		dim guidestep:guidestep = CInt(4*lvl) ' 5 intensity steps 0-4
'		if guidestep = 4 Then ' GI is at maximum brightness
'			if EBlight > 0 Then 
'				outerinner_prim.image="woodguideonEB" 'giison, EB is on.
''				metalramp_prim.image="metalrampeb"
'			else
'				outerinner_prim.image="woodguideon" 'giison
''				metalramp_prim.image="metalrampgion"
'			end if 
'		Else
'			outerinner_prim.image="woodguideon" & CStr(4-guidestep)
'		end if 
'	else
'		if EBLight > 0  Then
'			outerinner_prim.image="woodguideoffEB" 'gi is off, EB is on.
'		Else
'			outerinner_prim.image="woodguideoff" 'gi is off. EB is off.
'		end if
'	end if
'	'debug.print Cstr(lvl) & " " & outerinner_prim.image
'End Sub
'
'Sub UpdateLamp57(lvl)
'	' If light 57 (EB) changes we need to recalculate outerinner_prim which is handled by GI fading.   Just request GI fade update.
'	UpdateGIFade(FadeLights.Lvl(110))
'End Sub

Sub UpdateLamp101
	if FadeLights.state(101) <> 0 Then
		if FadeLights.state(104) <> 0 Then
			mask_prim.image="maskon"
		else
			mask_prim.image="maskrighton" '101 is on, 104 is off
		end if
	else
		if FadeLights.state(104) <> 0 Then
			mask_prim.image="masklefton"
		else
			mask_prim.image="maskoff"
		end if
	end if
End Sub

Sub UpdateLamp104
	if FadeLights.state(104) <> 0 Then
		if FadeLights.state(101) <> 0 Then
			mask_prim.image="maskon"
		else
			mask_prim.image="masklefton" '104 is on, 101 is off
		end if
	else
		if FadeLights.state(101) <> 0 Then
			mask_prim.image="maskrighton"
		else
			mask_prim.image="maskoff"
		end if
	end if
End Sub

Sub UpdateLamp105
	if FadeLights.state(105) <> 0 Then
		if GiIsOff Then
			if FadeLights.state(106) <> 0 Then
				outerr.image="outerrightoffflash"
				If VRRoom = 1 Then outerrVR.image="outerrightoffflash" End If
			Else
				outerr.image="outerrightoffflash3"
				If VRRoom = 1 Then outerrVR.image="outerrightoffflash3" End If
			end if
			if FadeLights.state(108) <> 0 Then
				ramp_prim.image="rampoffflash2-3"
			Else
				ramp_prim.image="rampoffflash3" 'giisoff, flasher is on (lamp 105)
			end if
			Flash3_l_prim.image="flasher3longion" 'giisoff, flasher is on (lamp 105) (same texture as when gi is on)
			Flash3_l_prim.blenddisablelighting=1
			Flash3_r_prim.image="flasher3rongion" 'giisoff, flasher is on (lamp 105) (same texture as when gi is on)
			Flash3_r_prim.blenddisablelighting=1			
			screws3.image="screws3offflash3"
			outerb.image="outerbackoffflash3"
			outerbmetal.image="outerbackmetalonflash3"
		else
			if FadeLights.state(106) <> 0 Then
				outerr.image="outerrightonflash"
				If VRRoom = 1 Then outerrVR.image="outerrightonflash" End If
			Else
				outerr.image="outerrightonflash3"
				If VRRoom = 1 Then outerrVR.image="outerrightonflash3" End If
			end if
			if FadeLights.state(108) <> 0 Then
				ramp_prim.image="rampflash2-3"
			Else
				ramp_prim.image="rampflash3" 'giisoff, flasher is on (lamp 105)
			end if
			Flash3_l_prim.image="flasher3longion" 'giison, flasher is on (lamp 105)
			Flash3_l_prim.blenddisablelighting=1
			Flash3_r_prim.image="flasher3rongion" 'giison, flasher is on (lamp 105)
			Flash3_r_prim.blenddisablelighting=1	
			screws3.image="screws3onflash3"
			outerr.image="outerrightonflash3"
			If VRRoom = 1 Then outerrVR.image="outerrightonflash3" End If
			outerb.image="outerbackonflash3"
			outerbmetal.image="outerbackmetalonflash3"
		end if 
	else
		if GiIsOff Then
			Flash3_l_prim.image="flasher3loffgioff"  'giisoff, flasher is off
			Flash3_l_prim.blenddisablelighting=.3
			Flash3_r_prim.image="flasher3rgioff"  'giisoff, flasher is off
			Flash3_r_prim.blenddisablelighting=.3
			ramp_prim.image="rampgioff" 'giisoff, flasher is off
			screws3.image="screws3off"
			outerr.image="outerrightoff"
			If VRRoom = 1 Then outerrVR.image="outerrightoff" End If
			outerb.image="outerbackoff"
			outerbmetal.image="outerbackmetal"
		Else
			Flash3_l_prim.image="flasher3loffgion" 'giison, flasher is off
			Flash3_l_prim.blenddisablelighting=.3
			Flash3_r_prim.image="flasher3rgioff"  'giisn, flasher is off
			Flash3_r_prim.blenddisablelighting=.3
			ramp_prim.image="rampgion" 'giison, flasher is off
			screws3.image="screws3on"
			outerr.image="outerrighton"
			If VRRoom = 1 Then outerrVR.image="outerrighton" End If
			outerb.image="outerbackon"
			outerbmetal.image="outerbackmetal"
		end if
	end if
End Sub

Sub UpdateLamp106
	if FadeLights.state(106) <> 0 Then
		if GiIsOff Then
			if FadeLights.state(105) <> 0 Then
				outerr.image="outerrightoffflash"
				If VRRoom = 1 Then outerrVR.image="outerrightoffflash" End If
			Else
				outerr.image="outerrightoffflash1"
				If VRRoom = 1 Then outerrVR.image="outerrightoffflash1" End If
			end if
			Flash1l_prim.image="flasher1lon" 'giisoff, flasher is on (lamp 106) (same texture as when gi is on)
			Flash1l_prim.blenddisablelighting=1		
			plasticleftflashlow.image="leftflashplasticoffflash1" 'giisoff, flasher is on (lamp 106)
			Flash1r_prim.image="flasher1ron" 'giisoff, flasher is on (lamp 106) (same texture as when gi is on)
			Flash1r_prim.blenddisablelighting=1			
			plasticrightflash.image="rightflasherplasticoffflash" 'giisoff, flasher is on (lamp 106)
			screws1.image="screws1offflash1"
			acorns.image="acornsoffflash1"
			metalramp_prim.image="metalrampflash"
			outerl.image="outerleftoffflash1"
			If VRRoom = 1 Then outerlVR.image="outerleftoffflash1" End If
		else
			if FadeLights.state(105) <> 0 Then
				outerr.image="outerrightonflash"
				If VRRoom = 1 Then outerrVR.image="outerrightonflash" End If
			Else
				outerr.image="outerrightonflash1"
				If VRRoom = 1 Then outerrVR.image="outerrightonflash1" End If
			end if
			Flash1l_prim.image="flasher1lon" 'giison, flasher is on (lamp 106)
			Flash1l_prim.blenddisablelighting=1	
			plasticleftflashlow.image="leftflashplasticonflash1" 'giison, flasher is n (lamp 106)
			Flash1r_prim.image="flasher1ron" 'giison, flasher is on (lamp 106)
			Flash1r_prim.blenddisablelighting=1	
			plasticrightflash.image="rightflasherplasticonflash" 'giison, flasher is n (lamp 106)
			screws1.image="screws1onflash1"
			acorns.image="acornsonflash1"
			metalramp_prim.image="metalrampflash"
			outerl.image="outerleftonflash1"
			If VRRoom = 1 Then outerlVR.image="outerleftoffflash1" End If
		end if 
	else
		if GiIsOff Then
			Flash1l_prim.image="flasher1loffgioff"  'giisoff, flasher is off
			Flash1l_prim.blenddisablelighting=.3
			plasticleftflashlow.image="leftflashplasticoff" 'giisoff, flasher is off
			Flash1r_prim.image="flasher1roffgioff"  'giisoff, flasher is off
			Flash1r_prim.blenddisablelighting=.3
			plasticrightflash.image="rightflasherplasticoff" 'giisoff, flasher is off
			screws1.image="screws1off"
			acorns.image="acornsoff"
			metalramp_prim.image="metalrampgioff"
			outerr.image="outerrightoff"
			outerl.image="outerleftoff"
			If VRRoom = 1 Then 
				outerlVR.image="outerleftoff" 
				outerrVR.image="outerrightoff"
				outerlVR1.image="outerleftoffcut"
			End If
		Else
			Flash1l_prim.image="flasher1loffgion" 'giison, flasher is off
			Flash1l_prim.blenddisablelighting=.3
			plasticleftflashlow.image="leftflashplasticon" 'giison, flasher is off
			Flash1r_prim.image="flasher1roffgion" 'giison, flasher is off
			Flash1r_prim.blenddisablelighting=.3
			plasticrightflash.image="rightflasherplasticon" 'giison, flasher is off
			screws1.image="screws1on"
			acorns.image="acornson"
			metalramp_prim.image="metalrampgion"
			outerr.image="outerrighton"
			outerl.image="outerlefton"
			If VRRoom = 1 Then 
				outerlVR.image="outerlefton" 
				outerrVR.image="outerrighton"
				outerlVR1.image="outerleftoncut"
			End If
		end if
	end if
End Sub

Sub UpdateLamp107
	if FadeLights.state(107) <> 0 Then
		outerr.image="outerrightonbump"
		If VRRoom = 1 Then outerrVR.image="outerrightonbump" End If
	else
		outerr.image="outerrighton"
		If VRRoom = 1 Then outerrVR.image="outerrighton" End If
	end if
End Sub

Sub UpdateLamp108
	if FadeLights.state(108) <> 0 Then
		if GiIsOff Then
			if FadeLights.state(105) <> 0 Then
				ramp_prim.image="rampoffflash2-3"
			Else
				ramp_prim.image="rampoffflash2" 'giisoff, flasher is on (lamp 108)
			end if
			Flash2_b_prim.image="flasher2bon" 'giisoff, flasher is on (lamp 108) (same texture as when gi is on)
			Flash2_b_prim.blenddisablelighting=1
			Flash2_t_prim.image="flasher2on" 'giisoff, flasher is on (lamp 108) (same texture as when gi is on)
			Flash2_t_prim.blenddisablelighting=1		
			plasticleftflashlow.image="leftflashplasticoffflash2" 'giisoff, flasher is on (lamp 108)
			plasticleftflashhigh.image="plasticflash2offflash" 'giisoff, flasher is on (lamp 108)
			screws2.image="screws2offflash2"
			outerl.image="outerleftoffflash2"
			If VRRoom = 1 Then outerlVR.image="outerleftoffflash2" End If
			outerb.image="outerbackoffflash2"
			outerbmetal.image="outerbackmetalonflash2"
		else
			if FadeLights.state(105) <> 0 Then
				ramp_prim.image="rampflash2-3"
			Else
				ramp_prim.image="rampflash2" 'giisoff, flasher is on (lamp 108)
			end if
			Flash2_b_prim.image="flasher2bon" 'giison, flasher is on (lamp 108)
			Flash2_b_prim.blenddisablelighting=1
			Flash2_t_prim.image="flasher2on" 'giison, flasher is on (lamp 108) (same texture as when gi is on)	
			Flash2_t_prim.blenddisablelighting=1
			plasticleftflashlow.image="leftflashplasticonflash2" 'giison, flasher is n (lamp 108)
			plasticleftflashhigh.image="plasticflash2onflash" 'giison, flasher is n (lamp 108)
			screws2.image="screws2onflash2"
			outerl.image="outerleftonflash2"
			If VRRoom = 1 Then outerlVR.image="outerleftonflash2" End If
			outerb.image="outerbackonflash2"
			outerbmetal.image="outerbackmetalonflash2"
		end if 
	else
		if GiIsOff Then
			Flash2_b_prim.image="flasher2boffgioff"  'giisoff, flasher is off
			Flash2_b_prim.blenddisablelighting=.3
			Flash2_t_prim.image="flasher2offgioff"  'giisoff, flasher is off
			Flash2_t_prim.blenddisablelighting=.3
			plasticleftflashlow.image="leftflashplasticoff" 'giisoff, flasher is off
			plasticleftflashhigh.image="plasticflash2off" 'giisoff, flasher is off
			ramp_prim.image="rampgioff" 'giisoff, flasher is off
			screws2.image="screws2off"
			outerl.image="outerleftoff"
			If VRRoom = 1 Then 
				outerlVR.image="outerleftoff" 
				outerlVR1.image="outerleftoffcut"
			End If
			outerb.image="outerbackoff"
			outerbmetal.image="outerbackmetal"
		Else
			Flash2_b_prim.image="flasher2boffgion" 'giison, flasher is off
			Flash2_b_prim.blenddisablelighting=.3
			Flash2_t_prim.image="flasher2offgion" 'giison, flasher is off
			Flash2_t_prim.blenddisablelighting=.3
			plasticleftflashlow.image="leftflashplasticon" 'giison, flasher is off
			plasticleftflashhigh.image="plasticflash2on"
			ramp_prim.image="rampgion" 'giison, flasher is off
			screws2.image="screws2on"
			outerl.image="outerlefton"
			If VRRoom = 1 Then 
				outerlVR.image="outerlefton" 
				outerlVR1.image="outerleftoncut"
			End If
			outerb.image="outerbackon"
			outerbmetal.image="outerbackmetal"
		end if
	end if
End Sub

Sub SetLamp(nr, value)
	' If the lamp state is not changing, just exit. 
	if FadeLights.state(nr) = value then exit sub

	FadeLights.state(nr) = value
	if nr = 101 then UpdateLamp101
	if nr = 104 then UpdateLamp104
	if nr = 105 then UpdateLamp105
	if nr = 106 then UpdateLamp106
	if nr = 107 then UpdateLamp107
	if nr = 108 then UpdateLamp108
End Sub



' *** NFozzy's lamp fade routines *** 


Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class	'todo do better

Class LampFader
	Public FadeSpeedDown(140), FadeSpeedUp(140)
	Private Lock(140), Loaded(140), OnOff(140)
	Public UseFunction
	Private cFilter
	Private UseCallback(140), cCallback(140)
	Public Lvl(140), Obj(140)

	Sub Class_Initialize()
		dim x : for x = 0 to uBound(OnOff) 	'Set up fade speeds
			if FadeSpeedDown(x) <= 0 then FadeSpeedDown(x) = 1/100	'fade speed down
			if FadeSpeedUp(x) <= 0 then FadeSpeedUp(x) = 1/80'Fade speed up
			UseFunction = False
			lvl(x) = 0
			OnOff(x) = False
			Lock(x) = True : Loaded(x) = False
		Next

		for x = 0 to uBound(OnOff) 		'clear out empty obj
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		'debug.print Lampz.Locked(100)	'debug
	Public Property Get state(idx) : state = OnOff(idx) : end Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property

	Public Property Let state(ByVal idx, input) 'Major update path
		input = cBool(input)
		if OnOff(idx) = Input then : Exit Property : End If	'discard redundant updates
		OnOff(idx) = input
		Lock(idx) = False 
		Loaded(idx) = False
	End Property

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then 
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x) : debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx) : debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				
			end if
		Next
		'debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Update1()	 'Handle all boolean numeric fading. If done fading, Lock(True). Update on a '1' interval Timer!
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) > 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					if Lvl(x) < 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
	End Sub


	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx : for x = 0 to uBound(OnOff)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then 
						for each xx in obj(x) : xx.IntensityScale = cFilter(Lvl(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then 
						obj(x).Intensityscale = cFilter(Lvl(x))
					Else
						obj(x).Intensityscale = Lvl(x)
					End If
				end if
				' Sleazy hack for regional decimal point problem
				If UseCallBack(x) then execute cCallback(x) & " CSng(" & CInt(10000 * Lvl(x)) & " / 10000)"	'Callback
				If Lock(x) Then
					if Lvl(x) = 1 or Lvl(x) = 0 then Loaded(x) = True	'finished fading
				end if
			end if
		Next
	End Sub
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

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
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
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
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


'******************************************************
'                BALL ROLLING AND DROP SOUNDS
'******************************************************

Const tnob = 5 ' total number of balls
ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

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
		StopSound("BallRoll_" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(BOT)
		If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
				rolling(b) = True
				PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
				StopSound("fx_Rolling_Plastic" & b)
				StopSound("fx_Rolling_Metal" & b)
		' ***Ball on METAL ramp*** 
		ElseIf BOT(b).z > 90 and InRect(BOT(b).x, BOT(b).y, 700, 1043, 765,1043, 765,1317, 700,1317) Then 
			PlaySound("fx_Rolling_Metal" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
			rolling(b) = True
			StopSound("BallRoll_" & b)
			StopSound("fx_Rolling_Plastic" & b)
	' ***Ball on PLASTIC ramp***
		Elseif  BOT(b).z > 100 and InRect(BOT(b).x, BOT(b).y, 590,395,940,395,940, 951, 670, 1095) Then 
			PlaySound("fx_Rolling_Plastic" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b)) 
			rolling(b) = True
			StopSound("BallRoll_" & b)
			StopSound("fx_Rolling_Metal" & b)
		Elseif  BOT(b).z > 30 and InRect(BOT(b).x, BOT(b).y, 43, 523, 130, 500,213, 745, 118, 780) Then 
			PlaySound("fx_Rolling_Plastic" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
			rolling(b) = True
			StopSound("fx_Rolling_Metal" & b)
		Elseif  BOT(b).z > 90 and InRect(BOT(b).x, BOT(b).y, 0,0,928,0,928,400,0,520) Then 
			PlaySound("fx_Rolling_Plastic" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b)) 
			StopSound("BallRoll_" & b)
			rolling(b) = True
			StopSound("fx_Rolling_Metal" & b)
		Elseif  BOT(b).z > 30 and InRect(BOT(b).x, BOT(b).y, 566,51,853,45,861,381,573,384) Then 
			PlaySound("fx_Rolling_Plastic" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
			rolling(b) = True
			StopSound("BallRoll_" & b)
			StopSound("fx_Rolling_Metal" & b)

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				StopSound("fx_Rolling_Plastic" & b)
				StopSound("fx_Rolling_Metal" & b)
				rolling(b) = False
			End If
		End If

		if BOT(b).z > 120 and InRect(BOT(b).x, BOT(b).y, 634,234,690,206,711,247,669,295) Then
			if BOT(b).vely < 0 and ballvel(BOT(b)) < 1 Then
				BOT(b).vely = BOT(b).vely * 2
				if BOT(b).velx > 0 then BOT(b).velx = -0.1
				'debug.print ballvel(BOT(b))
			end if
		elseif BOT(b).z > 120 and InRect(BOT(b).x, BOT(b).y, 645,162,708,172,690,206,634,234) Then
			if BOT(b).vely < 0 and ballvel(BOT(b)) < 1 Then
				BOT(b).vely = BOT(b).vely * 2
				if BOT(b).velx > 0 then BOT(b).velx = -0.1
			end if
		elseif BOT(b).z > 120 and InRect(BOT(b).x, BOT(b).y, 697,120,744,164,708,172,645,162) Then
			if BOT(b).velx > 0 and ballvel(BOT(b)) < 1 Then
				BOT(b).velx = BOT(b).velx * 2
				if BOT(b).vely > 0 then BOT(b).vely = -0.1
			end if
		end if

		'***Ball Drop Sounds***
		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz > -7 Then
						RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
						RandomSoundBallBouncePlayfieldHard BOT(b)
				End If                                
			End If
		End If
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If
	Next
End Sub



'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperUpdate()
	batleftshadow.rotz = LeftFlipper.CurrentAngle
	batrightshadow.rotz  = RightFlipper.CurrentAngle
	plungegate_prim.RotX = Gate4.CurrentAngle + 90
End Sub

'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

' The "DynamicBSUpdate" sub should be called with an interval of -1 (framerate)
' Place a toggleable variable (DynamicBallShadowsOn) in user options at the top of the script
' Import the "bsrtx7" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#, with at least as many objects each as there can be balls
'
' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
'***These must be organized in order, so that lights that intersect on the table are adjacent in the collection***
' This is because the code will only project two shadows if they are coming from lights that are consecutive in the collection
' The easiest way to keep track of this is to start with the group on the left slingshot and move clockwise around the table
'	For example, if you use 6 lights: A & B on the left slingshot and C & D on the right, with E near A&B and F next to C&D, your collection would look like EBACDF
'
'																E
'	A		 C													B
'	 B		D			your collection should look like		A		because E&B, B&A, etc. intersect; but B&D or E&F do not
'  E		  F													C
'																D
'																F
'
'Update shadow options in the code to fit your table and preference

'****** End Instructions ******

' *** Example timer sub

' The frame timer interval is -1, so executes at the display frame rate
'Sub FrameTimer_Timer()
'	If DynamicBallShadowsOn=1 Then DynamicBSUpdate 'update ball shadows
'End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary

'Const tnob = 10 ' total number of balls
Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done

' *** Example "Top of Script" User Option
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow, 1 = enable dynamic ball shadow

' *** Shadow Options ***
Const fovY					= -2	'Offset y position under ball to account for layback or inclination (more pronounced need further back, -2 seems best for alignment at slings)
Const DynamicBSFactor 		= 1.0	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.9	'0 to 1, higher is darker
Const Wideness				= 20	'Sets how wide the shadows can get (20 +5 thinness should be most realistic)
Const Thinness				= 5		'Sets minimum as ball moves away from source
' ***				 ***

Dim sourcenames, currentShadowCount

sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)


dim objrtx1(20), objrtx2(20)
dim objBallShadow(20)
Dim BallShadowA
BallShadowA = Array (BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5,BallShadowA6,BallShadowA7,BallShadowA8,BallShadowA9,BallShadowA10)

DynamicBSInit

sub DynamicBSInit()
	Dim iii

	for iii = 0 to tnob									'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = iii/1000 + 0.01
		objrtx1(iii).visible = 0
		'objrtx1(iii).uservalue=0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = (iii)/1000 + 0.02
		objrtx2(iii).visible = 0
		'objrtx2(iii).uservalue=0
		currentShadowCount(iii) = 0
		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		objBallShadow(iii).Z = iii/1000 + 0.04
	Next
end sub


Sub DynamicBSUpdate
	Dim falloff:	falloff = 150			'Max distance to light sources, can be changed if you have a reason
	Const AmbientShadowOn = 1				'Toggle for just the moving shadow primitive (ninuzzu's)
	Dim ShadowOpacity, ShadowOpacity2 
	Dim s, Source, LSd, b, currentMat, AnotherSource, BOT
	BOT = GetBalls

	'Hide shadow of deleted balls
	For s = UBound(BOT) + 1 to tnob
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
	Next

	If UBound(BOT) = lob - 1 Then Exit Sub		'No balls in play, exit

'The Magic happens here
	For s = lob to UBound(BOT)

' *** Normal "ambient light" ball shadow
		If AmbientShadowOn = 1 Then
			If BOT(s).X < tablewidth/2 Then
				objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/13)) + 5
			Else
				objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/13)) - 5
			End If
			objBallShadow(s).Y = BOT(s).Y + fovY

			If BOT(s).Z < 30 Then 'or BOT(s).Z > 105 Then		'Defining when (height-wise) you want ambient shadows
				objBallShadow(s).visible = 1
				BallShadowA(s).visible = 0
	'			objBallShadow(s).Z = BOT(s).Z - 25 + s/1000 + 0.04		'Uncomment if you want to add shadows to an upper/lower pf
			Else
	'			objBallShadow(s).visible = 0
				BallShadowA(s).X = BOT(s).X			'Flasher shadows for ramps
				ballShadowA(s).Y = BOT(s).Y + 12.5 + fovY
				BallShadowA(s).height=BOT(s).z - 12.5
				BallShadowA(s).visible = 1
			end if
		End If
' *** Dynamic shadows
		For Each Source in DynamicSources
			LSd=DistanceFast((BOT(s).x-Source.x),(BOT(s).y-Source.y))	'Calculating the Linear distance to the Source
			If BOT(s).Z < 30 Then 'Or BOT(s).Z > 105 Then				'Defining when (height-wise) you want dynamic shadows
				If LSd < falloff and Source.state=1 Then	    		'If the ball is within the falloff range of a light and light is on
					currentShadowCount(s) = currentShadowCount(s) + 1	'Within range of 1 or 2
					if currentShadowCount(s) = 1 Then					'1 dynamic shadow source
						sourcenames(s) = source.name
						currentMat = objrtx1(s).material
						objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
						objrtx1(s).rotz = AnglePP(Source.x, Source.y, BOT(s).X, BOT(s).Y) + 90
						ShadowOpacity = (falloff-LSd)/falloff									'Sets opacity/darkness of shadow by distance to light
						objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness						'Scales shape of shadow with distance/opacity
						UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0
						'debug.print "update1" & source.name & " at:" & ShadowOpacity

						currentMat = objBallShadow(s).material
						UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-ShadowOpacity),RGB(0,0,0),0,0,False,True,0,0,0,0

					Elseif currentShadowCount(s) = 2 Then
																'Same logic as 1 shadow, but twice
						currentMat = objrtx1(s).material
						set AnotherSource = Eval(sourcenames(s))
						objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
						objrtx1(s).rotz = AnglePP(AnotherSource.x, AnotherSource.y, BOT(s).X, BOT(s).Y) + 90
						ShadowOpacity = (falloff-(((BOT(s).x-AnotherSource.x)^2+(BOT(s).y-AnotherSource.y)^2)^0.5))/falloff
						objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness
						UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

						currentMat = objrtx2(s).material
						objrtx2(s).visible = 1 : objrtx2(s).X = BOT(s).X : objrtx2(s).Y = BOT(s).Y + fovY
'						objrtx2(s).Z = BOT(s).Z - 25 + s/1000 + 0.02							'Uncomment if you want to add shadows to an upper/lower pf
						objrtx2(s).rotz = AnglePP(Source.x, Source.y, BOT(s).X, BOT(s).Y) + 90
						ShadowOpacity2 = (falloff-LSd)/falloff
						objrtx2(s).size_y = Wideness*ShadowOpacity2+Thinness
						UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
						'debug.print "update2: " & source.name & " at:" & ShadowOpacity & " and "  & Eval(sourcenames(s)).name & " at:" & ShadowOpacity2

						currentMat = objBallShadow(s).material
						UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
					end if
				Else
					currentShadowCount(s) = 0
				End If
			Else									'Hide dynamic shadows everywhere else
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		Next
	Next
End Sub


Function DistanceFast(x, y)
	dim ratio, ax, ay
	'Get absolute value of each vector
	ax = abs(x)
	ay = abs(y)
	'Create a ratio
	ratio = 1 / max(ax, ay)
	ratio = ratio * (1.29289 - (ax + ay) * ratio * 0.29289)
	if ratio > 0 then
		DistanceFast = 1/ratio
	Else
		DistanceFast = 0
	End if
end Function

Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function
							'Enable these functions if they are not already present elswhere in your table
Dim PI: PI = 4*Atn(1)

'Function Atn2(dy, dx)
'	If dx > 0 Then
'		Atn2 = Atn(dy / dx)
'	ElseIf dx < 0 Then
'		If dy = 0 Then 
'			Atn2 = pi
'		Else
'			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
'		end if
'	ElseIf dx = 0 Then
'		if dy = 0 Then
'			Atn2 = 0
'		else
'			Atn2 = Sgn(dy) * pi / 2
'		end if
'	End If
'End Function
'
Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************



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

'
'Sub Pins_Hit (idx)
'	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
'End Sub
'
'Sub Targets_Hit (idx)
'	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
'End Sub
'
'Sub Metals_Thin_Hit (idx)
'	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'End Sub
'
'Sub Metals_Medium_Hit (idx)
'	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'End Sub
'
'Sub Metals2_Hit (idx)
'	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'End Sub

'Sub Gates_Hit (idx)
'	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

'Sub Rubbers_Hit(idx)
' 	dim finalspeed
'  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
' 	If finalspeed > 20 then 
'		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'	End if
'	If finalspeed >= 6 AND finalspeed <= 20 then
' 		RandomSoundRubber()
' 	End If
'End Sub

'Sub Posts_Hit(idx)
' 	dim finalspeed
'  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
' 	If finalspeed > 16 then 
'		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'	End if
'	If finalspeed >= 6 AND finalspeed <= 16 then
' 		RandomSoundRubber()
' 	End If
'End Sub

'Sub RandomSoundRubber()
'	Select Case Int(Rnd*3)+1
'		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'	End Select
'End Sub
'
'Sub RandomSoundFlipper()
'	Select Case Int(Rnd*3)+1
'		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
'	End Select
'End Sub

Sub Table1_Exit()
	
End Sub

Sub Table1_MusicDone()
	
End Sub


' **** Replacement mech class that moves smoothly.   Carbon copy of the cvpmMech
Class cvpmMyMech
	Public Sol1, Sol2, MType, Length, Steps, Acc, Ret
	Private mMechNo, mNextSw, mSw(), mLastPos, mLastSpeed, mCallback

	Private Sub Class_Initialize
		ReDim mSw(10)
		gNextMechNo = gNextMechNo + 1 : mMechNo = gNextMechNo : mNextSw = 0 : mLastPos = 0 : mLastSpeed = 0
		MType = 0 : Length = 0 : Steps = 0 : Acc = 0 : Ret = 0 : vpmTimer.addResetObj Me
	End Sub

	Public Sub AddSw(aSwNo, aStart, aEnd)
		mSw(mNextSw) = Array(aSwNo, aStart, aEnd, 0)
		mNextSw = mNextSw + 1
	End Sub

	Public Sub AddPulseSwNew(aSwNo, aInterval, aStart, aEnd)
		If Controller.Version >= "01200000" Then
			mSw(mNextSw) = Array(aSwNo, aStart, aEnd, aInterval)
		Else
			mSw(mNextSw) = Array(aSwNo, -aInterval, aEnd - aStart + 1, 0)
		End If
		mNextSw = mNextSw + 1
	End Sub

	Public Sub Start
		Dim sw, ii
		With Controller
			.Mech(1) = Sol1 : .Mech(2) = Sol2 : .Mech(3) = Length
			.Mech(4) = Steps : .Mech(5) = MType : .Mech(6) = Acc : .Mech(7) = Ret
			ii = 10
			For Each sw In mSw
				If IsArray(sw) Then
					.Mech(ii) = sw(0) : .Mech(ii+1) = sw(1)
					.Mech(ii+2) = sw(2) : .Mech(ii+3) = sw(3)
					ii = ii + 10
				End If
			Next
			.Mech(0) = mMechNo
		End With
		If IsObject(mCallback) Then mCallBack 0, 0, 0 : mLastPos = 0 : vpmTimer.EnableUpdate Me, True, True  ' <------- All for this.
	End Sub

	Public Property Get Position : Position = Controller.GetMech(mMechNo) : End Property
	Public Property Get Speed    : Speed = Controller.GetMech(-mMechNo)   : End Property
	Public Property Let Callback(aCallBack) : Set mCallback = aCallBack : End Property

	Public Sub Update
		Dim currPos, speed
		currPos = Controller.GetMech(mMechNo)
		speed = Controller.GetMech(-mMechNo)
		If currPos < 0 Or (mLastPos = currPos And mLastSpeed = speed) Then Exit Sub
		mCallBack currPos, speed, mLastPos : mLastPos = currPos : mLastSpeed = speed
	End Sub

	Public Sub Reset : Start : End Sub
	
End Class

Dim EnableBallControl
EnableBallControl = false 'Change to true to enable manual ball control (or press C in-game) via the arrow keys and B (boost movement) keys

'*****************************************
'   rothbauerw's Manual Ball Control
'*****************************************

Dim BCup, BCdown, BCleft, BCright
Dim ControlBallInPlay, ControlActiveBall
Dim BCvel, BCyveloffset, BCboostmulti, BCboost

BCboost = 1				'Do Not Change - default setting
BCvel = 4				'Controls the speed of the ball movement
BCyveloffset = -0.01 	'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
BCboostmulti = 3		'Boost multiplier to ball veloctiy (toggled with the B key) 

ControlBallInPlay = false

Sub StartBallControl_Hit()
	Set ControlActiveBall = ActiveBall
	ControlBallInPlay = true
End Sub

Sub StopBallControl_Hit()
	ControlBallInPlay = false
End Sub	

Sub BallControlTimer_Timer()
	If EnableBallControl and ControlBallInPlay then
		If BCright = 1 Then
			ControlActiveBall.velx =  BCvel*BCboost
		ElseIf BCleft = 1 Then
			ControlActiveBall.velx = -BCvel*BCboost
		Else
			ControlActiveBall.velx = 0
		End If

		If BCup = 1 Then
			ControlActiveBall.vely = -BCvel*BCboost
		ElseIf BCdown = 1 Then
			ControlActiveBall.vely =  BCvel*BCboost
		Else
			ControlActiveBall.vely = bcyveloffset
		End If
	End If
End Sub


' ***************************************************************************
'                                  LAMP CALLBACK
' ****************************************************************************

Set LampCallback = GetRef("UpdateMultipleLamps")

Sub UpdateMultipleLamps()

	'if L25.state = 1 then: (code here): end if

	'or
	IF VRRoom = 1 Then
		If Controller.Lamp(1) = 0 Then: FlGOver.visible=0: else: FlGOver.visible=1
		If Controller.Lamp(2) = 0 Then: FlMatch.visible=0: else: FlMatch.visible=1
		If Controller.Lamp(3) = 0 Then: FlBiP.visible=0: else: FlBiP.visible=1
	

		If Controller.Lamp(4) = 0 Then: FlClr1.visible=0: else: FlClr1.visible=1
		If Controller.Lamp(5) = 0 Then: FlClr2.visible=0: else: FlClr2.visible=1
		If Controller.Lamp(6) = 0 Then: FlClr3.visible=0: else: FlClr3.visible=1
		If Controller.Lamp(7) = 0 Then: FlClr4.visible=0: else: FlClr4.visible=1
		If Controller.Lamp(8) = 0 Then: FlClr5.visible=0: else: FlClr5.visible=1
'		If Controller.Lamp(15) = 0 Then: Flashertopper2.visible=0: else: Flashertopper2.visible=1
'		If Controller.Lamp(16) = 0 Then: Flashertopper3.visible=0: else: Flashertopper3.visible=1


		If Controller.Lamp(51) = 0 Then: FlBG13.visible=0: else: FlBG13.visible=1
		If Controller.Lamp(34) = 0 Then
	
		FlBG34.visible=0
		FlBG34a.visible=0
		FlBG34b.visible=0
		FlBG34c.visible=0
		FlBG34d.visible=0

		FlBG34e.visible=0
		FlBG34f.visible=0
		FlBG34g.visible=0
		FlBG34h.visible=0
		FlBG34i.visible=0

		else
		FlBG34.visible=1
		FlBG34a.visible=1
		FlBG34b.visible=1
		FlBG34c.visible=1
		FlBG34d.visible=1

		FlBG34e.visible=1
		FlBG34f.visible=1
		FlBG34g.visible=1
		FlBG34h.visible=1
		FlBG34i.visible=1
		end if
	End IF

End Sub


' ***************************************************************************
'                    BASIC FSS(DMD,SS,EM) SETUP CODE
' ****************************************************************************

Dim xoff,yoff,zoff,xrot,zscale, xcen,ycen

sub setup_backglass()

	xoff =490
	yoff =80
	zoff =930
	xrot = -90

	bgdark.x = xoff
	bgdark.y = yoff
	bgdark.height = zoff
	bgdark.rotx = xrot

	bgHigh.x = xoff
	bgHigh.y = yoff
	bgHigh.height = zoff
	bgHigh.rotx = xrot

	bgHigh1.x = xoff
	bgHigh1.y = yoff
	bgHigh1.height = zoff
	bgHigh1.rotx = xrot
		
	bgHigh2.x = xoff
	bgHigh2.y = yoff 
	bgHigh2.height = zoff
	bgHigh2.rotx = xrot

	bgFrame.x = xoff
	bgFrame.y = yoff 
	bgFrame.height = zoff - 100
	bgFrame.rotx = xrot

	BGFrameMask.x = xoff
	BGFrameMask.y = yoff 
	BGFrameMask.height = zoff -100
	BGFrameMask.rotx = xrot


	BGFrameMaskFill.x = xoff 
	BGFrameMaskFill.y = yoff 
	BGFrameMaskFill.height = zoff 
	BGFrameMaskFill.rotx = xrot

	BGFrameMaskFill1.x = xoff
	BGFrameMaskFill1.y = yoff 
	BGFrameMaskFill1.height = zoff
	BGFrameMaskFill1.rotx = xrot
		
	center_graphix()

	center_digits()
end sub

' ********************* POSITION IMAGES(flashers) ON BACKGLASS *************************

Dim BGArr 
BGArr=Array(FlBG1,FlBG3,FlBG4,FlBG5, FlBG6,FlBG7,FlBG8,FlBG9,FlBG10,FlBG11,FlBG12,FlBG13,FlBG14,FlBG15,FlBG16, FlBG17,FlBG18,FlBG19,FlBG20,FlBG21,FlBG22,FlBG23,FlBG24,FlBG25,FlBG26,FlBG27,_
FlBGbulb1,FlBGBulb2,FlBGBulb3,FlBGbulb4,FlBGBulb5,FlBGBulb6,FlBGBulb7,FlBGBulb8,FlBGbulb9,FlBGBulb10,FlBGBulb11,FlBGBulb12,FlBGBulb13,FlBGBulb14, FlBGBulb15,FlBGBulb16,FlBGBulb17,FlBGBulb18,_
FlBGBulb19,FlBGBulb20,FlBGBulb21,FlBGBulb22,FlBGBulb23,FlBGBulb24,FlBGBulb25,FlBGBulb26,FlBGBulb27,FlBGBulb28,_
FlBG34,FlBG34a,FlBG34b,FlBG34c,FlBG34d,FlBG34e, FlBG34f, FlBG34g, FlBG34h, FlBG34i,FlBGLeftSpark, FlBGRightSpark,_
FlMisCon1,FlMisCon2,FlMisCon3,FlMisCon4,FlClr1,FlClr2,FlClr3,FlClr4,FlClr5,FlBGFace,FlBGFace1,Empty) 



Sub center_graphix()
	Dim xx,yy,yfact,xfact,xobj
	zscale = 0.0000001

	xcen =(980 /2) - (70 / 2)
	ycen = (1008 /2 ) + (360 /2)


	yfact =0 'y fudge factor (ycen was wrong so fix)
	xfact =0

	For Each xobj In BGArr
		if Not IsEmpty(xobj) then
		xx =xobj.x 
			
		xobj.x = (xoff -xcen) + xx +xfact
		yy = xobj.y ' get the yoffset before it is changed
		xobj.y =yoff 

			If(yy < 0.) then
			yy = yy * -1
			end if

		
		xobj.height =( zoff - ycen) + yy - (yy * zscale) + yfact
		
		xobj.rotx = xrot
			if xobj.visible <> 0 then
			xobj.visible =1 ' for testing
			end if
		end if
	Next
end sub

Sub center_digits()
	Dim ix, xx, yy, yfact, xfact, xobj

	zscale = 0.0000001

	xcen =(980 /2) - (70 / 2)
	ycen = (1008 /2 ) + (360 /2)

	yfact =0 'y fudge factor (ycen was wrong so fix)
	xfact =0


	for ix =0 to 31
		For Each xobj In Digits(ix)

			'if obj NOT n then

			xx =xobj.x 
				
			xobj.x = (xoff -xcen) + xx +xfact
			yy = xobj.y ' get the yoffset before it is changed
			xobj.y =yoff 

				If(yy < 0.) then
				yy = yy * -1
				end if

			xobj.height =( zoff - ycen) + yy - (yy * (zscale)) + yfact
			
			xobj.rotx = xrot
		Next
	Next
end sub

'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************
 Dim Digits(32)
Digits(0)=Array(ax00, ax05, ax0c, ax0d, ax08, ax01, ax06, ax0f, ax02, ax03, ax04, ax07, ax0b, ax0a, ax09, ax0e)
 Digits(1)=Array(ax10, ax15, ax1c, ax1d, ax18, ax11, ax16, ax1f, ax12, ax13, ax14, ax17, ax1b, ax1a, ax19, ax1e)
 Digits(2)=Array(ax20, ax25, ax2c, ax2d, ax28, ax21, ax26, ax2f, ax22, ax23, ax24, ax27, ax2b, ax2a, ax29, ax2e)
 Digits(3)=Array(ax30, ax35, ax3c, ax3d, ax38, ax31, ax36, ax3f, ax32, ax33, ax34, ax37, ax3b, ax3a, ax39, ax3e)
 Digits(4)=Array(ax40, ax45, ax4c, ax4d, ax48, ax41, ax46, ax4f, ax42, ax43, ax44, ax47, ax4b, ax4a, ax49, ax4e)
 Digits(5)=Array(ax50, ax55, ax5c, ax5d, ax58, ax51, ax56, ax5f, ax52, ax53, ax54, ax57, ax5b, ax5a, ax59, ax5e)
 Digits(6)=Array(ax60, ax65, ax6c, ax6d, ax68, ax61, ax66, ax6f, ax62, ax63, ax64, ax67, ax6b, ax6a, ax69, ax6e)

 Digits(7)=Array(ax70, ax75, ax7c, ax7d, ax78, ax71, ax76, ax7f, ax72, ax73, ax74, ax77, ax7b, ax7a, ax79, ax7e)
 Digits(8)=Array(ax80, ax85, ax8c, ax8d, ax88, ax81, ax86, ax8f, ax82, ax83, ax84, ax87, ax8b, ax8a, ax89, ax8e)
 Digits(9)=Array(ax90, ax95, ax9c, ax9d, ax98, ax91, ax96, ax9f, ax92, ax93, ax94, ax97, ax9b, ax9a, ax99, ax9e)
 Digits(10)=Array(axa0, axa5, axac, axad, axa8, axa1, axa6, axaf, axa2, axa3, axa4, axa7, axab, axaa, axa9, axae)
 Digits(11)=Array(axb0, axb5, axbc, axbd, axb8, axb1, axb6, axbf, axb2, axb3, axb4, axb7, axbb, axba, axb9, axbe)
 Digits(12)=Array(axc0, axc5, axcc, axcd, axc8, axc1, axc6, axcf, axc2, axc3, axc4, axc7, axcb, axca, axc9, axce)
 Digits(13)=Array(axd0, axd5, axdc, axdd, axd8, axd1, axd6, axdf, axd2, axd3, axd4, axd7, axdb, axda, axd9, axde)
 ' 3rd Player
Digits(14) = Array(LED1x0,LED1x1,LED1x2,LED1x3,LED1x4,LED1x5,LED1x6)
Digits(15) = Array(LED2x0,LED2x1,LED2x2,LED2x3,LED2x4,LED2x5,LED2x6)
Digits(16) = Array(LED3x0,LED3x1,LED3x2,LED3x3,LED3x4,LED3x5,LED3x6)
Digits(17) = Array(LED4x0,LED4x1,LED4x2,LED4x3,LED4x4,LED4x5,LED4x6)
Digits(18) = Array(LED5x0,LED5x1,LED5x2,LED5x3,LED5x4,LED5x5,LED5x6)
Digits(19) = Array(LED6x0,LED6x1,LED6x2,LED6x3,LED6x4,LED6x5,LED6x6)
Digits(20) = Array(LED7x0,LED7x1,LED7x2,LED7x3,LED7x4,LED7x5,LED7x6)
' 4th Player
Digits(21) = Array(LED8x0,LED8x1,LED8x2,LED8x3,LED8x4,LED8x5,LED8x6)
Digits(22) = Array(LED9x0,LED9x1,LED9x2,LED9x3,LED9x4,LED9x5,LED9x6)
Digits(23) = Array(LED10x0,LED10x1,LED10x2,LED10x3,LED10x4,LED10x5,LED10x6)
Digits(24) = Array(LED11x0,LED11x1,LED11x2,LED11x3,LED11x4,LED11x5,LED11x6)
Digits(25) = Array(LED12x0,LED12x1,LED12x2,LED12x3,LED12x4,LED12x5,LED12x6)
Digits(26) = Array(LED13x0,LED13x1,LED13x2,LED13x3,LED13x4,LED13x5,LED13x6)
Digits(27) = Array(LED14x0,LED14x1,LED14x2,LED14x3,LED14x4,LED14x5,LED14x6)
' Ball in play
Digits(28) = Array(LEDcx500,LEDcx501,LEDcx502,LEDcx503,LEDcx504,LEDcx505,LEDcx506)
Digits(29) = Array(LEDdx600,LEDdx601,LEDdx602,LEDdx603,LEDdx604,LEDdx605,LEDdx606)
' Num of Credits
Digits(30) = Array(LEDax300,LEDax301,LEDax302,LEDax303,LEDax304,LEDax305,LEDax306)
Digits(31) = Array(LEDbx400,LEDbx401,LEDbx402,LEDbx403,LEDbx404,LEDbx405,LEDbx406)


 Sub DisplayTimer_Timer
	Dim ChgLED, ii, jj, num, chg, stat, objs, b, x
	ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
	If Not IsEmpty(ChgLED)Then
		If DesktopMode = True Then
			   For ii=0 To UBound(chgLED)
				  num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
				if (num < 32) then
					'if (num < 28) then
					  For Each objs In Digits(num)
						   If chg And 1 Then objs.visible=stat And 1
						   chg=chg\2 : stat=stat\2
					Next
				Else
				end if
			Next
		end if
	End If
 End Sub

SolCallback(9) = "Sol9" 'robot face
SolCallback(11) = "Sol11" 'GI
SolCallback(27) = "Sol27" ' left flip
SolCallback(28) = "Sol28" ' right flip

Sub Sol9(Enabled)
	if Enabled then
		FlBGFace.visible = 1
		FlBGFace1.visible = 1
	else 
		FlBGFace.visible = 0
		FlBGFace1.visible = 0
	end If
end Sub

Sub Sol11(Enabled)
	if Enabled = false then
		If VRRoom = 1 Then
		'	FlBG2.visible = 1
			FlBG3.visible = 1
			FlBG4.visible = 1
			FlBG5.visible = 1
			FlBG7.visible = 1
			FlBG8.visible = 1
			FlBG9.visible = 1
			FlBG10.visible = 1
			FlBG11.visible = 1
			FlBG12.visible = 1
			FlBG13.visible = 1
			FlBG14.visible = 1
			FlBG15.visible = 1
			FlBG16.visible = 1
			FlBG17.visible = 1
			FlBG18.visible = 1
			FlBG19.visible = 1
			FlBG20.visible = 1
			FlBG21.visible = 1
			FlBG22.visible = 1
			FlBG23.visible = 1
			FlBG24.visible = 1
			FlBG25.visible = 1
			FlBG26.visible = 1
			FlBG27.visible = 1
			FlBGbulb1.visible = 1
			FlBGBulb2.visible = 1
			FlBGBulb3.visible = 1
			FlBGBulb4.visible = 1
			FlBGBulb5.visible = 1
			FlBGBulb6.visible = 1
			FlBGBulb7.visible = 1
			FlBGBulb8.visible = 1
			FlBGBulb9.visible = 1
			FlBGBulb10.visible = 1
			FlBGBulb11.visible = 1
			FlBGBulb12.visible = 1
			FlBGBulb13.visible = 1
			FlBGBulb14.visible = 1
			FlBGBulb15.visible = 1
			FlBGBulb16.visible = 1
			FlBGBulb17.visible = 1
			FlBGBulb18.visible = 1
			FlBGBulb19.visible = 1
			FlBGBulb20.visible = 1
			FlBGBulb21.visible = 1
			FlBGBulb22.visible = 1
			FlBGBulb23.visible = 1
			FlBGBulb24.visible = 1
			FlBGBulb25.visible = 1
			FlBGBulb26.visible = 1
			FlBGBulb27.visible = 1
			FlBGBulb28.visible = 1
			

			FlMisCon1.visible = 1
			FlMisCon2.visible = 1
			FlMisCon3.visible = 1
			FlMisCon4.visible = 1

			bghigh2.intensityscale = 1.3
		End If
	else 
		If VRRoom = 1 Then
			FlBG3.visible = 0
			FlBG4.visible = 0
			FlBG5.visible = 0
			FlBG7.visible = 0
			FlBG8.visible = 0
			FlBG9.visible = 0
			FlBG10.visible = 0
			FlBG11.visible = 0
			FlBG12.visible = 0
			FlBG13.visible = 0
			FlBG14.visible = 0
			FlBG15.visible = 0
			FlBG16.visible = 0
			FlBG17.visible = 0
			FlBG18.visible = 0
			FlBG19.visible = 0
			FlBG20.visible = 0
			FlBG21.visible = 0
			FlBG22.visible = 0
			FlBG23.visible = 0
			FlBG24.visible = 0
			FlBG25.visible = 0
			FlBG26.visible = 0
			FlBG27.visible = 0
			FlBGbulb1.visible = 0
			FlBGBulb2.visible = 0
			FlBGBulb3.visible = 0
			FlBGBulb4.visible = 0
			FlBGBulb5.visible = 0
			FlBGBulb6.visible = 0
			FlBGBulb7.visible = 0
			FlBGBulb8.visible = 0
			FlBGBulb9.visible = 0
			FlBGBulb10.visible = 0
			FlBGBulb11.visible = 0
			FlBGBulb12.visible = 0
			FlBGBulb13.visible = 0
			FlBGBulb14.visible = 0
			FlBGBulb15.visible = 0
			FlBGBulb16.visible = 0
			FlBGBulb17.visible = 0
			FlBGBulb18.visible = 0
			FlBGBulb19.visible = 0
			FlBGBulb20.visible = 0
			FlBGBulb21.visible = 0
			FlBGBulb22.visible = 0
			FlBGBulb23.visible = 0
			FlBGBulb24.visible = 0
			FlBGBulb25.visible = 0
			FlBGBulb26.visible = 0
			FlBGBulb27.visible = 0
			FlBGBulb28.visible = 0

			FlMisCon1.visible = 0
			FlMisCon2.visible = 0
			FlMisCon3.visible = 0
			FlMisCon4.visible = 0

			bghigh2.intensityscale = 1.0
		End If
	end if
end Sub

Sub Sol27(Enabled) 
	if Enabled then
		If VRRoom = 1 Then
			FlBG1.visible = 1
			FlBG6.visible = 1
			FlBGLeftSpark.visible = 1
			FlBGRightSpark.visible = 1
		End If
	Else
		If VRRoom = 1 Then
			FlBG1.visible = 0
			FlBG6.visible = 0
			FlBGLeftSpark.visible = 0
			FlBGRightSpark.visible = 0
		End If
	end if
end Sub

Sub Sol28(Enabled)
	if Enabled then
		If VRRoom = 1 Then
			FlBG6.visible = 1
			FlBGRightSpark.visible = 1
		End If
	Else
		If VRRoom = 1 Then
			FlBG6.visible = 0
			FlBGRightSpark.visible = 0
		End If
	end if
end Sub


' ***************************************************************************
'                          DAY & NIGHT FUNCTIONS AND DATASETS
' ****************************************************************************
' PGI (Persistent General illumination)
'-1= 0.5x  emmissive de-block increase
' 0= (default)full emmisive blocking
' 1= Half of full
' 2= Quarter of full 
' 3= 8'th of full
' 4= 16'th of full
' 5= 32'nd of full
' 6 = full emmision

Const GILevel = 3
Const FLLevel = 5
Const BTLevel = 3

Const cUSEBACKGLASS = 1
Const cUSEBGLASSCCX =1
Const cUSEBGLASSREFL =1

Dim nxx, DNS
DNS = Table1.NightDay

Dim DivLevel: DivLevel = 35
Dim DNSVal: DNSVal = Round(DNS/10)
Dim DNShift: DNShift = 3

'Dim OPSValues: OPSValues=Array (100,50,20,10 ,5,4 ,3 ,2 ,1, 0,0)
'Dim DNSValues: DNSValues=Array (1.0,0.5,0.1,0.05,0.01,0.005,0.001,0.0005,0.0001, 0.00005,0.00000)
Dim OPSValues: OPSValues=Array (100,50,20,10 ,9,8 ,7 ,6 ,5, 4,3,2,1,0)
Dim OPS2: OPS2  =Array (1.0,0.95,0.90,0.85,0.80,0.75,0.70,0.65,0.60,0.55,0.50,0.50)
Dim OPSValues2: OPSValues2=Array (100*OPS2(DNSVal),50*OPS2(DNSVal),20*OPS2(DNSVal),10*OPS2(DNSVal),9*OPS2(DNSVal),8*OPS2(DNSVal),7*OPS2(DNSVal),6*OPS2(DNSVal),5*OPS2(DNSVal), 4*OPS2(DNSVal),3*OPS2(DNSVal),2*OPS2(DNSVal),1*OPS2(DNSVal),0*OPS2(DNSVal))
Dim OPS3: OPS3 =Array (1.0,0.93,0.85,0.78,0.70,0.65,0.55,0.48,0.40,0.33,0.25,0.25)
Dim OPSValues3: OPSValues3=Array (100*OPS3(DNSVal),50*OPS3(DNSVal),20*OPS3(DNSVal),10*OPS3(DNSVal),9*OPS3(DNSVal),8*OPS3(DNSVal),7*OPS3(DNSVal),6*OPS3(DNSVal),5*OPS3(DNSVal),4*OPS3(DNSVal),3*OPS3(DNSVal),2*OPS3(DNSVal),1*OPS3(DNSVal),0*OPS3(DNSVal))
Dim OPS4: OPS4 =Array (1.0,0.91,0.82,0.74,0.65,0.56,0.47,0.38,0.30,0.21,0.12,0.12)
Dim OPSValues4: OPSValues4=Array (100*OPS4(DNSVal),50*OPS4(DNSVal),20*OPS4(DNSVal),10*OPS4(DNSVal),9*OPS4(DNSVal),8*OPS4(DNSVal),7*OPS4(DNSVal),6*OPS4(DNSVal),5*OPS4(DNSVal),4*OPS4(DNSVal),3*OPS4(DNSVal),2*OPS4(DNSVal),1*OPS4(DNSVal),0*OPS4(DNSVal))
Dim OPS5: OPS5 =Array (1.0,0.91,0.83,0.72,0.63,0.55,0.44,0.35,0.25,0.16,0.07,0.07)
Dim OPSValues5: OPSValues5=Array (100*OPS5(DNSVal),50*OPS5(DNSVal),20*OPS5(DNSVal),10*OPS5(DNSVal),9*OPS5(DNSVal),8*OPS5(DNSVal),7*OPS5(DNSVal),6*OPS5(DNSVal),5*OPS5(DNSVal),4*OPS5(DNSVal),3*OPS5(DNSVal),2*OPS5(DNSVal),1*OPS5(DNSVal),0*OPS5(DNSVal))
Dim OPS6: OPS6 =Array (1.0,0.90,0.81,0.71,0.62,0.52,0.42,0.33,0.23,0.14,0.04,0.04)
Dim OPSValues6: OPSValues6=Array (100*OPS6(DNSVal),50*OPS6(DNSVal),20*OPS6(DNSVal),10*OPS5(DNSVal),9*OPS6(DNSVal),8*OPS6(DNSVal),7*OPS6(DNSVal),6*OPS6(DNSVal),5*OPS6(DNSVal),4*OPS6(DNSVal),3*OPS6(DNSVal),2*OPS6(DNSVal),1*OPS6(DNSVal),0*OPS6(DNSVal))

Dim OPS1: OPS1 = 0.01 '0.25
Dim OPSValues1: OPSValues1=Array (100*OPS1,50*OPS1,20*OPS1,10*OPS1,9*OPS1,8*OPS1,7*OPS1,6*OPS1,5*OPS1,4*OPS1,3*OPS1,2*OPS1,1*OPS1,0*OPS1)

Dim DNSValues: DNSValues=Array (1.0,0.5,0.1,0.05,0.01,0.005,0.001,0.0005,0.0001, 0.00005,0.00001, 0.000005, 0.000001)
Dim SysDNSVal: SysDNSVal=Array (1.0,0.9,0.8,0.7,0.6,0.5,0.5,0.5,0.5, 0.5,0.5)
Dim DivValues: DivValues =Array (1,2,4,8,16,32,32,32,32, 32,32)
Dim DivValues2: DivValues2 =Array (1,1.5,2,2.5,3,3.5,4,4.5,5, 5.5,6)
Dim DivValues3: DivValues3 =Array (1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2.0,2.1)

Dim DIV4: DIV4 =Array (1.0,0.95,0.90,0.85,0.80,0.75,0.70,0.65,0.60,0.55,0.50,0.50)
Dim DivValues4: DivValues4 =Array (1*DIV4(DNSVal),1.1*DIV4(DNSVal),1.2*DIV4(DNSVal),1.3*DIV4(DNSVal),1.4*DIV4(DNSVal),1.5*DIV4(DNSVal),1.6*DIV4(DNSVal),1.7*DIV4(DNSVal),1.8*DIV4(DNSVal),1.9*DIV4(DNSVal),2.0*DIV4(DNSVal),2.1*DIV4(DNSVal))
Dim DIV5: DIV5 =Array (1.0,0.93,0.85,0.78,0.70,0.65,0.55,0.48,0.40,0.33,0.25,0.25)
Dim DivValues5: DivValues5 =Array (1*DIV5(DNSVal),1.1*DIV5(DNSVal),1.2*DIV5(DNSVal),1.3*DIV5(DNSVal),1.4*DIV5(DNSVal),1.5*DIV5(DNSVal),1.6*DIV5(DNSVal),1.7*DIV5(DNSVal),1.8*DIV5(DNSVal),1.9*DIV5(DNSVal),2.0*DIV5(DNSVal),2.1*DIV5(DNSVal))
Dim DIV6: DIV6 =Array (1.0,0.91,0.82,0.74,0.65,0.56,0.47,0.38,0.30,0.21,0.12,0.12)
Dim DivValues6: DivValues6 =Array (1*DIV6(DNSVal),1.1*DIV6(DNSVal),1.2*DIV6(DNSVal),1.3*DIV6(DNSVal),1.4*DIV6(DNSVal),1.5*DIV6(DNSVal),1.6*DIV6(DNSVal),1.7*DIV6(DNSVal),1.8*DIV6(DNSVal),1.9*DIV6(DNSVal),2.0*DIV6(DNSVal),2.1*DIV6(DNSVal))
Dim DIV7: DIV7 =Array (1.0,0.91,0.83,0.72,0.63,0.55,0.44,0.35,0.25,0.16,0.07,0.07)
Dim DivValues7: DivValues7 =Array (1*DIV7(DNSVal),1.1*DIV7(DNSVal),1.2*DIV7(DNSVal),1.3*DIV7(DNSVal),1.4*DIV7(DNSVal),1.5*DIV7(DNSVal),1.6*DIV7(DNSVal),1.7*DIV7(DNSVal),1.8*DIV7(DNSVal),1.9*DIV7(DNSVal),2.0*DIV7(DNSVal),2.1*DIV7(DNSVal))
Dim DIV8: DIV8 =Array (1.0,0.90,0.81,0.71,0.62,0.52,0.42,0.33,0.23,0.14,0.04,0.04)
Dim DivValues8: DivValues8 =Array (1*DIV8(DNSVal),1.1*DIV8(DNSVal),1.2*DIV8(DNSVal),1.3*DIV8(DNSVal),1.4*DIV8(DNSVal),1.5*DIV8(DNSVal),1.6*DIV8(DNSVal),1.7*DIV8(DNSVal),1.8*DIV8(DNSVal),1.9*DIV8(DNSVal),2.0*DIV8(DNSVal),2.1*DIV8(DNSVal))

Dim MUL1: MUL1 =Array (1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0)' 1.1
Dim MulValues1: MulValues1 =Array (1*MUL1(DNSVal),1.1*MUL1(DNSVal),1.2*MUL1(DNSVal),1.3*MUL1(DNSVal),1.4*MUL1(DNSVal),1.5*MUL1(DNSVal),1.6*MUL1(DNSVal),1.7*MUL1(DNSVal),1.8*MUL1(DNSVal),1.9*MUL1(DNSVal),2.0*MUL1(DNSVal),2.1*MUL1(DNSVal))

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

' PLAYFIELD GENERAL OPERATIONAL and LOCALALIZED GI ILLUMINATION
Dim aAllFlashers: aAllFlashers=Array(FlBG1,FlBG3,FlBG4,FlBG5, FlBG6,FlBG7,FlBG8,FlBG9,FlBG10,FlBG11,FlBG12,FlBG13,FlBG14,FlBG15,FlBG16, FlBG17,FlBG18,FlBG19,FlBG20,FlBG21,FlBG22,FlBG23,FlBG24,FlBG25,FlBG26,FlBG27,_
FlBGbulb1,FlBGBulb2,FlBGBulb3,FlBGbulb4,FlBGBulb5,FlBGBulb6,FlBGBulb7,FlBGBulb8,FlBGbulb9,FlBGBulb10,FlBGBulb11,FlBGBulb12,FlBGBulb13,FlBGBulb14, FlBGBulb15,FlBGBulb16,FlBGBulb17,FlBGBulb18,_
FlBGBulb19,FlBGBulb20,FlBGBulb21,FlBGBulb22,FlBGBulb23,FlBGBulb24,FlBGBulb25,FlBGBulb26,FlBGBulb27,FlBGBulb28,_
FlBG34,FlBG34a,FlBG34b,FlBG34c,FlBG34d,FlBG34e, FlBG34f, FlBG34g, FlBG34h, FlBG34i,FlBGLeftSpark, FlBGRightSpark,_
FlMisCon1,FlMisCon2,FlMisCon3,FlMisCon4,FlClr1,FlClr2,FlClr3,FlClr4,FlClr5,FlBGFace,FlBGFace1,_
Flasher1L2,Flasher2L5,Flasher2L2,Flasher2TL2,Flasher2TR2,Flasher1R2,Flasher2L6,Flasher2TL3,Flasher2TR3)
'Dim aGiLights: aGiLights=array()
'Dim BloomLights: BloomLights=array()

'BACKBOX & BACKGLASS ILLUMINATION
if cUSEBACKGLASS then
	BGDark.ModulateVsAdd = MVSAdd(DNSVal)
	BGDark.Color = RGB(RValUP(DNSVal),GValUP(DNSVal),BValUP(DNSVal))
	BGDark.Amount = FValUP(DNSVal) / DivValues(DNSVal)
	BGHigh.Color = RGB(RValDN(DNSVal),GValDN(DNSVal),BValDN(DNSVal))
	BGHigh.Amount = FValDN(DNSVal)  / DivValues(DNSVal)
	BGHigh1.Color = RGB(RValDN(DNSVal),GValDN(DNSVal),BValDN(DNSVal))
	BGHigh1.Amount = FValDN(DNSVal) / DivValues(DNSVal)

	BGframe.ModulateVsAdd = MVSAdd(DNSVal) * 0.2
	BGframe.Color = RGB(RValUP(DNSVal),GValUP(DNSVal),BValUP(DNSVal))
	BGframe.Amount = FValUP(DNSVal) * DivValues(DNSVal)

	BGHigh.intensityscale = 0.5
	BGHigh1.intensityscale = 0.5
	BGDark.intensityscale = 0.6
	BGframe.intensityscale = 0.6

	'Add BGHigh2 to aAllFlashers array if not using BGFrameMaskFill1 & BGFrameMaskFill5 in cUSEBGLASSREFL otherwise leave it out.
	BGHigh2.intensityscale = 1.3
end If ' cUSEBACKGLASS

If FLLevel = 0 then ' default = Real
For each nxx in aAllFlashers:nxx.amount = nxx.amount / DivValues3(DNSVal):Next
For each nxx in aAllFlashers:nxx.opacity = nxx.opacity * OPSValues(DNSVal) / DivLevel:Next
elseif FLLevel = 1 then ' half of real reduced
For each nxx in aAllFlashers:nxx.amount = nxx.amount / DivValues4(DNSVal):Next
For each nxx in aAllFlashers:nxx.opacity = nxx.opacity * OPSValues2(DNSVal) / DivLevel:Next
elseif FLLevel = 2 then ' 1/4 of real reduced
For each nxx in aAllFlashers:nxx.amount = nxx.amount / DivValues5(DNSVal):Next
For each nxx in aAllFlashers:nxx.opacity = nxx.opacity * OPSValues3(DNSVal) / DivLevel:Next
elseif FLLevel = 3 then ' 1/8'th of real reduced
For each nxx in aAllFlashers:nxx.amount = nxx.amount / DivValues6(DNSVal):Next
For each nxx in aAllFlashers:nxx.opacity = nxx.opacity * OPSValues4(DNSVal) / DivLevel:Next
elseif FLLevel = 4 then ' 1/16'th of real reduced
For each nxx in aAllFlashers:nxx.amount = nxx.amount / DivValues7(DNSVal):Next
For each nxx in aAllFlashers:nxx.opacity = nxx.opacity * OPSValues5(DNSVal) / DivLevel:Next
elseif FLLevel = 5 then ' 1/32'th of real reduced
For each nxx in aAllFlashers:nxx.amount = nxx.amount / DivValues8(DNSVal):Next
For each nxx in aAllFlashers:nxx.opacity = nxx.opacity * OPSValues6(DNSVal) / DivLevel:Next
end If



'******************************************************
'                 NFOZZY DROP TARGETS
'******************************************************


'******************************************************
'                DROP TARGETS INITIALIZATION
'******************************************************

Class DropTarget
  Private m_primary, m_secondary, m_prim, m_sw, m_animate, m_isDropped

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(input): Set m_primary = input: End Property

  Public Property Get Secondary(): Set Secondary = m_secondary: End Property
  Public Property Let Secondary(input): Set m_secondary = input: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(input): Set m_prim = input: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(input): m_sw = input: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(input): m_animate = input: End Property

  Public Property Get IsDropped(): IsDropped = m_isDropped: End Property
  Public Property Let IsDropped(input): m_isDropped = input: End Property

  Public default Function init(primary, secondary, prim, sw, animate, isDropped)
    Set m_primary = primary
    Set m_secondary = secondary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate
    m_isDropped = isDropped

    Set Init = Me
  End Function
End Class

Dim DT49, DT50, DT51

Set DT49 = (new DropTarget)(sw49, sw49offset, psw49, 49, 0, false)
Set DT50 = (new DropTarget)(sw50, sw50offset, psw50, 50, 0, false)
Set DT51 = (new DropTarget)(sw51, sw51offset, psw51, 51, 0, false)

Dim DTArray
DTArray = Array(DT49, DT50, DT51)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110                         'in milliseconds
Const DTDropUpSpeed = 40                         'in milliseconds
Const DTDropUnits = 44                         'VP units primitive drops
Const DTDropUpUnits = 10                         'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8                                 'max degrees primitive rotates when hit
Const DTDropDelay = 20                         'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40                         'time in milliseconds before target drops back to normal up position after the solendoid fires to raise the target
Const DTBrickVel = 0                               'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0                       'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "targethit"        'Drop Target Hit sound
Const DTDropSound = "DTDrop"                'Drop Target Drop sound
Const DTResetSound = "DTReset"        'Drop Target reset sound

Const DTMass = 0.2                                'Mass of the Drop Target (between 0 and 1), higher values provide more resistance


'******************************************************
'                                DROP TARGETS FUNCTIONS
'******************************************************
    
Sub DTHit(switch)
        Dim i
        i = DTArrayID(switch)
    
'        PlaySoundAtVol  DTHitSound, Activeball, Vol(Activeball)*22.5
        DTArray(i).animate =  DTCheckBrick(Activeball,DTArray(i).prim)
        If DTArray(i).animate = 1 or DTArray(i).animate = 3 or DTArray(i).animate = 4 Then
                DTBallPhysics Activeball, DTArray(i).prim.rotz, DTMass
        End If
        DoDTAnim
End Sub

Sub DTRaise(switch)
        Dim i
        i = DTArrayID(switch)
    
        DTArray(i).animate = -1
        DoDTAnim
End Sub

Sub DTDrop(switch)
        Dim i
        i = DTArrayID(switch)
    
        DTArray(i).animate = 1
        DoDTAnim
End Sub

Function DTArrayID(switch)
        Dim i
        For i = 0 to uBound(DTArray) 
                If DTArray(i).sw = switch Then DTArrayID = i:Exit Function 
        Next
End Function


sub DTBallPhysics(aBall, angle, mass)
        dim rangle,bangle,calc1, calc2, calc3
        rangle = (angle - 90) * 3.1416 / 180
        bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
    
        calc1 = cor.BallVel(aball.id) * cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
        calc2 = cor.BallVel(aball.id) * sin(bangle - rangle) * cos(rangle + 4*Atn(1)/2)
        calc3 = cor.BallVel(aball.id) * sin(bangle - rangle) * sin(rangle + 4*Atn(1)/2)

        aBall.velx = calc1 * cos(rangle) + calc2
        aBall.vely = calc1 * sin(rangle) + calc3
End Sub

'Add Timer name DTAnim to editor to handle drop target animations
DTAnim.interval = 10
DTAnim.enabled = True

Sub DTAnim_Timer()
        DoDTAnim
End Sub

'Check if target is hit on it's face or sides and whether a 'brick' occurred
Function DTCheckBrick(aBall, dtprim) 
        dim bangle, bangleafter, rangle, rangle2, Xintersect, Yintersect, cdist, perpvel, perpvelafter, paravel, paravelafter
        rangle = (dtprim.rotz - 90) * 3.1416 / 180
        rangle2 = dtprim.rotz * 3.1416 / 180
        bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
        bangleafter = Atn2(aBall.vely,aball.velx)

        Xintersect = (aBall.y - dtprim.y - tan(bangle) * aball.x + tan(rangle2) * dtprim.x) / (tan(rangle2) - tan(bangle))
        Yintersect = tan(rangle2) * Xintersect + (dtprim.y - tan(rangle2) * dtprim.x)

        cdist = Distance(dtprim.x, dtprim.y, Xintersect, Yintersect)

        perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
        paravel = cor.BallVel(aball.id) * sin(bangle-rangle)

        perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 
        paravelafter = BallSpeed(aBall) * sin(bangleafter - rangle)

        If perpvel > 0 and  perpvelafter <= 0 Then
                If DTEnableBrick = 1 and  perpvel > DTBrickVel and DTBrickVel <> 0 and cdist < 8 Then
                        DTCheckBrick = 3
                Else
                        DTCheckBrick = 1
                End If
        ElseIf perpvel > 0 and ((paravel > 0 and paravelafter > 0) or (paravel < 0 and paravelafter < 0)) Then
                DTCheckBrick = 4
        Else 
                DTCheckBrick = 0
        End If
End Function


Sub DoDTAnim()
        Dim i
        For i=0 to Ubound(DTArray)
                DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).sw,DTArray(i).animate)
        Next
	'table specific code
	DTShadow psw49, dropplate1
	DTShadow psw50, dropplate2
	DTShadow psw51, dropplate3
End Sub

' Table specific function
Sub	DTShadow(prim, shadowprim)
	If prim.transz < -DTDropUnits/2 Then
		shadowprim.visible = false
	Else
		shadowprim.visible = true 
	End If
End Sub

Function DTAnimate(primary, secondary, prim, switch,  animate)
        dim transz
        Dim animtime, rangle

        rangle = prim.rotz * 3.1416 / 180

        DTAnimate = animate

        if animate = 0  Then
                primary.uservalue = 0
                DTAnimate = 0
                Exit Function
        Elseif primary.uservalue = 0 then 
                primary.uservalue = gametime
        end if

        animtime = gametime - primary.uservalue

        If (animate = 1 or animate = 4) and animtime < DTDropDelay Then
                primary.collidable = 0
                If animate = 1 then secondary.collidable = 1 else secondary.collidable= 0
                prim.rotx = DTMaxBend * cos(rangle)
                prim.roty = DTMaxBend * sin(rangle)
                DTAnimate = animate
                Exit Function
        elseif (animate = 1 or animate = 4) and animtime > DTDropDelay Then
                primary.collidable = 0
                If animate = 1 then secondary.collidable = 1 else secondary.collidable= 0
                prim.rotx = DTMaxBend * cos(rangle)
                prim.roty = DTMaxBend * sin(rangle)
                animate = 2
                PlaySoundAt SoundFX(DTDropSound,DOFDropTargets),prim
        End If
   
        if animate = 2 Then
                transz = (animtime - DTDropDelay)/DTDropSpeed *  DTDropUnits * -1
                if prim.transz > -DTDropUnits  Then
                        prim.transz = transz
                end if
 
                prim.rotx = DTMaxBend * cos(rangle)/2
                prim.roty = DTMaxBend * sin(rangle)/2

                if prim.transz <= -DTDropUnits Then 
                        prim.transz = -DTDropUnits
                        secondary.collidable = 0
                        controller.Switch(Switch) = 1
                        primary.uservalue = 0
                        DTAnimate = 0
                        Exit Function
                Else
                        DTAnimate = 2
                        Exit Function
                end If 
        End If
    
        If animate = 3 and animtime < DTDropDelay Then
                primary.collidable = 0
                secondary.collidable = 1
                prim.rotx = DTMaxBend * cos(rangle)
                prim.roty = DTMaxBend * sin(rangle)
        elseif animate = 3 and animtime > DTDropDelay Then
                primary.collidable = 1
                secondary.collidable = 0
                prim.rotx = 0
                prim.roty = 0
                primary.uservalue = 0
                DTAnimate = 0
                Exit Function
        End If
    
        if animate = -1 Then
                transz = (1 - (animtime)/DTDropUpSpeed) *  DTDropUnits * -1

                If prim.transz = -DTDropUnits Then
                        Dim BOT, b
                        BOT = GetBalls

                        For b = 0 to UBound(BOT)
                                If InRect(BOT(b).x,BOT(b).y,prim.x-25,prim.y-10,prim.x+25, prim.y-10,prim.x+25,prim.y+25,prim.x -25,prim.y+25) Then
                                        BOT(b).velz = 20
                                End If
                        Next
                End If

                if prim.transz < 0 Then
                        prim.transz = transz
                elseif transz > 0 then
                        prim.transz = transz
                end if

                if prim.transz > DTDropUpUnits then 
                        DTAnimate = -2
                        prim.rotx = 0
                        prim.roty = 0
                        primary.uservalue = gametime
                end if
                primary.collidable = 0
                secondary.collidable = 1
                controller.Switch(Switch) = 0

        End If

        if animate = -2 and animtime > DTRaiseDelay Then
                prim.transz = (animtime - DTRaiseDelay)/DTDropSpeed *  DTDropUnits * -1 + DTDropUpUnits 
                if prim.transz < 0 then
                        prim.transz = 0
                        primary.uservalue = 0
                        DTAnimate = 0

                        primary.collidable = 1
                        secondary.collidable = 0
                end If 
        End If
End Function

'******************************************************
'                DROP TARGET
'                SUPPORTING FUNCTIONS 
'******************************************************
    
' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function
    
' Used for drop targets
Function Atn2(dy, dx)
'        dim pi
'        pi = 4*Atn(1)
    
        If dx > 0 Then
                Atn2 = Atn(dy / dx)
        ElseIf dx < 0 Then
                If dy = 0 Then 
                        Atn2 = pi
                Else
                        Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
                end if
        ElseIf dx = 0 Then
                if dy = 0 Then
                        Atn2 = 0
                else
                        Atn2 = Sgn(dy) * pi / 2
                end if
        End If
End Function
    
' Used for drop targets
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
        End If End Function
    
' Used for drop targets
Function Distance(ax,ay,bx,by)
        Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function


Sub ResetDrops(enabled)
     if enabled then
          PlaySoundAt SoundFX(DTResetSound,DOFContactors), cutout_prim19
          DTRaise 49
          DTRaise 50
          DTRaise 51
     end if
End Sub


'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.

'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1                                                                                                                'volume level; range [0, 1]
NudgeLeftSoundLevel = 1                                                                                                        'volume level; range [0, 1]
NudgeRightSoundLevel = 1                                                                                                'volume level; range [0, 1]
NudgeCenterSoundLevel = 1                                                                                                'volume level; range [0, 1]
StartButtonSoundLevel = 0.1                                                                                                'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr                                                                                        'volume level; range [0, 1]
PlungerPullSoundLevel = 1                                                                                                'volume level; range [0, 1]
RollingSoundFactor = 1.1/5                

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010                                                           'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635                                                                'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                                                                        'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                                                                      'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel                                                                'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel                                                                'sound helper; not configurable
SlingshotSoundLevel = 0.95                                                                                                'volume level; range [0, 1]
BumperSoundFactor = 4.25                                                                                                'volume multiplier; must not be zero
KnockerSoundLevel = 1                                                                                                         'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2                                                                        'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5                                                                                        'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5                                                                                        'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5                                                                                'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025                                                                        'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025                                                                        'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8                                                                        'volume level; range [0, 1]
WallImpactSoundFactor = 0.075                                                                                        'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5                                                                                                        'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10                                                                                        'volume multiplier; must not be zero
DTSoundLevel = 0.25                                                                                                                'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                                                                      'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8                                                                                                                'volume level; range [0, 1]
BallReleaseSoundLevel = 1                                                                                                'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2                                                                        'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015                                                                                'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5                                                                                                        'volume multiplier; must not be zero


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
    PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
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
    PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
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
        Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
        Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
        PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


' *********************************************************************
'                     Fleep  Supporting Ball & Sound Functions
' *********************************************************************

Dim tablewidth, tableheight : tablewidth = table1.width : tableheight = table1.height

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
        Dim tmp
    tmp = tableobj.y * 2 / tableheight-1
    If tmp > 0 Then
                AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1
    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
        Vol = Csng(BallVel(ball) ^2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
        Volz = Csng((ball.velz) ^2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
        VolPlayfieldRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
    PitchPlayfieldRoll = BallVel(ball) ^2 * 15
End Function

Function RndInt(min, max)
    RndInt = Int(Rnd() * (max-min + 1) + min)' Sets a random number integer between min and max
End Function

Function RndNum(min, max)
    RndNum = Rnd() * (max-min) + min' Sets a random number between min and max
End Function

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////
Sub SoundStartButton()
        PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
        PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
End Sub

Sub SoundNudgeRight()
        PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
        PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
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
        PlaySoundAtLevelStatic ("Drain_" & Int(Rnd*11)+1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
        PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd*7)+1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
        PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd*10)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
        PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd*8)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumperTop(Bump)
        PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
        PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
        PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////
Sub SoundFlipperUpAttackLeft(flipper)
        FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
        PlaySoundAtLevelStatic ("Flipper_Attack-L01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
        FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
                PlaySoundAtLevelStatic ("Flipper_Attack-R01"), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////
Sub RandomSoundFlipperUpLeft(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd*9)+1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd*9)+1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd*7)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
        PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd*8)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
        FlipperLeftHitParm = parm/10
        If FlipperLeftHitParm > 1 Then
                FlipperLeftHitParm = 1
        End If
        FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
        RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
        FlipperRightHitParm = parm/10
        If FlipperRightHitParm > 1 Then
                FlipperRightHitParm = 1
        End If
        FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
         RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
        PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd*7)+1), parm  * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
        PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd*4)+1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
        RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////
Sub Rubbers_Hit(idx)
         dim finalspeed
          finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
         If finalspeed > 5 then                
                 RandomSoundRubberStrong 1
        End if
        If finalspeed <= 5 then
                 RandomSoundRubberWeak()
         End If        
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////
Sub RandomSoundRubberStrong(voladj)
        Select Case Int(Rnd*10)+1
                Case 1 : PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 2 : PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 3 : PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 4 : PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 5 : PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 6 : PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 7 : PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 8 : PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 9 : PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
                Case 10 : PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6*voladj
        End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////
Sub RandomSoundRubberWeak()
        PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd*9)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////
Sub Walls_Hit(idx)
         dim finalspeed
          finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
         If finalspeed > 5 then
                 RandomSoundRubberStrong 1 
        End if
        If finalspeed <= 5 then
                 RandomSoundRubberWeak()
         End If        
End Sub

Sub RandomSoundWall()
         dim finalspeed
          finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
         If finalspeed > 16 then 
                Select Case Int(Rnd*5)+1
                        Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 5 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
                End Select
        End if
        If finalspeed >= 6 AND finalspeed <= 16 then
                Select Case Int(Rnd*4)+1
                        Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
                End Select
         End If
        If finalspeed < 6 Then
                Select Case Int(Rnd*3)+1
                        Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
                        Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
                End Select
        End if
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////
Sub RandomSoundMetal()
        PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd*13)+1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Hit (idx)
        RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
        RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////
Sub RandomSoundBottomArchBallGuide()
         dim finalspeed
          finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
         If finalspeed > 16 then 
                PlaySoundAtLevelActiveBall ("Apron_Bounce_"& Int(Rnd*2)+1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
        End if
        If finalspeed >= 6 AND finalspeed <= 16 then
                 Select Case Int(Rnd*2)+1
                        Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
                        Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
                End Select
         End If
        If finalspeed < 6 Then
                 Select Case Int(Rnd*2)+1
                        Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
                        Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
                End Select
        End if
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////
Sub RandomSoundBottomArchBallGuideHardHit()
        PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd*3)+1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
        If Abs(cor.ballvelx(activeball.id) < 4) and cor.ballvely(activeball.id) > 7 then
                RandomSoundBottomArchBallGuideHardHit()
        Else
                RandomSoundBottomArchBallGuide
        End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////
Sub RandomSoundFlipperBallGuide()
         dim finalspeed
          finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
         If finalspeed > 16 then 
                 Select Case Int(Rnd*2)+1
                        Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
                        Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
                End Select
        End if
        If finalspeed >= 6 AND finalspeed <= 16 then
                PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd*3)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
         End If
        If finalspeed < 6 Then
                PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd*7)+1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
        End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
        PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()                
        PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
         dim finalspeed
          finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
         If finalspeed > 10 then
                 RandomSoundTargetHitStrong()
                RandomSoundBallBouncePlayfieldSoft Activeball
        Else 
                 RandomSoundTargetHitWeak()
         End If        
End Sub

Sub Targets_Hit (idx)
        PlayTargetSound        
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////
Sub RandomSoundBallBouncePlayfieldSoft(aBall)
        Select Case Int(Rnd*9)+1
                Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
                Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
                Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
                Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
                Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
                Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
                Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
                Case 8 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
                Case 9 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
        End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
        PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd*7)+1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////
Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
        Select Case Int(Rnd*5)+1
                Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
                Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
                Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
                Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
                Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
        End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()                        
        PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd*2)+1), GateSoundLevel, Activeball
End Sub

Sub SoundHeavyGate()
        PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub

Sub Gates_hit(idx)
        SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)        
        SoundPlayfieldGate        
End Sub        

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
        PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
        PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd*4)+1), Vol(ActiveBall) * ArchSoundFactor
End Sub


Sub Arch1_hit()
        If Activeball.velx > 1 Then SoundPlayfieldGate
        StopSound "Arch_L1"
        StopSound "Arch_L2"
        StopSound "Arch_L3"
        StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
        If activeball.velx < -8 Then
                RandomSoundRightArch
        End If
End Sub

Sub Arch2_hit()
        If Activeball.velx < 1 Then SoundPlayfieldGate
        StopSound "Arch_R1"
        StopSound "Arch_R2"
        StopSound "Arch_R3"
        StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
        If activeball.velx > 10 Then
                RandomSoundLeftArch
        End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
        PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd*2)+1), SaucerLockSoundLevel, Activeball
End Sub

Sub SoundSaucerKick(scenario, saucer)
        Select Case scenario
                Case 0: PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
                Case 1: PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
        End Select
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////
Sub OnBallBallCollision(ball1, ball2, velocity)
        Dim snd
        Select Case Int(Rnd*7)+1
                Case 1 : snd = "Ball_Collide_1"
                Case 2 : snd = "Ball_Collide_2"
                Case 3 : snd = "Ball_Collide_3"
                Case 4 : snd = "Ball_Collide_4"
                Case 5 : snd = "Ball_Collide_5"
                Case 6 : snd = "Ball_Collide_6"
                Case 7 : snd = "Ball_Collide_7"
        End Select

        PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

Sub WindowGlass_hit()
	PlaySoundAtBall "fx_glass"
End Sub

'/////////////////////////////////////////////////////////////////
'                                        End Mechanical Sounds
'/////////////////////////////////////////////////////////////////



'******************* VR Plunger **********************

Sub TimerVRPlunger_Timer

  If PinCab_Shooter.Y < -245 then
       PinCab_Shooter.Y = PinCab_Shooter.Y + 5
  End If
End Sub

Sub TimerVRPlunger2_Timer
	PinCab_Shooter.Y = -371 + (5* Plunger.Position) -20
End Sub



'***************************************************************************************************
' *************   DIMS FOR THE ANIMATED COIN DOOR AND MANUAL  *  Add near the top of the script into existing table code
'***************************************************************************************************
Dim ManualPageNumber: ManualPageNumber = 0
Dim CoindoorIsOpen
Dim DoorReady: DoorReady = true
Dim WobbleBall
Dim ManualMove:ManualMove= 1
Dim CDA
Dim DoorMove:DoorMove=-1
Dim DCO:DCO=False
Dim MA:MA=False
' creates ball for the VR key wobble and manual float wobble
set Wobbleball=keykicker.createball
keykicker.Kick 180, 1

Sub CoindoorTimer_Timer() 
' shaking the key and manual float motion here.. full time,  The ball will hit the gate when WobbleBall.vely is called.
    VR_KeyShake.rotx= -90 - Gate001.Currentangle / - 6	 
	VR_KeyShake.rotz= 0 - Gate002.Currentangle / - 6 
    VRManualInstructions.transy= 0 - Gate002.Currentangle / - 10
    ManualBinder.transy= 0 - Gate002.Currentangle / - 10
	'*************************************************************************************************************************************
	'                                         COINDOOR OPEN/CLOSE TIMER - RASCAL & RAWD
	'*************************************************************************************************************************************
	If DCO = True Then
		If DoorMove = -1 then VR_KeyShakeOpen.Visible = 1:VR_KeyShake.Visible = 0
		For Each CDA in CoindoorAll:CDA.RotY = CDA.RotY + DoorMove:Next    ' This is the main coin door animation command.  It moves all parts together in an ungrouped collection.
		If VR_CoinDoor.RotY <= -130 then DCO = False:DoorMove=1 :MA = true: WobbleBall.vely = -20    ' -130 is the door angle, set that here.
		If VR_CoinDoor.RotY >= 0 then DCO = False:DoorMove=-1:VR_KeyShakeOpen.Visible = 0:VR_KeyShake.Visible = 1 : DoorReady = true
	End If
	If MA = True Then
		VRManualInstructions.objrotx = VRManualInstructions.objrotx + ManualMove
		ManualBinder.objrotx = ManualBinder.objrotx + ManualMove
		if VRManualInstructions.objrotx > 164 Then MA = false: ManualMove = -1: DoorReady = true ' 164 is the instruction manual angle, set that here.
		if VRManualInstructions.objrotx < 1 Then MA = false: ManualMove = 1 : PlaySound "CoindoorClose": DCO = true: WobbleBall.vely = -20
	End If
	'*************************************************************************************************************************************
End Sub



' ***** Beer Bubble Code - Rawd *****
Sub BeerTimer_Timer()

Randomize(21)
BeerBubble1.z = BeerBubble1.z + Rnd(1)*0.5
if BeerBubble1.z > -771 then BeerBubble1.z = -955
BeerBubble2.z = BeerBubble2.z + Rnd(1)*1
if BeerBubble2.z > -768 then BeerBubble2.z = -955
BeerBubble3.z = BeerBubble3.z + Rnd(1)*1
if BeerBubble3.z > -768 then BeerBubble3.z = -955
BeerBubble4.z = BeerBubble4.z + Rnd(1)*0.75
if BeerBubble4.z > -774 then BeerBubble4.z = -955
BeerBubble5.z = BeerBubble5.z + Rnd(1)*1
if BeerBubble5.z > -771 then BeerBubble5.z = -955
BeerBubble6.z = BeerBubble6.z + Rnd(1)*1
if BeerBubble6.z > -774 then BeerBubble6.z = -955
BeerBubble7.z = BeerBubble7.z + Rnd(1)*0.8
if BeerBubble7.z > -768 then BeerBubble7.z = -955
BeerBubble8.z = BeerBubble8.z + Rnd(1)*1
if BeerBubble8.z > -771 then BeerBubble8.z = -955
End Sub



' ***************** VR Clock code below - THANKS RASCAL ******************
Dim CurrentMinute ' for VR clock 
' VR Clock code below....
Sub ClockTimer_Timer()
	Pminutes.RotAndTra2 = (Minute(Now())+(Second(Now())/100))*6
	Phours.RotAndTra2 = Hour(Now())*30+(Minute(Now())/2)
    Pseconds.RotAndTra2 = (Second(Now()))*6
	CurrentMinute=Minute(Now())
End Sub
 ' ********************** END CLOCK CODE   *********************************



'******************************************************
' 					LUT
'******************************************************



Dim LUTset, LutToggleSound
LutToggleSound = True
LoadLUT
SetLUT


'LUT selector timer

dim lutsetsounddir
sub LutSlctr_timer
	If lutsetsounddir = 1 And LutSet <> 15 Then
		Playsound "click", 0, 1, 0.1, 0.25 
	End If
	If lutsetsounddir = -1 And LutSet <> 15 Then
		Playsound "click", 0, 1, 0.1, 0.25 
	End If
	If LutSet = 15 Then
		Playsound "gun", 0, 1, 0.1, 0.25 
	End If
	LutSlctr.enabled = False
end sub


Sub SetLUT
	Table1.ColorGradeImage = "LUT" & LUTset
end sub 

Sub LUTBox_Timer
	LUTBox.TimerEnabled = 0 
	LUTBox.Visible = 0
    LUTBack.visible = 0
	VRLutdesc.visible = 0
End Sub

Sub ShowLUT
	LUTBox.visible = 1
    LUTBack.visible = 1
    VRLutdesc.visible = 1

	Select Case LUTSet
        Case 0: LUTBox.text = "Fleep Natural Dark 1": VRLUTdesc.imageA = "LUTcase0"
		Case 1: LUTBox.text = "Fleep Natural Dark 2": VRLUTdesc.imageA = "LUTcase1"
		Case 2: LUTBox.text = "Fleep Warm Dark": VRLUTdesc.imageA = "LUTcase2"
		Case 3: LUTBox.text = "Fleep Warm Bright": VRLUTdesc.imageA = "LUTcase3"
		Case 4: LUTBox.text = "Fleep Warm Vivid Soft": VRLUTdesc.imageA = "LUTcase4"
		Case 5: LUTBox.text = "Fleep Warm Vivid Hard": VRLUTdesc.imageA = "LUTcase5"
		Case 6: LUTBox.text = "Skitso Natural and Balanced": VRLUTdesc.imageA = "LUTcase6"
		Case 7: LUTBox.text = "Skitso Natural High Contrast": VRLUTdesc.imageA = "LUTcase7"
		Case 8: LUTBox.text = "3rdaxis Referenced THX Standard": VRLUTdesc.imageA = "LUTcase8"
		Case 9: LUTBox.text = "CalleV Punchy Brightness and Contrast": VRLUTdesc.imageA = "LUTcase9"
		Case 10: LUTBox.text = "HauntFreaks Desaturated" : VRLUTdesc.imageA = "LUTcase10"
  		Case 11: LUTBox.text = "Tomate washed out": VRLUTdesc.imageA = "LUTcase11"
        Case 12: LUTBox.text = "VPW original 1on1": VRLUTdesc.imageA = "LUTcase12"
        Case 13: LUTBox.text = "bassgeige": VRLUTdesc.imageA = "LUTcase13"
        Case 14: LUTBox.text = "blacklight": VRLUTdesc.imageA = "LUTcase14"
        Case 15: LUTBox.text = "B&W Comic Book": VRLUTdesc.imageA = "LUTcase15"
	End Select

	LUTBox.TimerEnabled = 1
End Sub

Sub SaveLUT
	Dim FileObj
	Dim ScoreFile

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		Exit Sub
	End if

	if LUTset = "" then LUTset = 12 'failsafe

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "PinbotLUT.txt",True)
	ScoreFile.WriteLine LUTset
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub

Sub LoadLUT
	Dim FileObj, ScoreFile, TextStr
	dim rLine

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		LUTset=12
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "PinbotLUT.txt") then
		LUTset=12
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "PinbotLUT.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		rLine = TextStr.ReadLine
		If rLine = "" then
			LUTset=12
			Exit Sub
		End if
		LUTset = int (rLine) 
		Set ScoreFile = Nothing
	    Set FileObj = Nothing
End Sub


Dim Object
If VRRoom = 1 Then
	Ramp16.visible = 0
	Ramp15.visible = 0
	Ramp17.visible = 0
	Ramp2.visible = 1
	Ramp5.visible = 1
	outerrVR.visible = 1
	outerlVR.visible = 1
	outerlVR1.visible = 1
	outerr.visible = 0
	outerl.visible = 0
	For each Object in VrStuff : object.visible = 1 : Next
	For each Object in Coindoorall : object.visible = 1 : Next
	BGDark.visible = 1
End If
If VRRoom = 0 Then
	If DesktopMode = True Then 'Show Desktop components
		Ramp16.visible=0
		Ramp15.visible=0
	Else
		Ramp16.visible=0
		Ramp15.visible=0
	End if
	Ramp17.visible=1
	Ramp2.visible = 0
	Ramp5.visible = 0
	outerrVR.visible = 0
	outerlVR.visible = 0
	outerlVR1.visible = 0
	For each Object in VrStuff : object.visible = 0 : Next
	For each Object in Coindoorall : object.visible = 0 : Next
	displaytimer.enabled = 0
	Clocktimer.enabled = 0
	BeerTimer.enabled = 0
End If

If PFGlass = 1 Then
	Windowglass.visible = 1
Else
	Windowglass.visible = 0
End If

If Scratches = 1 Then
	GlassImpurities.visible = 1
Else
	GlassImpurities.visible = 0
End If
