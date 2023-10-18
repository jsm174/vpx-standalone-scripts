'
' 	 ::::::::      :::     :::            :::     :::    ::: :::   ::: 
'	:+:    :+:   :+: :+:   :+:          :+: :+:   :+:    :+: :+:   :+: 
'	+:+         +:+   +:+  +:+         +:+   +:+   +:+  +:+   +:+ +:+  
'	:#:        +#++:++#++: +#+        +#++:++#++:   +#++:+     +#++:   
'	+#+   +#+# +#+     +#+ +#+        +#+     +#+  +#+  +#+     +#+    
'	#+#    #+# #+#     #+# #+#        #+#     #+# #+#    #+#    #+#    
' 	 ########  ###     ### ########## ###     ### ###    ###    ###    
' 	
'	Galaxy / IPD No. 980 / January, 1980 / 4 Players
' 	Created by Bord 	

Option Explicit
Randomize
 
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

'----- Volume Options -----
Const BallRollVolume = 0.9
Const Volumedial = 0.8

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
'									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
'									'2 = flasher image shadow, but it moves like ninuzzu's

'Outlane adjustments (seriously, don't do this):
Dim Dullard: Dullard=0
Dim Coward: Coward=0








'*******************************************
'  Constants and Global Variables
'*******************************************

Const BallSize = 50					'Ball size must be 50
Const BallMass = 1					'Ball mass must be 1
Const tnob = 5						'Total number of balls
Const lob = 0						'Locked balls

Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height

LoadVPM "01000100", "Stern.VBS", 3.26

'  Standard definitions
'******************************************************
'Const cGameName="galaxy",UseSolenoids=2,UseLamps=0,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff",SFlipperOn="fx_Flipperup",SFlipperOff="fx_Flipperdown"
'Const SCoin="coin"
Const cGameName = "galaxy"
Const UseSolenoids = 2			'1=Normal Flippers, 2 = Fastflips
Const UseLamps = 0
Const UseSync = 0
Const HandleMech = 0
Const SSolenoidOn = ""
Const SSolenoidOff = ""
Const SFlipperOn = ""
Const SFlipperOff = ""
Const SCoin = ""
Const UseGI=0

Dim DesktopMode: DesktopMode = Table1.ShowDT
Dim hiddenvalue, DTThings
If DesktopMode = True Then 'Show Desktop components
	Ramp16.visible=1
	Ramp15.visible=1
	hiddenvalue=0
	For each DTThings in DTStuff : DTThings.visible = 1 : Next
Else
	Ramp16.visible=0
	Ramp15.visible=0
	hiddenvalue=1
	For each DTThings in DTStuff : DTThings.visible = 0 : Next
End if

Dim VR_Obj

Dim VR_Room
' VR Auto-Detect
If RenderingMode = 2 Then 
	VR_Room = 1
Else
	VR_Room = 0
End If

If VR_Room = 1 Then
	For Each VR_Obj in VRCabinet : VR_Obj.Visible = 1 : Next
	For Each VR_Obj in VRRoom : VR_Obj.Visible = 1 : Next
	SetBackglass
	Ramp15.Visible = 0
	Ramp16.Visible = 0
	For each DTThings in DTStuff : DTThings.visible = 0 : Next
End If

Dim EnableBallControl
EnableBallControl = false 'Change to true to enable manual ball control (or press C in-game) via the arrow keys and B (boost movement) keys

'BSize = 25
'BMass = 1.4

'Solenoid Call backs
'**********************************************************************************************************
'SolCallback(1)	=  rsling
'SolCallback(2)	= lsling
'SolCallback(3)	= topbump
SolCallback(4)	= "ResetDropT"	'"dtdrop"
SolCallback(5)	= "solleftout"
SolCallback(6)  = "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
'SolCallback(7)	= rbump
SolCallback(8)	= "SolTopSaucer"
SolCallback(9)	= "drop1"
SolCallback(10) = "drop2"
SolCallback(11)	= "solrelease"	'"bstrough.solout"
'SolCallback(12) = lbump
SolCallback(13) = "drop3"
SolCallback(14) = "drop4"
SolCallback(15)	= "BRelease"
SolCallback(17) = "PFGI"
SolCallback(19)	= "vpmNudge.SolGameOn"
'SolCallback(sLLFlipper)="vpmSolFlipper LeftFlipper,nothing,"
'SolCallback(sLRFlipper)="vpmSolFlipper RightFlipper,nothing,"

	SolCallback(sLRFlipper) = "SolRFlipper"
	SolCallback(sLLFlipper) = "SolLFlipper"

	
'Solenoid Controlled toys
'**********************************************************************************************************		
Sub Drop1(enabled) : If enabled Then: DTDrop 4 :sw4_s.visible=False: controller.switch(4) = 1 : End If : End Sub
Sub Drop2(enabled) : If enabled Then: DTDrop 11 :sw11_s.visible=False: controller.switch(11) = 1 : End If : End Sub
Sub Drop3(enabled) : If enabled Then: DTDrop 10 :sw10_s.visible=False: controller.switch(10) = 1 : End If : End Sub
Sub Drop4(enabled) : If enabled Then: DTDrop 9 :sw9_s.visible=False: controller.switch(9) = 1 : End If : End Sub

'Sub Drop1(enabled) : If enabled Then: DTHit 4: End If : End Sub
'Sub Drop2(enabled) : If enabled Then: DTHit 11: End If : End Sub
'Sub Drop3(enabled) : If enabled Then: DTHit 10: End If : End Sub
'Sub Drop4(enabled) : If enabled Then: DTHit 9: End If : End Sub


'Sub DTdrop(enabled)
'	if enabled then
'		dtL.SolDropUp enabled
'	end if
'End Sub

'Playfield GI
Sub PFGI(Enabled)
	If Enabled Then
'		GiOFF
		gilvl = 0
		dim xx
		For each xx in GI1:xx.State = 0: Next
		If VR_Room = 1 Then For each xx in VRBGGI:xx.Visible = 0: Next
        PlaySound "fx_relay"
		If VR_Room = 0 Then
			Table1.ColorGradeImage = "ColorGradeLUT256x16_shadowcrush"
		End If
		flasher1.visible=0
		flasher2.visible=1
		plungeguide_prim.blenddisablelighting = 0.05
		bumpercap1_prim.blenddisablelighting = 0.05
		bumpercap2_prim.blenddisablelighting = 0.05
		bumpercap3_prim.blenddisablelighting = 0.05
		bumpercap1_prim.image = "bump3off"
		bumpercap2_prim.image = "bump3off"
		bumpercap3_prim.image = "bump3off"
		plasticedges_prim.blenddisablelighting = 0
		plasticedgeslow_prim.blenddisablelighting = 0
		flipperright_prim.blenddisablelighting = 0
		flipperleft_prim.blenddisablelighting = 0
		sw4p.blenddisablelighting = 0
		sw4p.image = "drop1off"
		sw11p.blenddisablelighting = 0
		sw11p.image = "drop2off"
		sw10p.blenddisablelighting = 0
		sw10p.image = "drop3off"
		sw9p.blenddisablelighting = 0
		sw9p.image = "drop4off"
		sw26p.blenddisablelighting = 0
		sw29p.blenddisablelighting = 0
		sw30p.blenddisablelighting = 0
		outers_prim.blenddisablelighting = 0
		innerwood_prim.blenddisablelighting = 0
		sw4_s.visible=False
		sw11_s.visible=False
		sw10_s.visible=False
		sw9_s.visible=False
	Else
'		GiON
		gilvl = 1
		For each xx in GI1:xx.State = 1: Next
		If VR_Room = 1 Then For each xx in VRBGGI:xx.Visible = 1: Next
        PlaySound "fx_relay"
		If VR_Room = 0 Then
			Table1.ColorGradeImage = "ColorGradeLUT256x16_1to1"
		End IF
		flasher1.visible=1
		flasher2.visible=0
		plungeguide_prim.blenddisablelighting = 0.4
		bumpercap1_prim.blenddisablelighting = 0
		bumpercap2_prim.blenddisablelighting = 0
		bumpercap3_prim.blenddisablelighting = 0
		bumpercap1_prim.image = "bump3on"
		bumpercap2_prim.image = "bump3on"
		bumpercap3_prim.image = "bump3on"
		plasticedges_prim.blenddisablelighting = 0.7
		plasticedgeslow_prim.blenddisablelighting = 0.7
		flipperright_prim.blenddisablelighting = 0.25
		flipperleft_prim.blenddisablelighting = 0.25
		sw4p.blenddisablelighting = 0.9
		sw4p.image = "drop1"
		sw11p.blenddisablelighting = 0.9
		sw11p.image = "drop2"
		sw10p.blenddisablelighting = 0.9
		sw10p.image = "drop3"
		sw9p.blenddisablelighting = 0.9
		sw9p.image = "drop4"
		sw26p.blenddisablelighting = 0.3
		sw29p.blenddisablelighting = 0.3
		sw30p.blenddisablelighting = 0.3
		outers_prim.blenddisablelighting = 0.8
		innerwood_prim.blenddisablelighting = 0.8
		sw4_s.visible=True
		sw11_s.visible=True
		sw10_s.visible=True
		sw9_s.visible=True
	end if 
End Sub



'  Timers
'******************************************************
Sub GameTimer_Timer()
	Cor.Update 						'update ball tracking
	RollingUpdate					'update rolling sounds
	DoSTAnim
	DoDTAnim
	If controller.switch(4)=-1 then sw4_s.visible=False 
	If controller.switch(11)=-1 then sw11_s.visible=False
	If controller.switch(10)=-1 then sw10_s.visible=False
	If controller.switch(9)=-1 then sw9_s.visible=False
End Sub

Dim FrameTime, InitFrameTime
InitFrameTime = 0
Sub FrameTimer_Timer() 'The frame timer interval should be -1, so executes at the display frame rate
	FrameTime = gametime - InitFrameTime
	InitFrameTime = gametime	'Count frametime
	FlipperVisualUpdate		 'update flipper shadows and primitives
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
	UpdateGatesSpinners
End Sub



'Initiate Table
'**********************************************************************************************************
 
Dim GBall1, BOT, bskicker, bssaucer, dtL
 
Sub Table1_Init
	vpmInit Me
	DTsOff
	Set GBall1 = Drain.CreateSizedballWithMass(Ballsize/2,Ballmass)
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Galaxy (Stern 1980)"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = hiddenvalue
		If Err Then MsgBox Err.Description
	End With
	On Error Goto 0
'	Controller.SolMask(0)=0
'   vpmTimer.AddTimer 2000,"Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the timer to renable all the solenoids after 2 seconds
	Controller.Run
	If Err Then MsgBox Err.Description
	On Error Goto 0

	'Main Timer init
	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled=1

	'Nudging
	vpmNudge.TiltSwitch = 7
	vpmNudge.Sensitivity = 2
	vpmNudge.TiltObj = Array(Bumper1,Bumper2,LeftSlingshot,RightSlingshot)
 
'   	Set bsTrough=New cvpmBallStack
' 	with bsTrough
'		.InitSw 0,33,0,0,0,0,0,0
'		.InitKick BallRelease,90,7
'		.InitExitSnd Soundfx("popper",DOFContactors), Soundfx("solenoid",DOFContactors)
'		.Balls=1
' 	end with
    BOT = Array(GBall1)
' 	Set dtL=New cvpmDropTarget
'		dtL.InitDrop Array(sw4,sw11,sw10,sw9),Array(4,11,10,9)
'		dtL.InitSnd SoundFX("DTDrop",DOFDropTargets),SoundFX("DTReset",DOFContactors)

	sw4p.blenddisablelighting = 0.1
	sw11p.blenddisablelighting = 0.1
	sw10p.blenddisablelighting = 0.1
	sw9p.blenddisablelighting = 0.1
	dim xx
	For each xx in Rubbers:xx.blenddisablelighting = 0: Next
End Sub

Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub
Sub Table1_exit()
	Controller.Pause = False
	Controller.Stop
End Sub







'	Key Press Handling
'******************************************************

Sub Table1_KeyDown(ByVal KeyCode)
'	If KeyDownHandler(keycode) Then Exit Sub
	If Keycode = LeftFlipperKey Then
		FlipperActivate LeftFlipper, LFPress
		VR_CabFlipperLeft.X = VR_CabFlipperLeft.X + 10
	End If

	If Keycode = RightFlipperKey Then
		FlipperActivate RightFlipper, RFPress
		VR_CabFlipperRight.X = VR_CabFlipperRight.X - 10
	End If
	
	If keycode = PlungerKey Then
		Plunger.PullBack
		SoundPlungerPull
		TimerPlunger.Enabled = True
		TimerPlunger2.Enabled = False
	End If

	If keycode = LeftTiltKey Then Nudge 90, 1 : SoundNudgeLeft
	If keycode = RightTiltKey Then Nudge 270, 1 : SoundNudgeRight
	If keycode = CenterTiltKey Then Nudge 0, 1 : SoundNudgeCenter

	If keycode = StartGameKey Then
		SoundStartButton
		VR_Cab_StartButton.y = VR_Cab_StartButton.y - 5	
	End If

	If keycode = AddCreditKey or keycode = AddCreditKey2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

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
	If vpmKeyDown(keycode) Then Exit Sub  
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
'	If KeyUpHandler(keycode) Then Exit Sub
	If Keycode = LeftFlipperKey Then
		FlipperDeActivate LeftFlipper, LFPress
		VR_CabFlipperLeft.X = VR_CabFlipperLeft.X - 10
	End If

	If Keycode = RightFlipperKey Then
		FlipperDeActivate RightFlipper, RFPress
		VR_CabFlipperRight.X = VR_CabFlipperRight.X + 10
	End If

	If keycode = PlungerKey Then
		Plunger.Fire
		SoundPlungerReleaseBall
		TimerPlunger.Enabled = False
		TimerPlunger2.Enabled = True
		VR_Primary_plunger.Y = 2115
	End If

	If Keycode = StartGameKey Then	
		VR_Cab_StartButton.y = VR_Cab_StartButton.y + 5
	End If

    'Manual Ball Control
	If EnableBallControl = 1 Then
		If keycode = 203 Then BCleft = 0	' Left Arrow
		If keycode = 200 Then BCup = 0		' Up Arrow
		If keycode = 208 Then BCdown = 0	' Down Arrow
		If keycode = 205 Then BCright = 0	' Right Arrow
	End If
	If vpmKeyUp(keycode) Then Exit Sub
End Sub


' switches
'**********************************************************************************************************

'  				Drain
'*******************************************

Sub Drain_Hit 
	RandomSoundDrain Drain
	Controller.Switch(33) = 1
End Sub

Sub SolRelease(enabled)
	If enabled Then 
		RandomSoundBallRelease Drain
		Drain.kick 60, 15
		Controller.Switch(33) = 0
	End If
End Sub


'kicker
'Sub sw40_Hit
'	playsound "kickerenter"
'	Controller.Switch(40) = 1
'	topsaucersafetytimer.enabled=1
'End Sub
'
'kicker safety in case kickout stalls
'Sub topsaucersafetytimer_timer
'	sw40.Kick 100, 15 + 5 * Rnd
'testing	playsound "bell"
'	topsaucersafetytimer.enabled=0
'End Sub

'******************************************************
'						Saucer
'******************************************************

dim k1step, k2step, k3step

Sub KickBall(kball, kangle, kvel, kvelz, kzlift)
	dim rangle
	rangle = PI * (kangle - 90) / 180

	kball.z = kball.z + kzlift
	kball.velz = kvelz
	kball.velx = cos(rangle)*kvel
	kball.vely = sin(rangle)*kvel
End Sub


Dim KickerBall

'
Sub sw40_Hit
	set KickerBall = activeball	
	Controller.Switch(40) = 1
	SoundSaucerLock 
End Sub

Sub sw40_unHit
	Controller.Switch(40) = 0
	SoundSaucerKick 1, sw40
End Sub

dim kickstep1

Sub SolTopSaucer(enabled)
    if enabled then
		If sw40.ballcntover > 0 then
			KickBall KickerBall, 110, 14, 5, 30
		End If
'		cupun2.rotx = -25
		k3step=0
		kicktoptimer.enabled=1
    end if
End Sub

Sub kicktoptimer_Timer
    Select Case K3Step
'        Case 3:cupun2.rotx = -25
'        Case 4:cupun2.rotx = -25
'        Case 5:cupun2.rotx = -25
'        Case 6:cupun2.rotx = -25
'        Case 7:cupun2.rotx = -25
'        Case 8:cupun2.rotx = -18
'        Case 9:cupun2.rotx = -11
'        Case 10:cupun2.rotx = 0
        Case 11:Me.Enabled = 0: if sw40.ballcntover > 0 then SolTopSaucer -1'SolSaucer -1
    End Select
    k3Step = k3Step + 1
End Sub


'********************** Drop Targets ************************
Sub Sw4_Hit:DTHit 4: TargetBouncer Activeball, 1.5: End Sub
Sub Sw11_Hit:DTHit 11: TargetBouncer Activeball, 1.5 : End Sub
Sub Sw10_Hit:DTHit 10: TargetBouncer Activeball, 1.5 : End Sub
Sub Sw9_Hit:DTHit 9: TargetBouncer Activeball, 1.5 : End Sub

Sub ResetDropT(enabled)
	if enabled then
		PlaySoundAt SoundFX(DTResetSound,DOFContactors),sw4p
		DTRaise 4
		DTRaise 11
		DTRaise 10
		DTRaise 9
		sw4_s.visible=True
		sw11_s.visible=True
		sw10_s.visible=True
		sw9_s.visible=True
	end if
End Sub

Sub DTsOff
		sw4_s.visible=False
		sw11_s.visible=False
		sw10_s.visible=False
		sw9_s.visible=False
End Sub

'********************** Stand Up Targets ************************
 Sub sw26_hit :vpmTimer.PulseSW (26) :STHit 26:End Sub
 Sub sw29_hit :vpmTimer.PulseSW (29) :STHit 29:End Sub
 Sub sw30_hit :vpmTimer.PulseSW (30) :STHit 30:End Sub

'Spinner
'**************************************
Sub sw5_Spin : vpmTimer.PulseSw (5) : SoundSpinner sw5: End Sub

Sub UpdateGatesSpinners
    SpinnerT1.RotX = -(sw5.currentangle)
	'90 -> 6, 0 -> 0, 180 -> 0, 270 -> 6
	SpinnerTShadow.size_y = abs(sin( (sw5.CurrentAngle+180) * (2*PI/360)) * 6)+2

'	debug.print sw5.currentangle & " -> " & SpinnerTShadow.size_y

    pSpinnerRod.TransX = sin( (sw5.CurrentAngle+180) * (2*PI/360)) * 6
    pSpinnerRod.TransY = sin( (sw5.CurrentAngle- 90) * (2*PI/360)) * 6   
End Sub


'Bumpers
  Sub Bumper1_Hit:vpmTimer.PulseSw 12 : RandomSoundBumperTop Bumper1: End Sub
  Sub Bumper2_Hit:vpmTimer.PulseSw 13 : RandomSoundBumperMiddle Bumper2: End Sub
  Sub Bumper3_Hit:vpmTimer.PulseSw 14 : RandomSoundBumperBottom Bumper3: End Sub

 'Star Rollovers
 Sub sw18_Hit   : Controller.Switch(18) = 1 : End Sub
 Sub sw18_UnHit : Controller.Switch(18) = 0 : End Sub
 Sub sw18a_Hit  : Controller.Switch(18) = 1 : End Sub
 Sub sw18a_UnHit: Controller.Switch(18) = 0 : End Sub
 Sub sw23_Hit   : Controller.Switch(23) = 1 : End Sub
 Sub sw23_UnHit : Controller.Switch(23) = 0 : End Sub
 Sub sw24_Hit   : Controller.Switch(24) = 1 : End Sub
 Sub sw24_UnHit : Controller.Switch(24) = 0 : End Sub
 Sub sw39_Hit   : Controller.Switch(39) = 1 : End Sub
 Sub sw39_UnHit : Controller.Switch(39) = 0 : End Sub

 'Wire Triggers
 Sub sw19_Hit   : Controller.Switch(19) = 1 : End Sub
 Sub sw19_UnHit : Controller.Switch(19) = 0 : End Sub
 Sub sw20_Hit   : Controller.Switch(20) = 1 : End Sub
 Sub sw20_UnHit : Controller.Switch(20) = 0 : End Sub
 Sub sw21_Hit   : Controller.Switch(21) = 1 : End Sub
 Sub sw21_UnHit : Controller.Switch(21) = 0 : End Sub
 Sub sw22_Hit  : Controller.Switch(22) = 1 : End Sub
 Sub sw22_UnHit: Controller.Switch(22) = 0 : End Sub
 Sub sw35_Hit  : Controller.Switch(35) = 1 : leftsaucersafetytimer.enabled = 1:End Sub
 Sub sw35_UnHit: Controller.Switch(35) = 0 : End Sub
 Sub sw36_Hit  : Controller.Switch(36) = 1 : End Sub
 Sub sw36_UnHit: Controller.Switch(36) = 0 : End Sub
 Sub sw37_Hit  : Controller.Switch(37) = 1 : End Sub
 Sub sw37_UnHit: Controller.Switch(37) = 0 : End Sub
 Sub sw38_Hit  : Controller.Switch(38) = 1 : End Sub
 Sub sw38_UnHit: Controller.Switch(38) = 0 : End Sub


'	FLIPPERS
'******************************************************
Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire
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
		RF.Fire
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

'Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub

Sub FlipperVisualUpdate 'This subroutine updates the flipper shadows and visual primitives
	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperRSh.RotZ = RightFlipper.CurrentAngle
    flipperleft_prim.rotz = leftflipper.currentangle
    flipperright_prim.rotz = rightflipper.currentangle
'	LFLogo.RotZ = LeftFlipper.CurrentAngle
'	RFlogo.RotZ = RightFlipper.CurrentAngle
End Sub





'******************************************************
' 	ZLMP:  LAMPZ by nFozzy
'******************************************************
' 
' Lampz is a utility designed to manage and fade the lights and light-related objects on a table that is being driven by a ROM.
' To set up Lampz, one must populate the Lampz.MassAssign array with VPX Light objects, where the index of the MassAssign array
' corrisponds to the ROM index of the associated light. More that one Light object can be associated with a single MassAssign index (not shown in this example)
' Optionally, callbacks can be assigned for each index using the Lampz.Callback array. This is very useful for allowing 3D Insert primitives
' to be controlled by the ROM. Note, the aLvl parameter (i.e. the fading level that ranges between 0 and 1) is appended to the callback call.

Dim NullFader
Set NullFader = New NullFadingObject
Dim Lampz
Set Lampz = New LampFader
InitLampsNF				  ' Setup lamp assignments
LampTimer.Interval =  - 1
LampTimer.Enabled = 1

Sub LampTimer_Timer()
		Dim x, chglamp
		chglamp = Controller.ChangedLamps
		If Not IsEmpty(chglamp) Then
			For x = 0 To UBound(chglamp) 'nmbr = chglamp(x, 0), state = chglamp(x, 1)
				Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
			Next
		End If
	Lampz.Update2 'update (fading logic only)
	If VR_Room = 0 Then UpdateLeds
End Sub

Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	If Lampz.UseFunc Then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub

Sub SetModLamp(id, val)
	Lampz.state(id) = val
End Sub

Sub InitLampsNF()
	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating
	
	'Adjust fading speeds (max level / full MS fading time). The Modulate property must be set to 1 / max level if lamp is modulated.
	Dim x
	For x = 0 To 150
		Lampz.FadeSpeedUp(x) = 1 / 40
		Lampz.FadeSpeedDown(x) = 1 / 120
		Lampz.Modulate(x) = 1
	Next

	'One can assign some individual lamp fading speeds like this. This example is to make GI fading to appear slower
	'Lampz.FadeSpeedUp(110) = 1 / 60 : Lampz.FadeSpeedDown(110) = 1 / 220 : Lampz.Modulate(110) = 1

	'Lampz Assignments
	'  In a ROM based table, the lamp ID is used to set the state of the Lampz objects
	
	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays
	Lampz.MassAssign(1) = l1
'	Lampz.Callback(1) = "DisableLighting p1, 200,"
	Lampz.MassAssign(1) = l1a
	Lampz.MassAssign(1) = l1z
	Lampz.MassAssign(2) = l2
'	Lampz.Callback(2) = "DisableLighting p2, 200,"
	Lampz.MassAssign(2) = l2a
	Lampz.MassAssign(2) = l2z
	Lampz.MassAssign(3) = l3
	Lampz.MassAssign(3) = l3a
	Lampz.MassAssign(3) = l3z
	Lampz.MassAssign(4) = l4
'	Lampz.Callback(4) = "DisableLighting p4, 200,"
	Lampz.MassAssign(4) = l4a
	Lampz.MassAssign(4) = l4z
	Lampz.MassAssign(5) = l5
'	Lampz.Callback(5) = "DisableLighting p5, 200,"
	Lampz.MassAssign(5) = l5a
	Lampz.MassAssign(5) = l5z
	Lampz.MassAssign(6) = l6
	Lampz.MassAssign(6) = l6a
'	Lampz.Callback(6) = "DisableLighting p6, 200,"
	Lampz.MassAssign(7) = l7
	Lampz.MassAssign(7) = l7a
'	Lampz.Callback(7) = "DisableLighting p7, 200,"
	Lampz.MassAssign(8) = l8
	Lampz.MassAssign(8) = l8a
'	Lampz.Callback(8) = "DisableLighting p8, 200,"
	Lampz.MassAssign(9) = l9
	Lampz.MassAssign(9) = l9a
'	Lampz.Callback(9) = "DisableLighting p9, 200,"

	Lampz.MassAssign(10) = l10
	Lampz.MassAssign(10) = l10a
'	Lampz.Callback(10) = "DisableLighting p10, 200,"
	Lampz.MassAssign(11) = l11
	Lampz.MassAssign(11) = l11a
'	Lampz.Callback(11) = "DisableLighting p11, 200,"
	Lampz.MassAssign(12) = l12
	Lampz.MassAssign(12) = l12a
'	Lampz.Callback(12) = "DisableLighting p12, 130,"
'	Lampz.MassAssign(13) = l13
'	Lampz.Callback(13) = "DisableLighting p13, 250,"
'	Lampz.MassAssign(14) = l14
'	Lampz.Callback(14) = "DisableLighting p14, 200,"
'	Lampz.MassAssign(15) = l15
'	Lampz.Callback(15) = "DisableLighting p15, 200,"
'	Lampz.MassAssign(16) = l16
'	Lampz.Callback(16) = "DisableLighting p16, 200,"
	Lampz.MassAssign(17) = l17
	Lampz.MassAssign(17) = l17a
'	Lampz.Callback(17) = "DisableLighting p17, 250,"
	Lampz.MassAssign(18) = l18
	Lampz.MassAssign(18) = l18a
''	Lampz.Callback(18) = "DisableLighting p18, 50,"
	Lampz.MassAssign(19) = l19
	Lampz.MassAssign(19) = l19a
'	Lampz.Callback(19) = "DisableLighting p19, 200,"

	Lampz.MassAssign(20) = l20
	Lampz.MassAssign(20) = l20a
'	Lampz.Callback(20) = "DisableLighting p20, 200,"
	Lampz.MassAssign(21) = l21
	Lampz.MassAssign(21) = l21a
'	Lampz.Callback(21) = "DisableLighting p21, 200,"
	Lampz.MassAssign(22) = l22
	Lampz.MassAssign(22) = l22a
'	Lampz.Callback(22) = "DisableLighting p22, 200,"
	Lampz.MassAssign(23) = l23
	Lampz.MassAssign(23) = l23a
'	Lampz.Callback(23) = "DisableLighting p23, 200,"
	Lampz.MassAssign(24) = l24
	Lampz.MassAssign(24) = l24a
'	Lampz.Callback(24) = "DisableLighting p24, 200,"
	Lampz.MassAssign(25) = l25
	Lampz.MassAssign(25) = l25a
'	Lampz.Callback(25) = "DisableLighting p25, 200,"
	Lampz.MassAssign(26) = l26
	Lampz.MassAssign(26) = l26a
'	Lampz.Callback(26) = "DisableLighting p26, 200,"
	Lampz.MassAssign(27) = l27
	Lampz.MassAssign(27) = l27a
	Lampz.MassAssign(27) = l27b
	Lampz.MassAssign(27) = l27c
'	Lampz.Callback(27) = "DisableLighting p27, 200,"
	Lampz.MassAssign(28) = l28
	Lampz.MassAssign(28) = l28a
'	Lampz.Callback(28) = "DisableLighting p28, 200,"
'	Lampz.MassAssign(29) = l29
'	Lampz.Callback(29) = "DisableLighting p29, 200,"

'	Lampz.MassAssign(30) = l30
'	Lampz.Callback(30) = "DisableLighting p30, 200,"
'	Lampz.MassAssign(31) = l31
'	Lampz.Callback(31) = "DisableLighting p31, 200,"
'	Lampz.MassAssign(32) = l32
'	Lampz.Callback(32) = "DisableLighting p32, 200,"
	Lampz.MassAssign(33) = l33
	Lampz.MassAssign(33) = l33a
'	Lampz.Callback(33) = "DisableLighting p33, 200,"
	Lampz.MassAssign(34) = l34
	Lampz.MassAssign(34) = l34a
'	Lampz.Callback(34) = "DisableLighting p34, 200,"
	Lampz.MassAssign(35) = l35
	Lampz.MassAssign(35) = l35a
'	Lampz.Callback(35) = "DisableLighting p35, 200,"
	Lampz.MassAssign(36) = l36
	Lampz.MassAssign(36) = l36a
'	Lampz.Callback(36) = "DisableLighting p36, 200,"
	Lampz.MassAssign(37) = l37
	Lampz.MassAssign(37) = l37a
'	Lampz.Callback(37) = "DisableLighting p37, 200,"
	Lampz.MassAssign(38) = l38
	Lampz.MassAssign(38) = l38a
'	Lampz.Callback(38) = "DisableLighting p38, 200,"
	Lampz.MassAssign(39) = l39
	Lampz.MassAssign(39) = l39a
'	Lampz.Callback(39) = "DisableLighting p39, 200,"
	
	Lampz.MassAssign(40) = l40
	Lampz.MassAssign(40) = l40a
'	Lampz.Callback(40) = "DisableLighting p40, 200,"
	Lampz.MassAssign(41) = l41
	Lampz.MassAssign(41) = l41a
'	Lampz.Callback(41) = "DisableLighting p41, 200,"
	Lampz.MassAssign(42) = l42
	Lampz.MassAssign(42) = l42a
'	Lampz.Callback(42) = "DisableLighting p42, 200,"
	Lampz.MassAssign(43) = l43
	Lampz.MassAssign(43) = l43a
'	Lampz.Callback(43) = "DisableLighting p43, 200,"
	Lampz.MassAssign(44) = l44
	Lampz.MassAssign(44) = l44a
'	Lampz.Callback(44) = "DisableLighting p44, 200,"
'	Lampz.MassAssign(45) = l45
'	Lampz.Callback(45) = "DisableLighting p45, 200,"
'	Lampz.MassAssign(46) = l46
'	Lampz.Callback(46) = "DisableLighting p46, 200,"
'	Lampz.MassAssign(47) = l47
'	Lampz.Callback(47) = "DisableLighting p47, 200,"
'	Lampz.MassAssign(48) = l48
'	Lampz.Callback(48) = "DisableLighting p48, 200,"
	Lampz.MassAssign(49) = l49
	Lampz.MassAssign(49) = l49a
'	Lampz.Callback(49) = "DisableLighting p49, 200,"

	Lampz.MassAssign(50) = l50
	Lampz.MassAssign(50) = l50a
'	Lampz.Callback(50) = "DisableLighting p50, 200,"
	Lampz.MassAssign(51) = l51
	Lampz.MassAssign(51) = l51a
'	Lampz.Callback(51) = "DisableLighting p51, 200,"
	Lampz.MassAssign(52) = l52
	Lampz.MassAssign(52) = l52a
'	Lampz.Callback(52) = "DisableLighting p52, 200,"
	Lampz.MassAssign(53) = l53
	Lampz.MassAssign(53) = l53a
'	Lampz.Callback(53) = "DisableLighting p53, 200,"
	Lampz.MassAssign(54) = l54
	Lampz.MassAssign(54) = l54a
'	Lampz.Callback(54) = "DisableLighting p54, 200,"
	Lampz.MassAssign(55) = l55
	Lampz.MassAssign(55) = l55a
'	Lampz.Callback(55) = "DisableLighting p55, 200,"
	Lampz.MassAssign(56) = l56
	Lampz.MassAssign(56) = l56a
'	Lampz.Callback(56) = "DisableLighting p56, 200,"
	Lampz.MassAssign(57) = l57
	Lampz.MassAssign(57) = l57a
'	Lampz.Callback(57) = "DisableLighting p57, 200,"
	Lampz.MassAssign(58) = l58
	Lampz.MassAssign(58) = l58a
'	Lampz.Callback(58) = "DisableLighting p58, 200,"
	Lampz.MassAssign(59) = l59
	Lampz.MassAssign(59) = l59a
'	Lampz.Callback(59) = "DisableLighting p59, 200,"
	Lampz.MassAssign(60) = l60
	Lampz.MassAssign(60) = l60a


	
	'Turn off all lamps on startup
	Lampz.Init	'This just turns state of any lamps to 1
	
	'Immediate update to turn on GI, turn off lamps
	Lampz.Update
End Sub


'******************************************************
'****  ZGIU:	GI Control
'******************************************************

'**** These are just debug commands. They emit same calls as what SolCallback will do. You may use these from debugger.
'Sub GIOn  : SetRelayGI False: End Sub
'Sub GIOff : SetRelayGI True : End Sub


'**** SetRelayGI is called from SolCallback. We define lamp #110 to act as a control channel for GI events.
'**** #110 can be any free index number. This number is referred in Lampz MassAssignment section above.
'sub SetRelayGI(Enabled)
'	msgbox Enabled
	'some tables have this solenoid reversed. So False may mean GION,
	'i.e. Sega and Data East GI lights are off when GI relay is on.
'	if Enabled then
'		Lampz.SetLamp 110, 0
'		FlBumperFadeTarget(1) = 1				'Pop Bumper Light On when GI is off
'		FlBumperFadeTarget(2) = 0				'Pop Bumper Light Off when GI is off
'		FlBumperFadeTarget(3) = 0				'Pop Bumper commented out, is now always on.
'	else
'		Lampz.SetLamp 110, 1
'		FlBumperFadeTarget(1) = 0				'Pop Bumper Light Off when GI is On
'		FlBumperFadeTarget(2) = 1				'Pop Bumper Light On when GI is On
'		FlBumperFadeTarget(3) = 1				'Pop Bumper commented out, is now always on.
'	end if
'End Sub

'**** Use these for GI strings and stepped GI, and comment out the SetRelayGI
'**** This example table uses Relay for GI control, we don't need these at all
'Set GICallback = GetRef("SetGI")		'some tables provides GI strings via this reference
'Set GICallback2 = GetRef("SetModGI")	'use this for stepped/modulated GI
	'Pinmame Controller -> core.vbs PinMameTimer Loop -> GIcallback2 ->  ModLampz dynamiclamps object -> object updates / more callbacks

'Sub SetGI(aNr, aValue)
''	msgbox "nr: " & aNr & " value: " & aValue
'	Lampz.SetLamp 110 + aNr, aValue
'End Sub
'
'Sub SetModGI(aNr, aValue)
''	msgbox "nr: " & aNr & " value: " & aValue
'	ModLampz.SetGI aNr, aValue
'End Sub


'**** Global variable to hold current GI light intensity. Not Mandatory, but may help in other lighting subs and timers
'**** This value is updated always when GI state is being updated
dim gilvl	

'**** GIupdates is called always when some event happens to GI channel. Not Mandatory, but left in use for example
Sub GIUpdates(aLvl)	

	'You may add any other GI related effects here. Like if you want to make some toy to appear more bright, set it like this:
	'Primitive001.blenddisablelighting = 1.2 * aLvl + 0.2	'This will result to DL brightness between 0.2 - 1.4 for ON/OFF states

	gilvl = aLvl		'Storing the latest GI fading state into global variable, so one can use it elsewhere too.
	'debug.print alvl	'just a debug line so that you see that fading happens correctly

End Sub

'******************************************************
'****  END GI Control
'******************************************************


'====================
'Class jungle nf
'====================

'No-op object instead of adding more conditionals to the main loop
'It also prevents errors if empty lamp numbers are called, and it's only one object
'should be g2g?
Class NullFadingObject
	Public Property Let IntensityScale(input)
		
	End Property
End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
' Note: if using multiple 'LampFader' objects, set the 'name' variable to avoid conflicts with callbacks
'Version 0.14 - Updated to support modulated signals - Niwak
'Version 0.15 - Added IsLight property - apophis

Class LampFader
	Public IsLight(150)
	Public FadeSpeedDown(150), FadeSpeedUp(150)
	Private Lock(150), Loaded(150), OnOff(150)
	Public UseFunc
	Private cFilter
	Public UseCallback(150), cCallback(150)
	Public Lvl(150), Obj(150)
	Private Mult(150)
	Public FrameTime
	Private InitFrame
	Public Name
	
	Sub Class_Initialize()
		InitFrame = 0
		Dim x
		For x = 0 To UBound(OnOff)	 'Set up fade speeds
			FadeSpeedDown(x) = 1 / 100	'fade speed down
			FadeSpeedUp(x) = 1 / 80		'Fade speed up
			UseFunc = False
			lvl(x) = 0
			OnOff(x) = 0
			Lock(x) = True
			Loaded(x) = False
			Mult(x) = 1
			IsLight(x) = False
		Next
		Name = "LampFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OF THESE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
		For x = 0 To UBound(OnOff)		 'clear out empty obj
			If IsEmpty(obj(x) ) Then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub
	
	Public Property Get Locked(idx)
		Locked = Lock(idx)
		'   debug.print Lampz.Locked(100)	'debug
	End Property
	
	Public Property Get state(idx)
		state = OnOff(idx)
	End Property
	
	Public Property Let Filter(String)
		Set cFilter = GetRef(String)
		UseFunc = True
	End Property
	
	Public Function FilterOut(aInput)
		If UseFunc Then 
			FilterOut = cFilter(aInput) 
		Else 
			FilterOut = aInput
		End If
	End Function
	
	'   Public Property Let Callback(idx, String)
	'	   cCallback(idx) = String
	'	   UseCallBack(idx) = True
	'   End Property
	
	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'   cCallback(idx) = String 'old execute method
		
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _
		
		Dim tmp
		tmp = Split(cCallback(idx), "___")
		
		Dim str, x
		For x = 0 To UBound(tmp)	'build proc contents
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x) = "" Then str = str & tmp(x) & " aLVL:"
		Next
		'   msgbox "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		Dim out
		out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		ExecuteGlobal Out
	End Property
	
	Public Property Let state(ByVal idx, input) 'Major update path
		If TypeName(input) <> "Double" And TypeName(input) <> "Integer"  And TypeName(input) <> "Long" Then
			If input Then
				input = 1
			Else
				input = 0
			End If
		End If
		If Input <> OnOff(idx) Then  'discard redundant updates
			OnOff(idx) = input
			Lock(idx) = False
			Loaded(idx) = False
		End If
	End Property
	
	'Sub MassAssign(aIdx, aInput)
	Public Property Let MassAssign(aIdx, aInput) 'Mass assign, Builds arrays where necessary
		If TypeName(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
			If IsArray(aInput) Then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
				If typename(aInput) = "Light" Then 
					IsLight(aIdx) = True
				End If
			End If
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		End If
	End Property
	
	Sub SetLamp(aIdx, aOn)  'If obj contains any light objects, set their states to 1 (Fading is our job!)
		state(aIdx) = aOn
	End Sub
	
	Public Sub TurnOnStates() 'turn state to 1
		Dim debugstr
		Dim idx
		For idx = 0 To UBound(obj)
			If IsArray(obj(idx)) Then
				'debugstr = debugstr & "array found at " & idx & "..."
				Dim x, tmp
				tmp = obj(idx) 'set tmp to array in order to access it
				For x = 0 To UBound(tmp)
					If TypeName(tmp(x)) = "Light" Then DisableState tmp(x)' : debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.001 ' this can prevent init stuttering
				Next
			Else
				If TypeName(obj(idx)) = "Light" Then DisableState obj(idx)' : debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.001 ' this can prevent init stuttering
			End If
		Next
		'   debug.print debugstr
	End Sub
	
	Private Sub DisableState(ByRef aObj)
		aObj.FadeSpeedUp = 1000
		aObj.State = 1
	End Sub
	
	Public Sub Init() 'Just runs TurnOnStates right now
		TurnOnStates
	End Sub
	
	Public Property Let Modulate(aIdx, aCoef)
		Mult(aIdx) = aCoef
		Lock(aIdx) = False
		Loaded(aIdx) = False
	End Property
	Public Property Get Modulate(aIdx)
		Modulate = Mult(aIdx)
	End Property
	
	Public Sub Update1() 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
		Dim x
		For x = 0 To UBound(OnOff)
			If Not Lock(x) Then 'and not Loaded(x) then
				If OnOff(x) > 0 Then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					If Lvl(x) >= OnOff(x) Then
						Lvl(x) = OnOff(x)
						Lock(x) = True
					End If
				Else 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					If Lvl(x) <= 0 Then
						Lvl(x) = 0
						Lock(x) = True
					End If
				End If
			End If
		Next
	End Sub
	
	Public Sub Update2() 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame
		InitFrame = GameTime	'Calculate frametime
		Dim x
		For x = 0 To UBound(OnOff)
			If Not Lock(x) Then 'and not Loaded(x) then
				If OnOff(x) > 0 Then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					If Lvl(x) >= OnOff(x) Then
						Lvl(x) = OnOff(x)
						Lock(x) = True
					End If
				Else 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					If Lvl(x) <= 0 Then
						Lvl(x) = 0
						Lock(x) = True
					End If
				End If
			End If
		Next
		Update
	End Sub
	
	Public Sub Update() 'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		Dim x,xx, aLvl
		For x = 0 To UBound(OnOff)
			If Not Loaded(x) Then
				aLvl = Lvl(x) * Mult(x)
				If IsArray(obj(x) ) Then	'if array
					If UseFunc Then
						For Each xx In obj(x)
							xx.IntensityScale = cFilter(aLvl)
						Next
					Else
						For Each xx In obj(x)
							xx.IntensityScale = aLvl
						Next
					End If
				Else						'if single lamp or flasher
					If UseFunc Then
						obj(x).Intensityscale = cFilter(aLvl)
					Else
						obj(x).Intensityscale = aLvl
					End If
				End If
				'   if TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" and typename(lvl(x)) <> "Long" then msgbox "uhh " & 2 & " = " & lvl(x)
				'   If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x))	'Callback
				If UseCallBack(x) Then Proc name & x,aLvl	'Proc
				If Lock(x) Then
					If Lvl(x) = OnOff(x) Or Lvl(x) = 0 Then Loaded(x) = True	'finished fading
				End If
			End If
		Next
	End Sub
End Class

'Lamp Filter
Function LampFilter(aLvl)
	LampFilter = aLvl ^ 1.6	'exponential curve?
End Function

'Helper functions
Sub Proc(String, Callback)	'proc using a string and one argument
	'On Error Resume Next
	Dim p
	Set P = GetRef(String)
	P Callback
	If err.number = 13 Then  MsgBox "Proc error! No such procedure: " & vbNewLine & String
	If err.number = 424 Then MsgBox "Proc error! No such Object"
End Sub

Function AppendArray(ByVal aArray, aInput)	'append one value, object, or Array onto the end of a 1 dimensional array
	If IsArray(aInput) Then 'Input is an array...
		Dim tmp
		tmp = aArray
		If Not IsArray(aArray) Then	'if not array, create an array
			tmp = aInput
		Else					'Append existing array with aInput array
			ReDim Preserve tmp(UBound(aArray) + UBound(aInput) + 1)	'If existing array, increase bounds by uBound of incoming array
			Dim x
			For x = 0 To UBound(aInput)
				If IsObject(aInput(x)) Then
					Set tmp(x + UBound(aArray) + 1 ) = aInput(x)
				Else
					tmp(x + UBound(aArray) + 1 ) = aInput(x)
				End If
			Next
			AppendArray = tmp	 'return new array
		End If
	Else 'Input is NOT an array...
		If Not IsArray(aArray) Then	'if not array, create an array
			aArray = Array(aArray, aInput)
		Else
			ReDim Preserve aArray(UBound(aArray) + 1)	'If array, increase bounds by 1
			If IsObject(aInput) Then
				Set aArray(UBound(aArray)) = aInput
			Else
				aArray(UBound(aArray)) = aInput
			End If
		End If
		AppendArray = aArray 'return new array
	End If
End Function

Function ColtoArray(aDict)	'converts a collection to an indexed array. Indexes will come out random probably.
	redim a(999)
	dim count : count = 0
	dim x  : for each x in aDict : set a(Count) = x : count = count + 1 : Next
	redim preserve a(count-1) : ColtoArray = a
End Function

'******************************************************
'****  END LAMPZ
'******************************************************


 'Stern Galaxy
 'added by Inkochnito
 Sub editDips
 	Dim vpmDips : Set vpmDips = New cvpmDips
 	With vpmDips
 		.AddForm 700,400,"Galaxy - DIP switches"
 		.AddFrame 0,0,190,"Maximum Credits",&H00060000,Array("10 Credits",0,"15 Credits",&H00020000,"25 Credits",&H00040000,"40 Credits",&H00060000)'dip 18&19
 		.AddFrame 0,76,190,"High Game To Date Awards",49152,Array("Nothing",0,"1 Replay",&H00004000,"2 Replays",32768,"3 Replays",49152)'dip 15&16
 		.AddFrame 0,152,190,"Replay Feature",&H00000020,Array("Add-A-Ball (Extra Ball Instead)",0,"Replay Enabled",&H00000020)'dip 6
 		.AddFrame 0,198,190,"Welfare (Inlanes Spot Drop Targets)",&H00000080,Array("Disabled At 4X",0,"Disabled At 3X",&H00000080)'dip 8
 		.AddFrame 0,244,190,"Extra Ball Feature",&H00010000,Array("Novelty (Extra Ball Disabled)",0,"Extra Ball Enabled",&H00010000)'dip 17
 		.AddChk 0,300,190,Array("Match Feature",&H00100000)'dip 21
 		.AddChk 0,315,190,Array("Credits Displayed",&H00080000)'dip 20
 		.AddFrame 205,0,190,"Special Award",&HC0000000,Array("Nothing",0,"100,000 Points",&H40000000,"Extra Ball",&H80000000,"Replay",&HC0000000)'dip 31&32
 		.AddFrame 205,76,190,"Planet Drop Target Special",&H03000000,Array("Lit At Pluto",0,"Lit At Uranus",&H01000000,"Lit At Neptune",&H02000000,"Lit At Saturn",&H03000000)'dip 25&26
 		.AddFrame 205,152,190,"'A' in GALAXY Difficulty",&H00800000,Array("Lite Each 'A' Individually",0,"Lite Both 'A's When One 'A' Lit",&H00800000)'dip 24
 		.AddFrame 205,198,190,"Balls Per Game",&H00000040,Array("3 Balls",0,"5 Balls",&H00000040)'dip 7
 		.AddFrame 205,244,190,"Replay Limit",&H00200000,Array("1 Per Ball",0,"1 Per Game",&H00200000)'dip 22
 		.AddChk 205,300,190,Array("Attract Mode Sound",&H00400000)'dip 23
 		.AddChk 205,315,190,Array("Background Sound",&H00002000)'dip 14
 		.AddLabel 50,340,300,15,"After hitting OK, press F3 to reset game with new settings."
 		.ViewDips
 	End With
 End Sub
 Set vpmShowDips = GetRef("editDips")
'**********************************************************************************************************
'**********************************************************************************************************
'**********************************************************************************************************
'**********************************************************************************************************

'	Slingshot Animations
'************************************************************

Dim RStep, Lstep, LAstep, LBstep, LCstep, lkick, skick

Sub RightSlingShot_Slingshot
    RS.VelocityCorrect(Activeball)
    vpmTimer.PulseSw 15
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -18
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	RandomSoundSlingshotRight LeafSwitch8
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(Activeball)
    vpmTimer.PulseSw 16
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -18
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	RandomSoundSlingshotLeft LeafSwitch1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
End Sub


'******************************************************
'	ZSSC: SLINGSHOT CORRECTION FUNCTIONS by apophis
'******************************************************
' To add these slingshot corrections:
'	 - On the table, add the endpoint primitives that define the two ends of the Slingshot
'	 - Initialize the SlingshotCorrection objects in InitSlingCorrection
'	 - Call the .VelocityCorrect methods from the respective _Slingshot event sub

Dim LS
Set LS = New SlingshotCorrection
Dim RS
Set RS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection
	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS
	
	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS
	
	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	' These values are best guesses. Retune them if needed based on specific table research.
	AddSlingsPt 0, 0.00, - 4
	AddSlingsPt 1, 0.45, - 7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4
End Sub

Sub AddSlingsPt(idx, aX, aY)		'debugger wrapper for adjusting flipper script in-game
	Dim a
	a = Array(LS, RS)
	Dim x
	For Each x In a
		x.addpoint idx, aX, aY
	Next
End Sub

'' The following sub are needed, however they may exist somewhere else in the script. Uncomment below if needed
'Dim PI: PI = 4*Atn(1)
'Function dSin(degrees)
'	dsin = sin(degrees * Pi/180)
'End Function
'Function dCos(degrees)
'	dcos = cos(degrees * Pi/180)
'End Function
'
'Function RotPoint(x,y,angle)
'	dim rx, ry
'	rx = x*dCos(angle) - y*dSin(angle)
'	ry = x*dSin(angle) + y*dCos(angle)
'	RotPoint = Array(rx,ry)
'End Function

Class SlingshotCorrection
	Public DebugOn, Enabled
	Private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2
	
	Public ModIn, ModOut
	
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
		Enabled = True
	End Sub
	
	Public Property Let Object(aInput)
		Set Slingshot = aInput
	End Property
	
	Public Property Let EndPoint1(aInput)
		SlingX1 = aInput.x
		SlingY1 = aInput.y
	End Property
	
	Public Property Let EndPoint2(aInput)
		SlingX2 = aInput.x
		SlingY2 = aInput.y
	End Property
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If gametime > 100 Then Report
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
	
	
	Public Sub VelocityCorrect(aBall)
		Dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then
			XL = SlingX1
			YL = SlingY1
			XR = SlingX2
			YR = SlingY2
		Else
			XL = SlingX2
			YL = SlingY2
			XR = SlingX1
			YR = SlingY1
		End If
		
		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then
			If Abs(XR - XL) > Abs(YR - YL) Then
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If
		
		'Velocity angle correction
		If Not IsEmpty(ModIn(0) ) Then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'   debug.print " BallPos=" & BallPos &" Angle=" & Angle 
			'   debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled Then aBall.Velx = RotVxVy(0)
			If Enabled Then aBall.Vely = RotVxVy(1)
			'   debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			'   debug.print " " 
		End If
	End Sub
End Class




Sub SolLeftOut(enabled)
    if enabled then
        playsound Soundfx("popper_ball",DOFContactors)
'        Controller.Switch(24) = false
        kicker.Kick  0, 28 + 5 * Rnd
		slingkick.rotx = 20
		lkick = 0
		slingkicktimer.enabled = 1
    end if
End Sub

Sub leftsaucersafetytimer_timer
	kicker.Kick 0, 18 + 5 * Rnd
'	playsound "bell"
	leftsaucersafetytimer.enabled=0
End Sub

Sub SlingKickTimer_Timer
    Select Case LKick
        Case 1:slingkick.rotx = 20
        Case 2:slingkick.rotx = 20
        Case 3:slingkick.rotx = 20
        Case 4:slingkick.rotx = 5
        Case 5:slingkick.rotx = 0
		slingkicktimer.enabled = 0
    End Select
    LKick = LKick + 1
End Sub

Sub SolSaucerOut(enabled)
    if enabled then
        playsound Soundfx("popper_ball",DOFContactors)
        playsound Soundfx("solenoid",DOFContactors)
        Controller.Switch(40) = 0
        sw40.Kick  100, 10 + 5 * Rnd
		pkickarm.rotz=2
		skick = 0
		topsaucertimer.enabled=1
    end if
End Sub

Sub topsaucertimer_Timer
    Select Case skick
        Case 1:Pkickarm.Rotz = 15
        Case 2:Pkickarm.Rotz = 15
        Case 3:Pkickarm.Rotz = 15
        Case 4:Pkickarm.Rotz = 8
        Case 5:Pkickarm.Rotz = 4
        Case 6:Pkickarm.Rotz = 2
        Case 7:Pkickarm.Rotz = 0:topsaucertimer.Enabled = 0
    End Select
    skick = skick + 1
End Sub


'	BALL ROLLING AND DROP SOUNDS
'******************************************************
'
' Be sure to call RollingUpdate in a timer with a 10ms interval see the GameTimer_Timer() sub

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

Sub RollingUpdate()
	Dim b', BOT
	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob - 1
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(BOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

	For b = 0 to UBound(BOT)
		If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If

		' Ball Drop Sounds
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

		' "Static" Ball Shadows
		' Comment the next If block, if you are not implementing the Dyanmic Ball Shadows
'		If AmbientBallShadowOn = 0 Then
'			If BOT(b).Z > 30 Then
'				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
'			Else
'				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
'			End If
'			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + fovY
'			BallShadowA(b).X = BOT(b).X
'			BallShadowA(b).visible = 1
'		End If
	Next
End Sub


'   END BALL ROLLING AND DROP SOUNDS
'******************************************************


' 	FLEEP MECHANICAL SOUNDS
'******************************************************

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.  
' Create the following new collections:
' 	Metals (all metal objects, metal walls, metal posts, metal wire guides)
' 	Apron (the apron walls and plunger wall)
' 	Walls (all wood or plastic walls)
' 	Rollovers (wire rollover triggers, star triggers, or button triggers)
' 	Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
' 	Gates (plate gates)
' 	GatesWire (wire gates)
' 	Rubbers (all rubbers including posts, sleeves, pegs, and bands)
' When creating the collections, make sure "Fire events for this collection" is checked.  
' You'll also need to make sure "Has Hit Event" is checked for each object placed in these collections (not necessary for gates and triggers).  
' Once the collections and objects are added, the save, close, and restart VPX.
'
' Tutorial vides by Apophis
' Part 1: 	https://youtu.be/PbE2kNiam3g
' Part 2: 	https://youtu.be/B5cm1Y8wQsk
' Part 3: 	https://youtu.be/eLhWyuYOyGg


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1														'volume level; range [0, 1]
NudgeLeftSoundLevel = 1													'volume level; range [0, 1]
NudgeRightSoundLevel = 1												'volume level; range [0, 1]
NudgeCenterSoundLevel = 1												'volume level; range [0, 1]
StartButtonSoundLevel = 0.1												'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr											'volume level; range [0, 1]
PlungerPullSoundLevel = 1												'volume level; range [0, 1]
RollingSoundFactor = 1.1/5		

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperLeft1HitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010           						'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635								'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                        						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                      						'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel								'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel								'sound helper; not configurable
SlingshotSoundLevel = 0.95												'volume level; range [0, 1]
BumperSoundFactor = 4.25												'volume multiplier; must not be zero
KnockerSoundLevel = 1 													'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel, PostSoundLevel

BallWithBallCollisionSoundFactor = 3.2									'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5											'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5											'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5										'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025									'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025									'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8									'volume level; range [0, 1]
WallImpactSoundFactor = 0.075											'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8
PostSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5													'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10											'volume multiplier; must not be zero
DTSoundLevel = 0.25														'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                              					'volume level; range [0, 1]
SpinnerSoundLevel = 0.5                              					'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8														'volume level; range [0, 1]
BallReleaseSoundLevel = 1												'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2									'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015										'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5													'volume multiplier; must not be zero


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


'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
    tmp = tableobj.y * 2 / tableheight-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

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
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, StartBallControl
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, sw16
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

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////
Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
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

Sub LeftFlipper1Collide(parm)
	FlipperLeft1HitParm = parm/10
	If FlipperLeft1HitParm > 1 Then
		FlipperLeft1HitParm = 1
	End If
	FlipperLeft1HitParm = FlipperUpSoundLevel * FlipperLeft1HitParm
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
	RandomSoundWall()
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


'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd*6)+1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd*6)+1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315									'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05									'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025*RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025*RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025*RelayFlashSoundLevel, obj			
		Case 0
			PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025*RelayFlashSoundLevel, obj		
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////


'******************************************************
'****  END FLEEP MECHANICAL SOUNDS
'******************************************************

'******************************************************
' 	ZNFF: FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level we'll need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. an object or point to tell the script where the tip of the flipper is at rest (EndPointLp, EndPointRp)
'	4. and, special scripting
'
' A common mistake is incorrect flipper length.  A 3-inch flipper with rubbers will be about 3.125 inches long.  
' This translates to about 147 vp units.  Therefore, the flipper start radius + the flipper length + the flipper end 
' radius should  equal approximately 147 vp units. Another common mistake is is that sometimes the right flipper
' angle was set with a large postive value (like 238 or something). It should be using negative value (like -122).
'
' The following settings are a solid starting point for various eras of pinballs.
' |                    | EM's           | late 70's to mid 80's | mid 80's to early 90's | mid 90's and later |
' | ------------------ | -------------- | --------------------- | ---------------------- | ------------------ |
' | Mass               | 1              | 1                     | 1                      | 1                  |
' | Strength           | 500-1000 (750) | 1400-1600 (1500)      | 2000-2600              | 3200-3300 (3250)   |
' | Elasticity         | 0.88           | 0.88                  | 0.88                   | 0.88               |
' | Elasticity Falloff | 0.15           | 0.15                  | 0.15                   | 0.15               |
' | Fricition          | 0.8-0.9        | 0.9                   | 0.9                    | 0.9                |
' | Return Strength    | 0.11           | 0.09                  | 0.07                   | 0.055              |
' | Coil Ramp Up       | 2.5            | 2.5                   | 2.5                    | 2.5                |
' | Scatter Angle      | 0              | 0                     | 0                      | 0                  |
' | EOS Torque         | 0.3            | 0.3                   | 0.275                  | 0.275              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |



'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 
'******************************************************

Dim LF: Set LF = New FlipperPolarity
Dim RF: Set RF = New FlipperPolarity

InitPolarity

'
''*******************************************
'' Late 70's to early 80's
'
Sub InitPolarity()
   dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 80
		x.DebugOn=False ' prints some info in debugger

		x.AddPt "Polarity", 0, 0, 0
		x.AddPt "Polarity", 1, 0.05, - 2.7		
		x.AddPt "Polarity", 2, 0.33, - 2.7
		x.AddPt "Polarity", 3, 0.37, - 2.7		
		x.AddPt "Polarity", 4, 0.41, - 2.7
		x.AddPt "Polarity", 5, 0.45, - 2.7
		x.AddPt "Polarity", 6, 0.576, - 2.7
		x.AddPt "Polarity", 7, 0.66, - 1.8
		x.AddPt "Polarity", 8, 0.743, - 0.5
		x.AddPt "Polarity", 9, 0.81, - 0.5
		x.AddPt "Polarity", 10, 0.88, 0

		x.AddPt "Velocity", 0, 0, 1
		x.AddPt "Velocity", 1, 0.16, 1.06
		x.AddPt "Velocity", 2, 0.41, 1.05
		x.AddPt "Velocity", 3, 0.53, 1 '0.982
		x.AddPt "Velocity", 4, 0.702, 0.968
		x.AddPt "Velocity", 5, 0.95,  0.968
		x.AddPt "Velocity", 6, 1.03, 0.945
	Next

	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
    LF.SetObjects "LF", LeftFlipper, TriggerLF 
    RF.SetObjects "RF", RightFlipper, TriggerRF 
End Sub
'
'
'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************
 
' modified 2023 by nFozzy
' Removed need for 'endpoint' objects
' Added 'createvents' type thing for TriggerLF / TriggerRF triggers. 
' Removed AddPt function which complicated setup imo
' made DebugOn do something (prints some stuff in debugger) 
'   Otherwise it should function exactly the same as before
 
Class FlipperPolarity 
	Public DebugOn, Enabled
	Private FlipAt		'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay		'delay before trigger turns off and polarity is disabled
	private Flipper, FlipperStart, FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)
	private Name
 
	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub
 
	Public Sub SetObjects(aName, aFlipper, aTrigger) 
 
		if typename(aName) <> "String" then msgbox "FlipperPolarity: .SetObjects error: first argument must be a string (and name of Object). Found:" & typename(aName) end if
		if typename(aFlipper) <> "Flipper" then msgbox "FlipperPolarity: .SetObjects error: second argument must be a flipper. Found:" & typename(aFlipper) end if
		if typename(aTrigger) <> "Trigger" then msgbox "FlipperPolarity: .SetObjects error: third argument must be a trigger. Found:" & typename(aTrigger) end if
		if aFlipper.EndAngle > aFlipper.StartAngle then LR = -1 Else LR = 1 End If
		Name = aName
		Set Flipper = aFlipper : FlipperStart = aFlipper.x 
		FlipperEnd = Flipper.Length * sin((Flipper.StartAngle / 57.295779513082320876798154814105)) + Flipper.X ' big floats for degree to rad conversion
		FlipperEndY = Flipper.Length * cos(Flipper.StartAngle / 57.295779513082320876798154814105)*-1 + Flipper.Y
 
		dim str : str = "sub " & aTrigger.name & "_Hit() : " & aName & ".AddBall ActiveBall : End Sub'"
		ExecuteGlobal(str)
		str = "sub " & aTrigger.name & "_UnHit() : " & aName & ".PolarityCorrect ActiveBall : End Sub'"
		ExecuteGlobal(str)  
 
	End Sub
 
	Public Property Let EndPoint(aInput) :  : End Property ' Legacy: just no op 
 
	Public Sub AddPt(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
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
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function		'Timer shutoff for polaritycorrect
 
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
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)								'find safety coefficient 'ycoef' data
				end if
			Next
 
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)												'find safety coefficient 'ycoef' data
			End If
 
			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
 
				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
 
				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If
 
			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
 
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
			if DebugOn then debug.print "PolarityCorrect" & " " & Name & " @ " & gametime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100)
		End If
		RemoveBall aBall
	End Sub
End Class
 
'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS 
'******************************************************
 
' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)		'Shuffle objects in a temp array
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
	redim aArray(aCount-1+offset)		'Resize original array
	for x = 0 to aCount-1				'set objects back into original array
		if IsObject(a(x)) then 
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub
 
' Used for flipper correction and rubber dampeners
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub
 
' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
	BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function
 
' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)		'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function
 
' Used for flipper correction
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
 
' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)		'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)		'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )
 
	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )		 'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )		'Clamp upper
 
	LinearEnvelope = Y
End Function


'******************************************************
'  FLIPPER TRICKS 
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					exit Sub
				end If
			Next
			For b = 0 to Ubound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				end If
			Next
		End If
	Else 
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then EOSNudge1 = 0
	End If
End Sub

'*****************
' Maths
'*****************
Dim PI: PI = 4*Atn(1)

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

Function Atn2(dy, dx)
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

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI /180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle+90))+Flipper.x, Sin(Radians(Flipper.currentangle+90))+Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle  = ABS(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360

	If DistanceFromFlipper(ballx,bally,Flipper) < 48 and DiffAngle <= 90 and Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If        
End Function


'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0 '0 = fast, 1 = medium, 2 = slow (tap passes should work)

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 1 'EM's to late 80's
'Const EOSTnew = 0.8 '90's and later
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup

Select Case FlipperCoilRampupMode
	Case 0
	SOSRampup = 2.5
	Case 1
	SOSRampup = 6
	Case 2
	SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'Const EOSReturn = 0.055  'EM's
Const EOSReturn = 0.045  'late 70's to mid 80's
'Const EOSReturn = 0.035  'mid 80's to early 90's
'Const EOSReturn = 0.025  'mid 90's and later
LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

Sub FlipperActivate(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity

	Flipper.eostorque = EOST         
	Flipper.eostorqueangle = EOSA         
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA
	Flipper.eostorque = EOST*EOSReturn/FReturn


	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b

		For b = 0 to UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)        '-1 for Right Flipper

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

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup                        
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST        
			Flipper.eostorqueangle = EOSA
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
	Dir = Flipper.startangle/Abs(Flipper.startangle)    '-1 for Right Flipper
	Dim LiveCatchBounce                                                                                                                        'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.5 Then                                                'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)        'Partial catch when catch happens a bit late
		end If

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
	End If
End Sub


'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************


'******************************************************
' 	ZDMP: RUBBER DAMPENERS
'******************************************************
'
' These are data mined bounce curves, 
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
	TargetBouncer Activeball, 0.7
End Sub


dim RubbersD : Set RubbersD = new Dampener        'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        'shows info in textbox "TBPout"
RubbersD.Print = False        'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1        'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967        'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64        'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener        'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False        'shows info in textbox "TBPout"
SleevesD.Print = False        'debug, reports in debugger (in vel, out cor)
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
	public name, Threshold         'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
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
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)

		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
	End Sub

	public sub Dampenf(aBall, parm) 'Rubberizer is handle here
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id)+0.0001)
		coef = desiredcor / realcor 
		If abs(aball.velx) < 2 and aball.vely < 0 and aball.vely > -3.75 then 
			aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		End If
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
	End Sub


	Public Sub Report()         'debug, reports all coords in tbPL.text
		if not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub

End Class


'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = BOT
		

		for each b in allballs
			if b.id >= HighestID then highestID = b.id
		Next

		if uBound(ballvel) < highestID then redim ballvel(highestID)	'set bounds
		if uBound(ballvelx) < highestID then redim ballvelx(highestID)	'set bounds
		if uBound(ballvely) < highestID then redim ballvely(highestID)	'set bounds

		for each b in allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class


'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************



'******************************************************
' 	ZRDT: DROP TARGETS by Rothbauerw
'******************************************************
' This solution improves the physics for drop targets to create more realistic behavior. It allows the ball 
' to move through the target enabling the ability to score more than one target with a well placed shot.
' It also handles full drop target animation, including deflection on hit and a slight lift when the drop 
' targets raise, switch handling, bricking, and popping the ball up if it's over the drop target when it raises.
'
'Add a Timer named DTAnim to editor to handle drop & standup target animations, or run them off an always-on 10ms timer (GameTimer)
'DTAnim.interval = 10
'DTAnim.enabled = True

'Sub DTAnim_Timer
'	DoDTAnim
'	DoSTAnim
'End Sub

' For each drop target, we'll use two wall objects for physics calculations and one primitive for visuals and   
' animation. We will not use target objects.  Place your drop target primitive the same as you would a VP drop target. 
' The primitive should have it's pivot point centered on the x and y axis and at or just below the playfield 
' level on the z axis. Orientation needs to be set using Rotz and bending deflection using Rotx. You'll find a hooded 
' target mesh in this table's example. It uses the same texture map as the VP drop targets.


'******************************************************
'  DROP TARGETS INITIALIZATION
'******************************************************

Class DropTarget
  Private m_primary, m_secondary, m_prim, m_sw, m_animate, m_isDropped

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(primary): Set m_primary = primary: End Property

  Public Property Get Secondary(): Set Secondary = m_secondary: End Property
  Public Property Let Secondary(secondary): Set m_secondary = secondary: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(prim): Set m_prim = prim: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(sw): m_sw = sw: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(animate): m_animate = animate: End Property

  Public Property Get IsDropped(): IsDropped = m_isDropped: End Property
  Public Property Let IsDropped(isDropped): m_isDropped = isDropped: End Property

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

'Define a variable for each drop target
Dim DT4, DT11, DT10, DT9

'Set array with drop target objects
'
'DropTargetvar = Array(primary, secondary, prim, swtich, animate)
' 	primary: 			primary target wall to determine drop
'	secondary:			wall used to simulate the ball striking a bent or offset target after the initial Hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							rotz must be used for orientation
'							rotx to bend the target back
'							transz to move it up and down
'							the pivot point should be in the center of the target on the x, y and at or below the playfield (0) on z
'	switch:				ROM switch number
'	animate:			Array slot for handling the animation instrucitons, set to 0
'						Values for animate: 1 - bend target (hit to primary), 2 - drop target (hit to secondary), 3 - brick target (high velocity hit to secondary), -1 - raise target 
'   isDropped:			Boolean which determines whether a drop target is dropped. Set to false if they are initially raised, true if initially dropped.

Set DT4 = (new DropTarget)(sw4, sw4a, sw4p, 4, 0, false)
Set DT11 = (new DropTarget)(sw11, sw11a, sw11p, 11, 0, false)
Set DT10 = (new DropTarget)(sw10, sw10a, sw10p, 10, 0, false)
Set DT9 = (new DropTarget)(sw9, sw9a, sw9p, 9, 0, false)
'Set DT57 = (new DropTarget)(sw57, sw57a, sw57p, 57, 0, false)
'Set DT59 = (new DropTarget)(sw59, sw59a, sw59p, 59, 0, false)

Dim DTArray
DTArray = Array(DT4, DT11, DT10, DT9)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 44 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 30 'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "" 'Drop Target Hit sound
Const DTDropSound = "DropTarget_Down" 'Drop Target Drop sound
Const DTResetSound = "DropTarget_Up" 'Drop Target reset sound

Const DTMass = 0.4 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance


'******************************************************
'  DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)

	PlayTargetSound
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
End Sub

Function DTAnimate(primary, secondary, prim, switch, animate)
	dim transz, switchid
	Dim animtime, rangle

	switchid = switch

	Dim ind
	ind = DTArrayID(switchid)

	rangle = prim.rotz * PI / 180

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
		SoundDropTargetDrop prim
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
			DTArray(ind).isDropped = true 'Mark target as dropped
			controller.Switch(Switchid) = 1
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
			Dim b', BOT
'			BOT = GetBalls

			For b = 0 to UBound(BOT)
				If InRotRect(BOT(b).x,BOT(b).y,prim.x, prim.y, prim.rotz, -25,-10,25,-10,25,25,-25,25) and BOT(b).z < prim.z+DTDropUnits+25 Then
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
			prim.transz = DTDropUpUnits
			prim.rotx = 0
			prim.roty = 0
			primary.uservalue = gametime
		end if
		primary.collidable = 0
		secondary.collidable = 1
		DTArray(ind).isDropped = false 'Mark target as not dropped
		controller.Switch(Switchid) = 0

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
'  DROP TARGET
'  SUPPORTING FUNCTIONS 
'******************************************************


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
	End If
End Function

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
    Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
    Dim rotxy
    rotxy = RotPoint(ax,ay,angle)
    rax = rotxy(0)+px : ray = rotxy(1)+py
    rotxy = RotPoint(bx,by,angle)
    rbx = rotxy(0)+px : rby = rotxy(1)+py
    rotxy = RotPoint(cx,cy,angle)
    rcx = rotxy(0)+px : rcy = rotxy(1)+py
    rotxy = RotPoint(dx,dy,angle)
    rdx = rotxy(0)+px : rdy = rotxy(1)+py

    InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

Function RotPoint(x,y,angle)
    dim rx, ry
    rx = x*dCos(angle) - y*dSin(angle)
    ry = x*dSin(angle) + y*dCos(angle)
    RotPoint = Array(rx,ry)
End Function


'******************************************************
'****  END DROP TARGETS
'******************************************************




'******************************************************
'	ZRST: STAND-UP TARGET INITIALIZATION
'******************************************************

Class StandupTarget
  Private m_primary, m_prim, m_sw, m_animate

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(primary): Set m_primary = primary: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(prim): Set m_prim = prim: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(sw): m_sw = sw: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(animate): m_animate = animate: End Property

  Public default Function init(primary, prim, sw, animate)
    Set m_primary = primary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate

    Set Init = Me
  End Function
End Class

'Define a variable for each stand-up target
Dim ST26, ST29, ST30

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
' 	primary: 			vp target to determine target hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							transy must be used to offset the target animation
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instrucitons, set to 0
' 
'You will also need to add a secondary hit object for each stand up (name sw11o, sw12o, and sw13o on the example Table1)
'these are inclined primitives to simulate hitting a bent target and should provide so z velocity on high speed impacts

Set ST26 = (new StandupTarget)(sw26, sw26p, 26, 0)
Set ST29 = (new StandupTarget)(sw29, sw29p, 29, 0)
Set ST30 = (new StandupTarget)(sw30, sw30p, 30, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST26, ST29, ST30)

'Configure the behavior of Stand-up Targets
Const STAnimStep =  1.5 		'vpunits per animation step (control return to Start)
Const STMaxOffset = 9 			'max vp units target moves when hit

Const STMass = 0.2				'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				STAND-UP TARGETS FUNCTIONS
'******************************************************

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)

	PlayTargetSound
	STArray(i).animate =  STCheckHit(Activeball,STArray(i).primary)

	If STArray(i).animate <> 0 Then
		DTBallPhysics Activeball, STArray(i).primary.orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 to uBound(STArray) 
		If STArray(i).sw = switch Then STArrayID = i:Exit Function 
	Next
End Function

'Check if target is hit on it's face
Function STCheckHit(aBall, target) 
	dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter, paravel, paravelafter
	rangle = (target.orientation - 90) * 3.1416 / 180	
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)

	perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
	paravel = cor.BallVel(aball.id) * sin(bangle-rangle)

	perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 
	paravelafter = BallSpeed(aBall) * sin(bangleafter - rangle)

	If perpvel > 0 and  perpvelafter <= 0 Then
		STCheckHit = 1
	ElseIf perpvel > 0 and ((paravel > 0 and paravelafter > 0) or (paravel < 0 and paravelafter < 0)) Then
		STCheckHit = 1
	Else 
		STCheckHit = 0
	End If
End Function

Sub DoSTAnim()
	Dim i
	For i=0 to Ubound(STArray)
		STArray(i).animate = STAnimate(STArray(i).primary,STArray(i).prim,STArray(i).sw,STArray(i).animate)
	Next
End Sub

Function STAnimate(primary, prim, switch,  animate)
	Dim animtime

	STAnimate = animate

	if animate = 0  Then
		primary.uservalue = 0
		STAnimate = 0
		Exit Function
	Elseif primary.uservalue = 0 then 
		primary.uservalue = gametime
	end if

	animtime = gametime - primary.uservalue

	If animate = 1 Then
		primary.collidable = 0
		prim.transy = -STMaxOffset
		vpmTimer.PulseSw switch
		STAnimate = 2
		Exit Function
	elseif animate = 2 Then
		prim.transy = prim.transy + STAnimStep
		If prim.transy >= 0 Then
			prim.transy = 0
			primary.collidable = 1
			STAnimate = 0
			Exit Function
		Else 
			STAnimate = 2
		End If
	End If	
End Function

'******************************************************
'***	END STAND-UP TARGETS
'******************************************************




'******************************************************
' 	ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    if TargetBouncerEnabled = 1 and aball.z < 30 then
        'debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        vel = BallSpeed(aBall)
        if aBall.velx = 0 then vratio = 1 else vratio = aBall.vely/aBall.velx
        Select Case Int(Rnd * 6) + 1
            Case 1: zMultiplier = 0.2*defvalue
			Case 2: zMultiplier = 0.25*defvalue
            Case 3: zMultiplier = 0.3*defvalue
			Case 4: zMultiplier = 0.4*defvalue
            Case 5: zMultiplier = 0.45*defvalue
            Case 6: zMultiplier = 0.5*defvalue
        End Select
        aBall.velz = abs(vel * zMultiplier * TargetBouncerFactor)
        aBall.velx = sgn(aBall.velx) * sqr(abs((vel^2 - aBall.velz^2)/(1+vratio^2)))
        aBall.vely = aBall.velx * vratio
        'debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
        'debug.print "conservation check: " & BallSpeed(aBall)/vel
	end if
end sub

'****************************************************************
'****  END VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'****************************************************************




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




'***************************************************************
'	ZSHA: VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

'****** Part A:  Table Elements ******
'
' Import the "bsrtx8" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the BallShadowA flasher set and the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#
'	* Count from 0 up, with at least as many objects each as there can be balls, including locked balls.  You'll get an "eval" warning if tnob is higher
'	* Warning:  If merging with another system (JP's ballrolling), you may need to check tnob math and add an extra BallShadowA# flasher (out of range error)
' Ensure you have a timer with a -1 interval that is always running
' Set plastic ramps DB to *less* than the ambient shadows (-11000) if you want to see the pf shadow through the ramp
' Place triggers at the start of each ramp *type* (solid, clear, wire) and one at the end if it doesn't return to the base pf
'	* These can share duties as triggers for RampRolling sounds

' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
' It's recommended that you be selective in which lights go in this collection, as there are limitations:
' 1. The shadows can "pass through" solid objects and other light sources, so be mindful of where the lights would actually able to cast shadows
' 2. If there are more than two equidistant sources, the shadows can suddenly switch on and off, so places like top and bottom lanes need attention
' 3. At this time the shadows get the light on/off from tracking gilvl, so if you have lights you want shadows for that are on at different times you will need to either:
'	a) remove this restriction (shadows think lights are always On)
'	b) come up with a custom solution (see TZ example in script)
' After confirming the shadows work in general, use ball control to move around and look for any weird behavior

'****** End Part A:  Table Elements ******


'****** Part B:  Code and Functions ******

' *** Timer sub
' The "DynamicBSUpdate" sub should be called by a timer with an interval of -1 (framerate)
' Example timer sub:

'Sub FrameTimer_Timer()
'	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
'End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary
'Const tnob = 10 ' total number of balls
'Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
'Dim tablewidth: tablewidth = Table1.width
'Dim tableheight: tableheight = Table1.height

' *** User Options - Uncomment here or move to top for easy access by players

' *** The following segment goes within the RollingUpdate sub, so that if Ambient...=0 and Dynamic...=0 the entire DynamicBSUpdate sub can be skipped for max performance
' ** Change BOT to BOT if using existing getballs code
' ** Double commented lines commonly found there included for reference:

''	' stop the sound of deleted balls
''	For b = UBound(BOT) + 1 to tnob
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
''		...rolling(b) = False
''		...StopSound("BallRoll_" & b)
''	Next
''
'' ...rolling and drop sounds...
''
''		If DropCount(b) < 5 Then
''			DropCount(b) = DropCount(b) + 1
''		End If
''
'		' "Static" Ball Shadows
'		If AmbientBallShadowOn = 0 Then
'			BallShadowA(b).visible = 1
'			BallShadowA(b).X = BOT(b).X + offsetX
'			If BOT(b).Z > 30 Then
'				BallShadowA(b).height=BOT(b).z - BallSize/4 + b/1000	'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
'				BallShadowA(b).Y = BOT(b).Y + offsetY + BallSize/10
'			Else
'				BallShadowA(b).height=BOT(b).z - BallSize/2 + 1.04 + b/1000
'				BallShadowA(b).Y = BOT(b).Y + offsetY
'			End If
'		End If

' *** Place this inside the table init, just after trough balls are added to BOT
' 
' Add balls to shadow dictionary
'	For Each xx in BOT
'		bsDict.Add xx.ID, bsNone
'	Next

' *** Example RampShadow trigger subs:

'Sub ClearRampStart_hit()
'	bsRampOnClear			'Shadow on ramp and pf below
'End Sub

'Sub SolidRampStart_hit()
'	bsRampOn				'Shadow on ramp only
'End Sub

'Sub WireRampStart_hit()
'	bsRampOnWire			'Shadow only on pf
'End Sub

'Sub RampEnd_hit()
'	bsRampOff ActiveBall.ID	'Back to default shadow behavior
'End Sub


' *** Required Functions, enable these if they are not already present elswhere in your table
Function max(a,b)
	If a > b Then
		max = a
	Else
		max = b
	End If
End Function

'Function Distance(ax,ay,bx,by)
'	Distance = SQR((ax - bx)^2 + (ay - by)^2)
'End Function

'Dim PI: PI = 4*Atn(1)

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

'Function AnglePP(ax,ay,bx,by)
'	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
'End Function

'****** End Part B:  Code and Functions ******


'****** Part C:  The Magic ******

' *** These define the appearance of shadows in your table	***

'Ambient (Room light source)
Const AmbientBSFactor = 0.9		'0 to 1, higher is darker
Const AmbientMovement = 1		'1+ higher means more movement as the ball moves left and right
Const offsetX = 0				'Offset x position under ball (These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY = 5				'Offset y position under ball (^^for example 5,5 if the light is in the back left corner)

'Dynamic (Table light sources)
Const DynamicBSFactor = 0.90	'0 to 1, higher is darker
Const Wideness = 20				'Sets how wide the dynamic ball shadows can get (20 +5 thinness is technically most accurate for lights at z ~25 hitting a 50 unit ball)
Const Thinness = 5				'Sets minimum as ball moves away from source

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!  (will throw errors if there aren't enough objects)
Dim objrtx1(7), objrtx2(7)
Dim objBallShadow(7)
Dim OnPF(7)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5,BallShadowA6,BallShadowA7)
Dim DSSources(30), numberofsources', DSGISide(30) 'Adapted for TZ with GI left / GI right

' *** The Shadow Dictionary
Dim bsDict
Set bsDict = New cvpmDictionary
Const bsNone = "None"
Const bsWire = "Wire"
Const bsRamp = "Ramp"
Const bsRampClear = "Clear"

'Initialization
DynamicBSInit

Sub DynamicBSInit()
	Dim iii, source
	
	'Prepare the shadow objects before play begins
	For iii = 0 To tnob - 1
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = 1 + iii / 1000 + 0.01  'Separate z for layering without clipping
		objrtx1(iii).visible = 0
		
		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = 1 + iii / 1000 + 0.02
		objrtx2(iii).visible = 0
		
		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 1 + iii / 1000 + 0.04
		objBallShadow(iii).visible = 0
		
		BallShadowA(iii).Opacity = 100 * AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next
	
	iii = 0
	
	For Each Source In DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
		'   If Instr(Source.name , "Left") > 0 Then DSGISide(iii) = 0 Else DSGISide(iii) = 1	'Adapted for TZ with GI left / GI right
		iii = iii + 1
	Next
	numberofsources = iii
End Sub

Sub BallOnPlayfieldNow(onPlayfield, ballNum)	'Whether a ball is currently on the playfield. Only update certain things once, save some cycles
	If onPlayfield Then
		OnPF(ballNum) = True
		bsRampOff BOT(ballNum).ID
		'   debug.print "Back on PF"
		UpdateMaterial objBallShadow(ballNum).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(ballNum).size_x = 5
		objBallShadow(ballNum).size_y = 4.5
		objBallShadow(ballNum).visible = 1
		BallShadowA(ballNum).visible = 0
		BallShadowA(ballNum).Opacity = 100 * AmbientBSFactor
	Else
		OnPF(ballNum) = False
		'   debug.print "Leaving PF"
	End If
End Sub

Sub DynamicBSUpdate
	Dim falloff 'Max distance to light sources, can be changed dynamically if you have a reason
	falloff = 150
	Dim ShadowOpacity1, ShadowOpacity2
	Dim s, LSd, iii
	Dim dist1, dist2, src1, src2
	Dim bsRampType
	'   Dim BOT: BOT=getballs	'Uncomment if you're destroying balls - Not recommended! #SaveTheBalls
	
	'Hide shadow of deleted balls
	For s = UBound(BOT) + 1 To tnob - 1
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next
	
	If UBound(BOT) < lob Then Exit Sub 'No balls in play, exit
	
	'The Magic happens now
	For s = lob To UBound(BOT)
		' *** Normal "ambient light" ball shadow
		'Layered from top to bottom. If you had an upper pf at for example 80 units and ramps even above that, your Elseif segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else (under 20)
		
		'Primitive shadow on playfield, flasher shadow in ramps
		If AmbientBallShadowOn = 1 Then
			'** Above the playfield
			If BOT(s).Z > 30 Then
				If OnPF(s) Then BallOnPlayfieldNow False, s		'One-time update
				bsRampType = getBsRampType(BOT(s).id)
				'   debug.print bsRampType
				
				If Not bsRampType = bsRamp Then 'Primitive visible on PF
					objBallShadow(s).visible = 1
					objBallShadow(s).X = BOT(s).X + (BOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
					objBallShadow(s).Y = BOT(s).Y + offsetY
					objBallShadow(s).size_x = 5 * ((BOT(s).Z + BallSize) / 80) 'Shadow gets larger and more diffuse as it moves up
					objBallShadow(s).size_y = 4.5 * ((BOT(s).Z + BallSize) / 80)
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor * (30 / (BOT(s).Z)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else 'Opaque, no primitive below
					objBallShadow(s).visible = 0
				End If
				
				If bsRampType = bsRampClear Or bsRampType = bsRamp Then 'Flasher visible on opaque ramp
					BallShadowA(s).visible = 1
					BallShadowA(s).X = BOT(s).X + offsetX
					BallShadowA(s).Y = BOT(s).Y + offsetY + BallSize / 10
					BallShadowA(s).height = BOT(s).z - BallSize / 4 + s / 1000 'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
					If bsRampType = bsRampClear Then BallShadowA(s).Opacity = 50 * AmbientBSFactor
				ElseIf bsRampType = bsWire Or bsRampType = bsNone Then 'Turn it off on wires or falling out of a ramp
					BallShadowA(s).visible = 0
				End If
				
				'** On pf, primitive only
			ElseIf BOT(s).Z <= 30 And BOT(s).Z > 20 Then
				If Not OnPF(s) Then BallOnPlayfieldNow True, s
				objBallShadow(s).X = BOT(s).X + (BOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
				objBallShadow(s).Y = BOT(s).Y + offsetY
				'   objBallShadow(s).Z = BOT(s).Z + s/1000 + 0.04		'Uncomment (and adjust If/Elseif height logic) if you want the primitive shadow on an upper/split pf																																						 
				
				'** Under pf, flasher shadow only
			Else
				If OnPF(s) Then BallOnPlayfieldNow False, s
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 1
				BallShadowA(s).X = BOT(s).X + offsetX
				BallShadowA(s).Y = BOT(s).Y + offsetY
				BallShadowA(s).height = BOT(s).z - BallSize / 4 + s / 1000
			End If
			
			'Flasher shadow everywhere
		ElseIf AmbientBallShadowOn = 2 Then
			If BOT(s).Z > 30 Then 'In a ramp
				BallShadowA(s).X = BOT(s).X + offsetX
				BallShadowA(s).Y = BOT(s).Y + offsetY + BallSize / 10
				BallShadowA(s).height = BOT(s).z - BallSize / 4 + s / 1000 'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			ElseIf BOT(s).Z <= 30 And BOT(s).Z > 20 Then 'On pf
				BallShadowA(s).visible = 1
				BallShadowA(s).X = BOT(s).X + (BOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
				BallShadowA(s).Y = BOT(s).Y + offsetY
				BallShadowA(s).height = 1.04 + s / 1000
			Else 'Under pf
				BallShadowA(s).X = BOT(s).X + offsetX
				BallShadowA(s).Y = BOT(s).Y + offsetY
				BallShadowA(s).height = BOT(s).z - BallSize / 4 + s / 1000
			End If
		End If
		
		' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If BOT(s).Z < 30 And BOT(s).X < 850 Then 'Parameters for where the shadows can show, here they are not visible above the table (no upper pf) or in the plunger lane
				dist1 = falloff
				dist2 = falloff
				For iii = 0 To numberofsources - 1 'Search the 2 nearest influencing lights
					LSd = Distance(BOT(s).x, BOT(s).y, DSSources(iii)(0), DSSources(iii)(1)) 'Calculating the Linear distance to the Source
					If LSd < falloff And gilvl > 0 Then
						'   If LSd < dist2 And ((DSGISide(iii) = 0 And Lampz.State(100)>0) Or (DSGISide(iii) = 1 And Lampz.State(104)>0)) Then	'Adapted for TZ with GI left / GI right
						dist2 = dist1
						dist1 = LSd
						src2 = src1
						src1 = iii
					End If
				Next
				ShadowOpacity1 = 0
				If dist1 < falloff Then
					objrtx1(s).visible = 1
					objrtx1(s).X = BOT(s).X
					objrtx1(s).Y = BOT(s).Y
					'   objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx1(s).rotz = AnglePP(DSSources(src1)(0), DSSources(src1)(1), BOT(s).X, BOT(s).Y) + 90
					ShadowOpacity1 = 1 - dist1 / falloff
					objrtx1(s).size_y = Wideness * ShadowOpacity1 + Thinness
					UpdateMaterial objrtx1(s).material,1,0,0,0,0,0,ShadowOpacity1 * DynamicBSFactor ^ 3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx1(s).visible = 0
				End If
				ShadowOpacity2 = 0
				If dist2 < falloff Then
					objrtx2(s).visible = 1
					objrtx2(s).X = BOT(s).X
					objrtx2(s).Y = BOT(s).Y + offsetY
					'   objrtx2(s).Z = BOT(s).Z - 25 + s/1000 + 0.02 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx2(s).rotz = AnglePP(DSSources(src2)(0), DSSources(src2)(1), BOT(s).X, BOT(s).Y) + 90
					ShadowOpacity2 = 1 - dist2 / falloff
					objrtx2(s).size_y = Wideness * ShadowOpacity2 + Thinness
					UpdateMaterial objrtx2(s).material,1,0,0,0,0,0,ShadowOpacity2 * DynamicBSFactor ^ 3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx2(s).visible = 0
				End If
				If AmbientBallShadowOn = 1 Then
					'Fades the ambient shadow (primitive only) when it's close to a light
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor * (1 - max(ShadowOpacity1, ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					BallShadowA(s).Opacity = 100 * AmbientBSFactor * (1 - max(ShadowOpacity1, ShadowOpacity2))
				End If
			Else 'Hide dynamic shadows everywhere else, just in case
				objrtx2(s).visible = 0
				objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub

' *** Ramp type definitions

Sub bsRampOnWire()
	If bsDict.Exists(ActiveBall.ID) Then
		bsDict.Item(ActiveBall.ID) = bsWire
	Else
		bsDict.Add ActiveBall.ID, bsWire
	End If
End Sub

Sub bsRampOn()
	If bsDict.Exists(ActiveBall.ID) Then
		bsDict.Item(ActiveBall.ID) = bsRamp
	Else
		bsDict.Add ActiveBall.ID, bsRamp
	End If
End Sub

Sub bsRampOnClear()
	If bsDict.Exists(ActiveBall.ID) Then
		bsDict.Item(ActiveBall.ID) = bsRampClear
	Else
		bsDict.Add ActiveBall.ID, bsRampClear
	End If
End Sub

Sub bsRampOff(idx)
	If bsDict.Exists(idx) Then
		bsDict.Item(idx) = bsNone
	End If
End Sub

Function getBsRampType(id)
	Dim retValue
	If bsDict.Exists(id) Then
		retValue = bsDict.Item(id)
	Else
		retValue = bsNone
	End If
	getBsRampType = retValue
End Function

'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************



if dullard = 1 Then
	dullardL.collidable = 1
	dullardR.collidable = 1
	cowardL.collidable = 0
	cowardR.collidable = 0
else
	dullardL.collidable = 0
	dullardR.collidable = 0
end If

if coward = 1 Then
	dullardL.collidable = 0
	dullardR.collidable = 0
	cowardL.collidable = 1
	cowardR.collidable = 1
Else
	cowardL.collidable = 0
	cowardR.collidable = 0
end if


'************************************
'         Desktop LEDs Display
'     Based on Scapino's LEDs
'************************************

Dim Digits(28)
Dim Patterns(11)
Dim Patterns2(11)

Patterns(0) = 0     'empty
Patterns(1) = 63    '0
Patterns(2) = 6     '1
Patterns(3) = 91    '2
Patterns(4) = 79    '3
Patterns(5) = 102   '4
Patterns(6) = 109   '5
Patterns(7) = 125   '6
Patterns(8) = 7     '7
Patterns(9) = 127   '8
Patterns(10) = 111  '9

Patterns2(0) = 128  'empty
Patterns2(1) = 191  '0
Patterns2(2) = 134  '1
Patterns2(3) = 219  '2
Patterns2(4) = 207  '3
Patterns2(5) = 230  '4
Patterns2(6) = 237  '5
Patterns2(7) = 253  '6
Patterns2(8) = 135  '7
Patterns2(9) = 255  '8
Patterns2(10) = 239 '9

'Assign 6-digit output to reels
Set Digits(0) = a0
Set Digits(1) = a1
Set Digits(2) = a2
Set Digits(3) = a3
Set Digits(4) = a4
Set Digits(5) = a5

Set Digits(6) = a6
Set Digits(7) = b0
Set Digits(8) = b1
Set Digits(9) = b2
Set Digits(10) = b3
Set Digits(11) = b4

Set Digits(12) = b5
Set Digits(13) = b6
Set Digits(14) = c0
Set Digits(15) = c1
Set Digits(16) = c2
Set Digits(17) = c3

Set Digits(18) = c4
Set Digits(19) = c5
Set Digits(20) = c6
Set Digits(21) = d000
Set Digits(22) = d001
Set Digits(23) = d002

Set Digits(24) = d003
Set Digits(25) = d004
Set Digits(26) = d005
Set Digits(27) = d006

Sub UpdateLeds
    On Error Resume Next
    Dim ChgLED, ii, jj, chg, stat
    ChgLED = Controller.ChangedLEDs(&HFF, &HFFFF)
    If Not IsEmpty(ChgLED)Then
        For ii = 0 To UBound(ChgLED)
            chg = chgLED(ii, 1):stat = chgLED(ii, 2)
            For jj = 0 to 10
                If stat = Patterns(jj)OR stat = Patterns2(jj)then Digits(chgLED(ii, 0)).SetValue jj
            Next
        Next
    End IF
End Sub



'*****************************************************************************************************
' VR PLUNGER ANIMATION
'*****************************************************************************************************

Sub TimerPlunger_Timer
  If VR_Primary_plunger.Y < 2250 then
  		VR_Primary_plunger.Y = VR_Primary_plunger.Y + 5
  End If
End Sub

Sub TimerPlunger2_Timer
  VR_Primary_plunger.Y = 2115 + (5* Plunger.Position) - 20
End Sub

Dim VRBGObj

Sub SetBackglass()
	For Each VRBGObj In VRBackglass
		VRBGObj.x = VRBGObj.x
		VRBGObj.height = - VRBGObj.y - 100
		VRBGObj.y = 115 'adjusts the distance from the backglass towards the user
		VRBGObj.Rotx = -90
	Next
End Sub

' ******************************************************************************************
'      LAMP CALLBACK for the 6 backglass flasher lamps (not the solenoid conrolled ones)
' ******************************************************************************************

If VR_Room = 1 Then Set LampCallback = GetRef("UpdateMultipleLamps")

Sub UpdateMultipleLamps()
	If Controller.Lamp(45) = 0 Then: BGGameOver.visible=0 : else: BGGameOver.visible=1 'Game Over
	If Controller.Lamp(13) = 0 Then: FlBGL29.visible=0: else: FlBGL29.visible=1 'High Score To Date
	If Controller.Lamp(63) = 0 Then: FlBGL63.visible=0: BGBulb009.visible = 0: else: FlBGL63.visible=1: BGBulb009.visible = 1 'Match
	If Controller.Lamp(61) = 0 Then: FlBGL61.visible=0 : else: FlBGL61.visible=1 'Tilt
	If Controller.Lamp(11) = 0 Then: FlBGL43.visible=0: else: FlBGL43.visible=1 'Shoot Again
End Sub

if VR_Room = 0 Then Set LampCallback = GetRef("UpdateDTLamps")

Sub UpdateDTLamps()
	If Controller.Lamp(45) = 0 Then: GameOverReel.setValue(0):	Else: GameOverReel.setValue(1) 'Game Over
	If Controller.Lamp(63) = 0 Then: MatchReel.setValue(0):		Else: MatchReel.setValue(1) 'Match
	If Controller.Lamp(61) = 0 Then: TiltReel.setValue(0):		Else: TiltReel.setValue(1) 'Tilt
	If Controller.Lamp(13) = 0 Then: HighScoreReel.setValue(0):	Else: HighScoreReel.setValue(1) 'High Score
	If Controller.Lamp(11) = 0 Then: ShootAgainReel.setValue(0):	Else: ShootAgainReel.setValue(1) 'Shoot Again
End Sub


Dim DigitsVR(28)
DigitsVR(0) = Array(LED1x0,LED1x1,LED1x2,LED1x3,LED1x4,LED1x5,LED1x6)
DigitsVR(1) = Array(LED2x0,LED2x1,LED2x2,LED2x3,LED2x4,LED2x5,LED2x6)
DigitsVR(2) = Array(LED3x0,LED3x1,LED3x2,LED3x3,LED3x4,LED3x5,LED3x6)
DigitsVR(3) = Array(LED4x0,LED4x1,LED4x2,LED4x3,LED4x4,LED4x5,LED4x6)
DigitsVR(4) = Array(LED5x0,LED5x1,LED5x2,LED5x3,LED5x4,LED5x5,LED5x6)
DigitsVR(5) = Array(LED6x0,LED6x1,LED6x2,LED6x3,LED6x4,LED6x5,LED6x6)
DigitsVR(6) = Array(LED7x0,LED7x1,LED7x2,LED7x3,LED7x4,LED7x5,LED7x6)

DigitsVR(7) = Array(LED8x0,LED8x1,LED8x2,LED8x3,LED8x4,LED8x5,LED8x6)
DigitsVR(8) = Array(LED9x0,LED9x1,LED9x2,LED9x3,LED9x4,LED9x5,LED9x6)
DigitsVR(9) = Array(LED10x0,LED10x1,LED10x2,LED10x3,LED10x4,LED10x5,LED10x6)
DigitsVR(10) = Array(LED11x0,LED11x1,LED11x2,LED11x3,LED11x4,LED11x5,LED11x6)
DigitsVR(11) = Array(LED12x0,LED12x1,LED12x2,LED12x3,LED12x4,LED12x5,LED12x6)
DigitsVR(12) = Array(LED13x0,LED13x1,LED13x2,LED13x3,LED13x4,LED13x5,LED13x6)
DigitsVR(13) = Array(LED14x0,LED14x1,LED14x2,LED14x3,LED14x4,LED14x5,LED14x6)

DigitsVR(14) = Array(LED1x000,LED1x001,LED1x002,LED1x003,LED1x004,LED1x005,LED1x006)
DigitsVR(15) = Array(LED1x100,LED1x101,LED1x102,LED1x103,LED1x104,LED1x105,LED1x106)
DigitsVR(16) = Array(LED1x200,LED1x201,LED1x202,LED1x203,LED1x204,LED1x205,LED1x206)
DigitsVR(17) = Array(LED1x300,LED1x301,LED1x302,LED1x303,LED1x304,LED1x305,LED1x306)
DigitsVR(18) = Array(LED1x400,LED1x401,LED1x402,LED1x403,LED1x404,LED1x405,LED1x406)
DigitsVR(19) = Array(LED1x500,LED1x501,LED1x502,LED1x503,LED1x504,LED1x505,LED1x506)
DigitsVR(20) = Array(LED1x600,LED1x601,LED1x602,LED1x603,LED1x604,LED1x605,LED1x606)

DigitsVR(21) = Array(LED2x000,LED2x001,LED2x002,LED2x003,LED2x004,LED2x005,LED2x006)
DigitsVR(22) = Array(LED2x100,LED2x101,LED2x102,LED2x103,LED2x104,LED2x105,LED2x106)
DigitsVR(23) = Array(LED2x200,LED2x201,LED2x202,LED2x203,LED2x204,LED2x205,LED2x206)
DigitsVR(24) = Array(LED2x300,LED2x301,LED2x302,LED2x303,LED2x304,LED2x305,LED2x306)
DigitsVR(25) = Array(LED2x400,LED2x401,LED2x402,LED2x403,LED2x404,LED2x405,LED2x406)
DigitsVR(26) = Array(LED2x500,LED2x501,LED2x502,LED2x503,LED2x504,LED2x505,LED2x506)
DigitsVR(27) = Array(LED2x600,LED2x601,LED2x602,LED2x603,LED2x604,LED2x605,LED2x606)

dim DisplayColor

DisplayColor =  RGB(255,40,1)

Sub DisplayTimer_timer
	If VR_Room = 1 Then
		Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
		ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
		If Not IsEmpty(ChgLED)Then
		   For ii=0 To UBound(chgLED)
			  num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
				if (num < 28) then
					For Each obj In DigitsVR(num)
	'               If chg And 1 Then obj.visible=stat And 1
					If chg And 1 Then FadeDisplay obj, stat And 1	
					   chg=chg\2 : stat=stat\2
					Next
				Else
				end if
			Next
		End If
	End If
End Sub

Sub FadeDisplay(object, onoff)
	If OnOff = 1 Then
		object.color = DisplayColor
		Object.Opacity = 600
	Else
		Object.Color = RGB(1,1,1)
		Object.Opacity = 10
	End If
End Sub

Sub InitDigitsVR()
	dim tmp, x, obj
	for x = 0 to uBound(DigitsVR)
		if IsArray(DigitsVR(x) ) then
			For each obj in DigitsVR(x)
				obj.height = obj.height + 20
				FadeDisplay obj, 0
			next
		end If
	Next
End Sub

InitDigitsVR
