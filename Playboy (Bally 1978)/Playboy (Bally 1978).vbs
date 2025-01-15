Option Explicit
'*****************************************************************************************************
' CREDITS
' Initial table created by fuzzel, jimmyfingers, jpsalas, toxie & unclewilly (in alphabetical order)
' Flipper primitives by zany
' Ball rolling sound script by jpsalas
' Ball shadow by ninuzzu
' Ball control by rothbauerw
' DOF by arngrim
' Positional sound helper functions by djrobx
' Plus a lot of input from the whole community (sorry if we forgot you :/)
'*****************************************************************************************************
Randomize
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0

LoadVPM "01130100", "Bally.VBS", 3.36
Dim EnableBallControl,Objekt,bsTrough,bsSaucer,dtRBank
Const FlippersAlwaysOn 	= 0 
Const cGameName 		= "playboyb"  
Const UseSolenoids 		= 1
Const UseLamps 			= 1
Const UseGI 			= 0
Const UseSync 			= 0
Const HandleMech 		= 0
Const SSolenoidOn 		= "SolOn"
Const SSolenoidOff 		= "SolOff"
Const SCoin 			= "Coin"
Const SKnocker 			= "Knocker"
Const SFlipperOn		= "fx_flipperup"
Const SFlipperOff		= "fx_flipperdown"
SolCallback(7) 			= "bsTrough.SolOut"
SolCallback(8)     		= "bsSaucer.SolOut"
SolCallback(13) 		= "dtRBank.SolDropUp"
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
' SolCallback(2)     		= "vpmSolSound Soundfx(""Knocker"",DOFKnocker)," 

Set vpmShowDips 		= GetRef("editDips")
Set LampCallback		= GetRef("UpdateMultipleLamps")

EnableBallControl = false 'Change to true to enable manual ball control (or press C in-game) via the arrow keys and B (boost movement) keys

Sub Table1_Init
'	vpmInit Me
	On Error Resume Next
	With Controller
		.GameName				= cGameName
		.SplashInfoLine			= "Playboy Bally 1978"
		.HandleKeyboard 		= 0
		.ShowTitle				= 0
		.ShowDMDOnly			= 1
		.ShowFrame				= 0
		.ShowTitle 				= 0
		.Games(cGameName)
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
	End With
	On Error Goto 0

	PinMAMETimer.Interval	= PinMAMEInterval
	PinMAMETimer.Enabled	= True
	vpmMapLights aLights

	vpmNudge.TiltSwitch		= 7
	vpmNudge.Sensitivity	= 3
	vpmNudge.TiltObj 		= Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)
 
 Set bsTrough=new cvpmBallStack
   With bsTrough
	.InitSw 0,1,0,0,0,0,0,0
	.InitKick BallRelease,90,5
	.InitExitSnd SoundFX("BallRelease",DOFContactors),Soundfx("Solenoid",DOFContactors)
	.Balls=1
	End With
 
 Set bsSaucer=new cvpmBallStack
   With bsSaucer
	.InitSaucer Kicker1, 32, 0, 26
	.InitExitSnd SoundFX("Popper_Ball",DOFContactors),SoundFX("Popper",DOFContactors)
	.KickForceVar = 5
	End With
 
 Set dtRBank = New cvpmDropTarget
	With dtRBank
	.InitDrop Array (Target7, Target8, Target9, Target10, Target11), array (1,2,3,4,5)
    .InitSnd SoundFX("Target",DOFDropTargets),SoundFX("fx_resetdrop",DOFContactors)
	End With
	
	If Table1.ShowDT = False then
		for each objekt in Backdropstuff
		objekt.visible = False
			Next
	End If 
End Sub
 



Sub SolLFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFFlippers):LeftFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFFlippers):LeftFlipper.RotateToStart
     End If
  End Sub
  
Sub SolRFlipper(Enabled)
     If Enabled Then
         PlaySound SoundFX("fx_Flipperup",DOFFlippers):RightFlipper.RotateToEnd
     Else
         PlaySound SoundFX("fx_Flipperdown",DOFFlippers):RightFlipper.RotateToStart
     End If
End Sub


Sub Table1_KeyDown(ByVal keycode)
	If vpmKeyDown(keycode) Then Exit Sub
	If keycode = PlungerKey Then
		Plunger.PullBack
		PlaySound "plungerpull",0,1,AudioPan(Plunger),0.25,0,0,1,AudioFade(Plunger)
	End If

	If FlippersAlwaysOn = 1 then
	If keycode = LeftFlipperKey Then
		LeftFlipper.RotateToEnd
		PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, .67, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
	End If

	If keycode = RightFlipperKey Then
		RightFlipper.RotateToEnd
		PlaySound SoundFX("fx_flipperup",DOFFlippers), 0, .67, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
	End If
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
	
End Sub

Sub Table1_KeyUp(ByVal keycode)
	If vpmKeyUp(keycode) Then Exit Sub
	If keycode = PlungerKey Then
		Plunger.Fire
		PlaySound "plunger",0,1,AudioPan(Plunger),0.25,0,0,1,AudioFade(Plunger)
	End If
	
	If FlippersAlwaysOn = 1 then
	If keycode = LeftFlipperKey Then
		LeftFlipper.RotateToStart
		PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
	End If

	If keycode = RightFlipperKey Then
		RightFlipper.RotateToStart
		PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(RightFlipper), 0.05,0,0,1,AudioFade(RightFlipper)
	End If
End If

    'Manual Ball Control
	If EnableBallControl = 1 Then
		If keycode = 203 Then BCleft 	= 0	' Left Arrow
		If keycode = 200 Then BCup 		= 0		' Up Arrow
		If keycode = 208 Then BCdown 	= 0	' Down Arrow
		If keycode = 205 Then BCright 	= 0	' Right Arrow
	End If
End Sub

'InitSwitches
'Sub InitSwitches: End Sub

Sub Target5_Hit	: vpmTimer.pulseSw 25: End Sub
Sub Target4_Hit	: vpmTimer.pulseSw 26: End Sub
Sub Target3_Hit	: vpmTimer.pulseSw 27: End Sub
Sub Target2_Hit	: vpmTimer.pulseSw 28: End Sub
Sub Target1_Hit	: vpmTimer.pulseSw 29: End Sub
Sub Target7_Hit	: vpmTimer.pulseSw 01: End Sub
Sub Target8_Hit	: vpmTimer.pulseSw 02: End Sub
Sub Target9_Hit	: vpmTimer.pulseSw 03: End Sub
Sub Target10_Hit: vpmTimer.pulseSw 04: End Sub
Sub Target11_Hit: vpmTimer.pulseSw 05: End Sub
Sub Target6_Hit : vpmTimer.pulseSw 17: End Sub

Sub Bumper1_Hit:  vpmTimer.pulseSw 40: RandomSoundBumper:End Sub
Sub Bumper2_Hit:  vpmTimer.pulseSw 39: RandomSoundBumper:End Sub
Sub Bumper3_Hit:  vpmTimer.pulseSw 38: RandomSoundBumper:End Sub

Sub Wall6_Hit: 	  vpmTimer.pulseSw 33: End Sub

Sub Trigger2_Hit: 	Controller.Switch (30) = 1: End Sub
Sub Trigger2_UnHit: Controller.Switch (30) = 0: End Sub
Sub Trigger3_Hit: 	controller.switch (18) = 1: End Sub
Sub Trigger3_UnHit:	controller.switch (18) = 0: End Sub
Sub Trigger4_Hit: 	controller.switch (19) = 1: End Sub
Sub Trigger4_UnHit: controller.switch (19) = 0: End Sub
Sub Trigger5_Hit: 	controller.switch (20) = 1: End Sub
Sub Trigger5_UnHit: controller.switch (20) = 0: End Sub
Sub Trigger6_Hit: 	controller.switch (21) = 1: End Sub
Sub Trigger6_UnHit: controller.switch (21) = 0: End Sub
Sub Trigger8_Hit: 	controller.switch (31) = 1: End Sub
Sub Trigger8_UnHit: controller.switch (31) = 0: End Sub
Sub Trigger9_Hit: 	controller.switch (23) = 1: End Sub
Sub Trigger9_UnHit: controller.switch (23) = 0: End Sub
Sub Trigger10_Hit: 	controller.switch (24) = 1: End Sub
Sub Trigger10_UnHit:controller.switch (24) = 0: End Sub
Sub Trigger11_Hit: 	controller.switch (24) = 1: End Sub
Sub Trigger11_UnHit:controller.switch (24) = 0: End Sub
Sub Trigger12_Hit: 	controller.switch (22) = 1: End Sub
Sub Trigger12_UnHit:controller.switch (22) = 0: End Sub

Sub Kicker1_Hit: bsSaucer.AddBall Me: End Sub
Sub Drain_Hit(): vpmTimer.pulseSw 8: PlaySound "Drain",0,1,AudioPan(Drain),0.25,0,0,1,AudioFade(Drain):bsTrough.AddBall Me:End Sub

'**********************************************
'Drop Target Lighting
'**********************************************
Sub DropTargetLights_Timer()
	If Target7.isdropped 	then DTlight.state 	= 1 Else DTLight.state=0 
	If Target8.isdropped 	then DTlight1.state = 1 Else DTlight1.state=0
	If Target9.isdropped 	then DTlight2.state = 1 Else DTlight2.state=0
	If Target10.isdropped 	then DTlight3.state = 1 Else DTlight3.state=0
	If Target11.isdropped 	then DTlight4.state = 1 Else DTlight4.state=0
End Sub

'*****GI Lights On
dim xx
For each xx in GI:xx.State = 1: Next

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot:vpmTimer.pulseSw 36
	PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:gi1.State = 1:Gi2.State = 1
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot:vpmTimer.pulseSw 37
	PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:gi3.State = 1:Gi4.State = 1
    End Select
    LStep = LStep + 1
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
'
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


'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 5 ' total number of balls
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
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
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


'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle

End Sub

'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************
Dim BallShadow,Ballsize
Ballsize	=	50
BallShadow 	= 	Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

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
        If BOT(b).X < Table1.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 6
        End If
        ballShadow(b).Y = BOT(b).Y + 12
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub
'***********************************************************

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
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
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub RandomSoundBumper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound SoundFX("fx_bumper1",DOFContactors)
		Case 2 : PlaySound SoundFX("fx_bumper2",DOFContactors)
		Case 3 : PlaySound SoundFX("fx_bumper3",DOFContactors)
	End Select
End Sub

'Bally Playboy
'added by Inkochnito
Sub editDips
	Dim vpmDips : Set vpmDips = New cvpmDips
	With vpmDips
		.AddForm 700,400,"Playboy - DIP switches"
		.AddChk 2,10,180,Array("Match feature",&H00100000)'dip 21
		.AddChk 205,10,115,Array("Credits display",&H00080000)'dip 20
		.AddFrame 2,30,190,"Maximum credits",&H00070000,Array("5 credits",0,"10 credits",&H00010000,"15 credits",&H00020000,"20 credits",&H00030000,"25 credits",&H00040000,"30 credits",&H00050000,"35 credits",&H00060000,"40 credits",&H00070000)'dip 17&18&19
		.AddFrame 2,160,190,"Sound features",&H80000080,Array("chime effects",&H80000000,"chime and tunes",0,"noise",&H00000080,"noises and tunes",&H80000080)'dip 8&32
		.AddFrame 2,235,190,"High game to date",&H00000060,Array("no award",0,"1 credit",&H00000020,"2 credits",&H00000040,"3 credits",&H00000060)'dip 6&7
		.AddFrame 2,310,190,"High score feature",&H00006000,Array("no award",0,"extra ball",&H00004000,"replay",&H00006000)'dip 14&15
		.AddFrame 205,30,190,"Balls per game", 32768,Array("3 balls",0,"5 balls", 32768)'dip 16
		.AddFrame 205,76,190,"Drop target special remains lit",&H00200000,Array("until ball goes in outhole",0,"until collected",&H00200000)'dip 22
		.AddFrame 205,122,190,"Key lites made are",&H00400000,Array ("not held over",0,"held over until next ball",&H00400000)'dip 23
		.AddFrame 205,168,190,"Outlane 25,000 adjustment",&H00800000,Array("25,000 lite alternates",0,"both lanes lite for 25,000",&H00800000)'dip 24
		.AddFrame 205,214,190,"2 and 3 key lane are",&H10000000,Array("not tied together",0,"tied together",&H10000000)'dip 29
		.AddFrame 205,260,190,"1 and 4 key lane are",&H20000000,Array("not tied together",0,"tied together",&H20000000)'dip 30
		.AddFrame 205,306,190,"Extra ball and/or special",&H40000000,Array("are not held",0,"are held until made",&H40000000)'dip 31
		.AddLabel 50,382,300,20,"After hitting OK, press F3 to reset game with new settings."
		.ViewDips
	End With
End Sub

	 
'************************************
'          LEDs Display
'     Based on Scapino's LEDs
'************************************

Dim Digits(32)
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

'Assign 7-digit output to reels
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

Set Digits(21) = d0
Set Digits(22) = d1
Set Digits(23) = d2
Set Digits(24) = d3
Set Digits(25) = d4
Set Digits(26) = d5
Set Digits(27) = d6

Set Digits(28) = e0
Set Digits(29) = e1
Set Digits(30) = e2
Set Digits(31) = e3

Sub LedTimer_Timer()
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

'BackDrop Lights
 Dim BDL

Sub UpdateMultipleLamps

	BDL=Controller.Lamp(29) 'HS to Date
	If BDL Then
		HighScore_Box.text="HIGH SCORE TO DATE"
	  Else
		HighScore_Box.text=""
	End If

	BDL=Controller.Lamp(45) 'Game Over
	If BDL Then
		GameOver_Box.text="GAME OVER"
	  Else
		GameOver_Box.text=""
	End If

	BDL=Controller.Lamp(61) 'Tilt
	If BDL Then
		Tilt_Box.text="TILT"
	  Else
		Tilt_Box.text=""
	End If

	BDL=Controller.Lamp(11) 'Shoot Again
	If BDL Then
		ShootAgain_Box.text="SHOOT AGAIN"
	  Else
		ShootAgain_Box.text=""
	End If

	BDL=Controller.Lamp(14) '1 Player
	If BDL Then
		OnePlayer_Box.text="1"
	  Else
		OnePlayer_Box.text=""
	End If

	BDL=Controller.Lamp(30) '2 Player
	If BDL Then
		TwoPlayer_Box.text="2"
	  Else
		TwoPlayer_Box.text=""
	End If

	BDL=Controller.Lamp(46) '3 Player
	If BDL Then
		ThreePlayer_Box.text="3"
	  Else
		ThreePlayer_Box.text=""
	End If

	BDL=Controller.Lamp(62) '4 Player
	If BDL Then
		FourPlayer_Box.text="4"
	  Else
		FourPlayer_Box.text=""
	End If
 End Sub