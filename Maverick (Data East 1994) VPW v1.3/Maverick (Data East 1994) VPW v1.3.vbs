'Maverick - Data East 1994
'https://www.ipdb.org/machine.cgi?id=1561

'VPW Table Tuneup Mod v1.0
'Based on the vp9 version by fuzzel, jimmyfingers, jpsalas, toxie & unclewilly.
'
'Antisect - 99% of the table construction and work!
'Edizzle - Donating  his script for the rotating wheel from his vp9 table and advice.
'Tomate - Primitive ramps and flippers, primitive assistance
'Sixtoe - VR Room & general fixes and tweaks
'Apophis - Dynamic ball shadows, physics additions, scripting stuff, playfield cleanup and redrawing
'Flupper - Ramp assistance and tutorial
'Fluffhead35 - Wire rolling sounds.
'Ebislit - Playfield alignment assistance.
'Dark - Lauren Bell Boat Model
'iaakki & daphishbowl - script assistance
'Rik & VPW Team - Testing
'The whole VPW Team, for their assistance and support
'
'*****************************************
'			VPin Workshop Revisions
'*****************************************
'
'001 - 039 - Antisect - 99% of table completed.
'040 - Sixtoe - Added VR table and scripted in some bits and pieces, physics tweaked, loads of old and/or redundant script stripped out and things rescripted, redirected game load to de.vbs, fixed drop target bricking, adjusted kickback and plunger, should be more reliable? not sure how strong the plunger is supposed to be.
'041 - apophis - Added Wylte's RTX dynamic ball shadows. Removed old ball shadow stuff, Increased plunger strength and speed. Removed duplicate sound related functions.
'042 - apophis - Updated RTX BS to current version
'043 - fluffhead35 - Adding RampRoll Sound Loops, Added new RampSound Triggers to turn RampRolling on and Off on table
'044 - apophis - Fixed ball release bug. Added target bouncer, flipper rubberizer and coil rampup mods. Tweaked RTX shadows. Reduced skillshot autoplunger strength.
'045 - Sixtoe - Compressed some webp images, converted audio from wav to mp3 (cut 90meg off the file size),
'046 - apophis - Work around for ball release bug
'047 - Sixtoe - Bugfixes
'1.0.1 - Apophis - Positional sound calls added / updated (specifically for the VUK etc.)
'1.0.2 - Sixtoe - Flashers updated / tweaked, desktop room tweaked, plunger tweaked.
'1.1 - Sixtoe - Reverted all mp3's to wavs (should fix all audio issues, there is mp3 support in 10.7, but not for things like ball rolling etc.), Flashers updated / tweaked, Desktop background tweaked, Plunger speed tweaked (you should be able to make the skill shot more reliably now, Updated credits
'1.2 RC1 - Sixtoe - Flipper power adjusted and lowered and table gradient reduced (thanks Rik and Randy), plunger adjusted (thanks to Goldchicco & Rik)
'1.2 RC2 - apophis - Removed ball shadow z position dependency on ball z position.
'1.2 - Sixtoe - Final tweaks and tidying
'1.2.1 - apophis - Added Tilt functionality. Reduced nudge strength. Fixed captured ball entrance to prevent stuck ball issue. Added better drop target and relay sounds.
'1.2.2 - apophis - Updated start button logic to allow for High Score entry when balls are locked.
'1.2.3 - apophis - Updated drop target code with help from rothbauerw (thanks!) to handle collisions with ball when target is raised. Swapped 8H and 8D drop target images.

Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0
Const BallSize = 50
Const BallMass = 1

' Options
'///////////////////////-----VR Room-----///////////////////////
Const VRRoom = 0 ' 0 - VR Room off, 1 - Minimal Room, 2 - Ultra Minimal

'/////////////////////-----Cabinet Mode-----/////////////////////
Const CabinetMode = 0	'0 - Off, 1 - Hides rails & scales side panels, 2 - Hides rails and side panels

'/////////////////////-----Dynamic Ball Shadows-----/////////////////////
Const RtxBSon = 1	'0 = no RTX ball shadow, 1 = enable RTX ball shadow

'/////////////////////-----Other Physics Mods-----/////////////////////
Const RubberizerEnabled =	1 		'0 = normal flip rubber, 1 = more lively rubber for flips
Const FlipperCoilRampupMode = 0     '0 = fast, 1 = medium, 2 = slow (tap passes should work)
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 1.0 	'Level of bounces. 0.2 - 1.5 are probably usable values.


Dim DesktopMode:DesktopMode = Table1.ShowDT
Dim UseVPMDMD
If VRRoom <> 0 Then UseVPMDMD = True Else UseVPMDMD = DesktopMode

If DesktopMode = True Then 'Show Desktop components
	plasticRamp.image = "Ramp-DT"
	Else
	plasticRamp.image = "Ramp-FS"
End if

Const cGameName = "mav_402",UseSolenoids=2,UseLamps=0,UseGI=0,SCoin=""

LoadVPM "01120100", "DE.vbs", 3.36

'Dont use Table1.width or Table1.height in script as it can effect performance
dim tablewidth: tablewidth = Table1.width
dim tableheight: tableheight = Table1.height

'///////////////////////-----General Sound Options-----///////////////////////
'// VolumeDial:
'// VolumeDial is the actual global volume multiplier for the mechanical sounds.
'// Values smaller than 1 will decrease mechanical sounds volume.
'// Recommended values should be no greater than 1.
Const VolumeDial = 0.8

Dim EnableBallControl
EnableBallControl = false 'Change to true to enable manual ball control (or press C in-game) via the arrow keys and B (boost movement) keys

'********************
'Standard definitions
'********************

' Standard Sounds
Const SSolenoidOn = "fx_Solenoid"
Const SSolenoidOff = "fx_solenoidoff"

'Solenoids
SolCallBack(1) = "SolLockout"								'4-Ball Ass'y Lockout
SolCallBack(2) = "bsTroughKick.SolOut"						'Ball Release (Eject)
SolCallback(3)  = "SolSkillshot"							'Skill Shot Launch
SolCallBack(4) = "SolLUBankAuto"							'5-Bank Autodrop Down
SolCallback(5) = "SolLUBank"								'5-Bank Autodrop Reset Up
SolCallBack(6) = "SolCDTBank"								'3-Bank Drop Target
SolCallBack(7) = "SolRDTBank"								'4-Bank Drop Target
SolCallback(8)=	"vpmSolSound SoundFX(""Knocker_1"",DOFKnocker),"
SolCallback(9) = "SolRdiv"									'Plastic Ramp Diverter
SolCallBack(11) = "GIRelay"									'G.I. Relay
SolCallBack(12) = "LockRelease"								'Ball Lock Assembly	
SolCallBack(13) = "SolLDTLwrBank"							'5-Bank Lwr. Left D.T.
SolCallback(14)="VukTopPop"
SolCallBack(15) ="SolBallDeflector"							'Upr. Left Ball Deflector
SolCallBack(16) ="SolWheel"									'Paddle Wheel				
'SolCallBack(17) = "vpmSolSound ""jet3"","					'Left Turbo Bumper
'SolCallBack(18) = "vpmSolSound ""jet3"","					'Bottom Turbe Bumper
'SolCallBack(19) = "vpmSolSound ""jet3"","					'Right Turbo Bumper
'SolCallBack(20) = "vpmSolSound ""Sling"","					'Left Slingshot
'SolCallBack(21) = "vpmSolSound ""Sling"","					'Right Slingshot
SolCallBack(22)	= "SolKickBack"								'KickBack
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sURFlipper) = "SolURFlipper"
SolCallBack(25)="FlashSkill"								'Skill Shot Flash
SolCallBack(26)="JokersFlash"								'Jokers Flash
SolCallBack(27)="SetLamp 127,"								'Turbo Bumpers Flash
SolCallBack(28)="FlashPaddle"								'Paddle Wheel Flash
SolCallBack(29)="FlashLLeft"								'Lower Left Flash
'SolCallBack(29)="LLeftFlash"								'Lower Left Flash
SolCallBack(30)="SetLamp 130,"								'Right Drop Target Flash
SolCallBack(31)="SetLamp 131,"								'3-Bank D.T. Flash
SolCallBack(32)="SetLamp 132," 								'Lwr. LT 5-Bank DT Flash

'************
' Table init.
'************

Dim bsTrough, plungerIM, bsLScoop, bsRScoop, mBar, bsTroughKick, vLock

Sub Table1_Init
	
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Maverick Data East 1994"&chr(13)&"VPW"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden = 0
        .Games(cGameName).Settings.Value("sound")=1
		.PuPHide = 1
         On Error Resume Next
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
	End With
	On Error Goto 0

	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled=1

	kickback.PullBack
	SkillShot.PullBack

'* TROUGH *********************************************

	Set bsTrough = new cvpmBallStack
	bsTrough.InitSw 0,14,13,12,11,0,0,0
	bsTrough.Balls = 4
	
	Set bsTroughKick=New cvpmBallStack
	bsTroughKick.InitSaucer BallRelease,15,80,8

'* Ball Lock *********************************************

	Set vLock = New cvpmVLock : With vLock
		.InitVlock Array(sw46, sw45, swE), Array(sw46a, sw45a, swEa),Array(46, 45, 0)
		.InitSnd "ballrelease", "solenoid"
		.CreateEvents "vLock"
	End With

'* Captive Ball *********************************************

	CapKicker1.CreateSizedballWithMass Ballsize/2, BallMass
	CapKicker1.kick 0,0

	diverterWall.isdropped=0
	Deflector_on.isdropped=0
	Deflector_on2.isdropped=1
	
'Load LUT
	LoadLUT

	vpmNudge.TiltSwitch = 1
	vpmNudge.Sensitivity = 1

End Sub

'*********
'   LUT
'*********

Dim bLutActive, LUTImage
Sub LoadLUT
	dim x
	bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "") Then LUTImage = x Else LUTImage = 0
	UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT: LUTImage = (LUTImage +1 ) MOD 12: UpdateLUT: SaveLUT: End Sub

Sub UpdateLUT
Select Case LutImage
Case 0: table1.ColorGradeImage = "LUT0"
Case 1: table1.ColorGradeImage = "LUT1"
Case 2: table1.ColorGradeImage = "LUT2"
Case 3: table1.ColorGradeImage = "LUT3"
Case 4: table1.ColorGradeImage = "LUT4"
Case 5: table1.ColorGradeImage = "LUT5"
Case 6: table1.ColorGradeImage = "LUT6"
Case 7: table1.ColorGradeImage = "LUT7"
Case 8: table1.ColorGradeImage = "LUT8"
Case 9: table1.ColorGradeImage = "LUT9"
Case 10: table1.ColorGradeImage = "LUT10"
Case 11: table1.ColorGradeImage = "LUT11"
End Select
End Sub

'******************************************************
'* GameTimer *****************************************
'******************************************************

Sub Gametimer_Timer
	RDampen
	FlipperTimer
	RollingTimer
	BallControlTimer
	Pincab_Shooter.Y = -20.45162 + (5* Plunger.Position) -20
End Sub

'******************************************************
'* FLIPPERS *******************************************
'******************************************************

Const ReflipAngle = 20

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


Sub SolURFlipper(Enabled)
	If Enabled Then
		URightFlipper.RotateToEnd:RandomSoundReflipUpRight URightFlipper
	Else
		URightFlipper.RotateToStart:RandomSoundFlipperDownRight URightFlipper
	End If
End Sub


Sub URightFlipper_Collide(parm)
        RandomSoundRubberFlipper(parm)
End Sub


Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
	if RubberizerEnabled <> 0 then Rubberizer(parm)
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
	if RubberizerEnabled <> 0 then Rubberizer(parm)
End Sub

sub Rubberizer(parm)
	if parm < 10 And parm > 2 And Abs(activeball.angmomz) < 10 then
		activeball.angmomz = -activeball.angmomz * 2
		activeball.vely = activeball.vely * 1.2
	Elseif parm <= 2 and parm > 0.2 Then
		activeball.angmomz = -activeball.angmomz * 0.5
		activeball.vely = activeball.vely * (1.2 + rnd(1)/3 )
	end if
end sub


'******************************************************
'* DRAIN **********************************************
'******************************************************

Sub Drain_Hit()
	RandomSoundDrain Drain
	BsTrough.AddBall Me
	Controller.Switch(11) = 1
End Sub


'******************************************************
'* Lockout *********************************************
'******************************************************

Sub SolLockout(Enabled)
	If Enabled Then
		bsTrough.ExitSol_On
		BallRelease.CreateBall
		bsTroughKick.AddBall 0
		RandomSoundBallRelease BallRelease
	End If
End Sub


'******************************************************
'* Paddle Wheel ***************************************
'******************************************************

Sub Solwheel(Enabled)
	If Enabled Then
		PaddleTimer.Enabled = 1
	else
		PaddleTimer.Enabled = 0
	end if
End Sub

Dim RWBall(2), RWKick(2,2), WRot, wx, wy, wz, WDetail, WDetail2, obj
Const WheelAngle = 10, WRadius = 85	'WRadius is distance from center of wheel to center of ball inside wheel
dim redin, redout: redin = 10 : redout = 150

Sub KickerRW_Hit(x)
	SoundSaucerLock
	KickerRW(x).Enabled = 0								'Enable this kicker
	KickerRW((x+4) Mod 3).Enabled = 1					'Enable next kicker
	Set RWBall(x) = ActiveBall							'Defines ball for wheel movement
	RWKick(x,1) = 1										'Marks ball as waiting for wheel
	'KickerUpdate										'Enable next available kicker
	kickerq1.enabled =1
End Sub

Sub PaddleTimer_Timer
 Dim y, bx, by, bz
	RedWheel.RotAndTra0 = RedWheel.RotAndTra0 + .95 : If RedWheel.RotAndTra0 = 360 Then RedWheel.RotAndTra0 = 0
	'For each obj in RedWheel : obj.RotAndTra0 = RedWheel.RotAndTra0 : next
	If WDetail <> WDetail2 Then
		For each obj in RedWheels : obj.TopVisible = 0 : next
		RedWheels(WDetail).TopVisible = 1
		WDetail2 = WDetail
	End If
	
	If RedWheel.RotAndTra0 mod 90 = 20 Then 						'If RedWheel Scoop @ Insert position (10 degrees) Then
		For y = 0 to 2												'Cycle through possible balls
			If RWKick(y,1) = 1 Then									'If Ball is waiting ((y,1)=1) then
				RWKick(y,1) = 2										'mark Ball as inserted ((y,1)=2)
				RWKick(y,2) = 10									'and set initial wheel angle for ball ((y,2)=30)
				Exit For
			End If
		Next
	End If

	For y = 0 to 2
																	'RedWheel Ball Update
		If IsObject(RWBall(y)) And RWKick(y,1) = 2 Then				'If Ball is defined and marked as inserted
			PlaySoundAt "motor",KickerRWc
			If RWKick(y,2) = 200 Then								'If Wheel is at exit angle
				KickerRW(y).Kick 190,5,0							'kick ball at defined angle and strength
				PlaySoundAt "Ball_Bounce_Playfield_Hard_1",KickerRWa
				'Set RWBall(y) = nothing							'clear ball object
				RWKick(y,1) = 0										'mark ball as exited
				kickerq1.kick 180, 0.1
				kickerq1.enabled = False
				'KickerUpdate										'and enable next available kicker
			End If
			RWKick(y,2) = RWKick(y,2) + 1							'increment wheel angle for specific ball
			BallPos RWKick(y,2), bx, by, bz							'calculate relevant position modifiers and apply them
			RWBall(y).x = RedWheel.x + bx
			RWBall(y).y = RedWheel.y + by
			RWBall(y).z = RedWheel.z + bz + (BallSize/2)
		End If
	Next
End Sub

Sub BallPos (wrot, wx, wy, wz)									'Used to determine position modifiers for moving ball in relation to center of wheel 
  Dim y, AWRot : AWRot = WRot + 50	  							'Ball center is 21.5 degrees off wheel rotation angle
	y = dsin(AWRot) * WRadius
	wz = dcos(AWRot) * WRadius * -1
	wx = y * dsin(WheelAngle)
	wy = y * dcos(WheelAngle) * -1
End Sub

'******************************************************
'                  RAMP SOUNDS 
'******************************************************
sub RampSound009_Hit: WireRampOn True : End Sub
sub RampSound008_Hit: WireRampOff : End Sub
Sub RampSound010_Hit: WireRampOn False : End Sub
sub RampSound007_Hit: WireRampOff : End Sub
sub RampSound003_Hit: WireRampOn True : End Sub
sub RampSound004_Hit: WireRampOff : End Sub
sub RampSound001_Hit: WireRampOn False : End Sub
sub RampSound002_Hit: WireRampOff : End Sub
sub RampSound006_Hit: WireRampOn False: End Sub
sub RampSound005_Hit: WireRampOff : End Sub


'******************************************************
'					TRIGGERS
'******************************************************

Sub sw16_Hit:Controller.Switch(16)=1 : End Sub 
Sub sw16_unHit:Controller.Switch(16)=0:End Sub
sub sw30_hit(): vpmtimer.Pulsesw 30:end Sub 'Captive Ball
Sub sw32_Hit:vpmTimer.PulseSw 32:End Sub
Sub sw37_Hit:Controller.Switch(37)=1 : End Sub 
Sub sw37_unHit:Controller.Switch(37)=0:End Sub
Sub sw38_Hit:Controller.Switch(38)=1 : End Sub 
Sub sw38_unHit:Controller.Switch(38)=0:End Sub
Sub sw39_Hit:Controller.Switch(39)=1 : End Sub 
Sub sw39_unHit:Controller.Switch(39)=0:End Sub
Sub sw40_Hit:Controller.Switch(40)=1 : End Sub 
Sub sw40_unHit:Controller.Switch(40)=0:End Sub
Sub sw44_Hit:vpmTimer.PulseSw 44:End Sub
'Sub sw45_hit:Controller.switch(45)=1:End Sub
'Sub sw45_unhit:Controller.switch(45)=0:End Sub
'Sub sw46_hit:Controller.switch(46)=1:End Sub
'Sub sw46_unhit:Controller.switch(46)=0:End Sub
Sub sw47_Hit:Controller.Switch(47)=1 : End Sub 
Sub sw47_unHit:Controller.Switch(47)=0:End Sub
Sub sw49_Hit:Controller.Switch(49)=1 : End Sub 
Sub sw49_unHit:Controller.Switch(49)=0:End Sub
Sub sw50_Hit:vpmTimer.PulseSw 50:End Sub
Sub sw52_Hit:Controller.Switch(52)=1 : End Sub 
Sub sw52_unHit:Controller.Switch(52)=0:End Sub
Sub sw55_Hit:Controller.Switch(55)=1 : End Sub 
Sub sw55_unHit:Controller.Switch(55)=0:End Sub
Sub sw54_Hit:Controller.Switch(54)=1 : End Sub 
Sub sw54_unHit:Controller.Switch(54)=0:End Sub
Sub sw53_Hit:Controller.Switch(53)=1 : End Sub 
Sub sw53_unHit:Controller.Switch(53)=0:End Sub
Sub sw56_Hit:vpmTimer.PulseSw 56:End Sub
Sub sw57_Hit:Controller.Switch(57)=1 : End Sub 
Sub sw57_unHit:Controller.Switch(57)=0:End Sub
Sub sw58_Hit:vpmTimer.PulseSw 58:End Sub
Sub sw60_Hit:Controller.Switch(60)=1 : End Sub 
Sub sw60_unHit:Controller.Switch(60)=0:End Sub
Sub sw61_Hit:Controller.Switch(61)=1 : End Sub 
Sub sw61_unHit:Controller.Switch(61)=0:End Sub
Sub sw62_Hit:Controller.Switch(62)=1 : End Sub 
Sub sw62_unHit:Controller.Switch(62)=0:End Sub

Sub BumperTL_Hit
	vpmTimer.PulseSw 41
	RandomSoundBumperTop BumperTL
End Sub
Sub BumperTR_Hit
	vpmTimer.PulseSw 43
	RandomSoundBumperMiddle BumperTR
End Sub
Sub BumperB_Hit
	vpmTimer.PulseSw 42
	RandomSoundBumperBottom BumperB
End Sub

'******************************************************
'			     	Ball Lock
'******************************************************
Dim BallRecentlyUnlocked
BallRecentlyUnlocked = False

Sub LockRelease(Enabled)
	If Enabled Then
		vLock.SolExit True
		BallRecentlyUnlocked = True
		sw46a.TimerEnabled = True
	Else
		vLock.SolExit False
	End If
End Sub

Sub sw46a_Timer
	BallRecentlyUnlocked = False
	sw46a.TimerEnabled = False
End Sub

'******************************************************
'			     	Vertical Kick
'******************************************************

'Variables used for VUK 
 Dim raiseballsw, raiseball

 Sub TopVUK_Hit() 
 	TopVUK.Enabled=FALSE
	Controller.switch (31) = True
	SoundSaucerLock
 End Sub
 
 Sub VukTopPop(enabled)
	if(enabled and Controller.switch (31)) then
		TopVUK.DestroyBall
 		Set raiseball = TopVUK.CreateBall
        SoundSaucerKick 1, TopVUK
 		raiseballsw = True
 		TopVukraiseballtimer.Enabled = True 'Added by Rascal
		TopVUK.Enabled=TRUE	
 		Controller.switch (31) = False
	else

	end if
End Sub
 
 Sub TopVukraiseballtimer_Timer()
 	If raiseballsw = True then
 		raiseball.z = raiseball.z + 12   '8
		raiseball.x = raiseball.x + 0
 		If raiseball.z > 242 then
			'playsound "vukout"
 			TopVUK.Kick 100, 6
 			Set raiseball = Nothing
 			TopVukraiseballtimer.Enabled = False
 			raiseballsw = False
 		End If
 	End If
 End Sub

'*****************
' Ramp Diverter
'*****************
 Sub SolRdiv(enabled)
 	if enabled then
		'Playsound "solenoid"
		PlaySoundAtLevelStatic ("solenoid"), VolumeDial, gi019
 		diverterWall.isdropped=true
		DiverterTimerON002.enabled = True
 	else
		DiverterTimerOFF002.enabled = True
 		diverterWall.isdropped=false
 	end if
 end sub




Sub DiverterTimerON002_Timer()
		PrimiDiverter2.RotX = PrimiDiverter2.RotX - 1
		If PrimiDiverter2.RotX = -60 then
			DiverterTimerON002.enabled = False
		End If
End Sub

Sub DiverterTimerOFF002_Timer()
		PrimiDiverter2.RotX = PrimiDiverter2.RotX + 1
		If PrimiDiverter2.RotX = 0 then
			DiverterTimerOFF002.enabled = False
		End If
End Sub

'******************************************************
'			     	Ball Deflector
'******************************************************
dim PD001
PD001 = 1

Sub SolBallDeflector(enabled)
	 if enabled then
		'Playsound "solenoid"
		PlaySoundAtLevelStatic ("solenoid"), VolumeDial, gi019
 		Deflector_on.isdropped=true
		Deflector_on2.isdropped=false
		DiverterTimerON001.enabled = True
 	else
 		Deflector_on.isdropped=false
 		Deflector_on2.isdropped=true
		DiverterTimerOFF001.enabled = True
 	end if
 end sub


Sub DiverterTimerON001_Timer()
		PDeflector001.z = PDeflector001.z - 1
		If PDeflector001.z = - 49 then
			DiverterTimerON001.enabled = False
		End If
End Sub

Sub DiverterTimerOFF001_Timer()

If PDeflector001.z <= 10 and PD001 = 1 then
		PDeflector001.z = PDeflector001.z + 1
else
		PD001 = 0
End If
		If PDeflector001.z > 0 and PD001 = 0 then
			PDeflector001.z = PDeflector001.z - 1
				If PDeflector001.z = 0 then
					PD001 = 1
					DiverterTimerOFF001.enabled = False
				End If
		End If
End Sub


'******************************************************
'			     		KickBack
'******************************************************


Sub SolKickback(Enabled)
     If Enabled Then
		KickBack.Fire
        SoundKickbackReleaseBall() 
     Else
        KickBack.Pullback
     End If
End Sub


'******************************************************
'			     	Skill shot
'******************************************************


Sub SolSkillshot(Enabled)
     If Enabled Then
		SkillShot.Fire
        SoundSkillShotReleaseBall() 
     Else
        SkillShot.Pullback
     End If
End Sub


'----------------------------

Sub Table1_KeyDown(ByVal keycode)

	If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress
	If KeyCode = PlungerKey Then Plunger.PullBack:SoundPlungerPull()
	If keycode = LeftTiltKey Then Nudge 90, 2 : SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 2 : SoundNudgeRight()
	If keycode = CenterTiltKey Then Nudge 0, 4 : SoundNudgeCenter()
	If keycode = LeftMagnaSave Then bLutActive = True
	If keycode = RightMagnaSave Then 
		If bLutActive Then NextLUT: End If
	End If

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25

		End Select
	End If

	if keycode=StartGameKey then soundStartButton()


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

	if keycode=StartGameKey then
		if (bsTrough.Balls = 4 or vLock.balls > 0) and not BallRecentlyUnlocked then
			Controller.Switch(swStartButton)  = True
		End if
	else
		If KeyDownHandler(keycode) Then Exit Sub
	end if
End Sub

Sub Table1_KeyUp(ByVal keycode)

	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress
	If KeyCode = PlungerKey Then
		Plunger.Fire
		'If BIPL = 1 Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		'Else
		'	SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		'End If
	End If
	If keycode = LeftMagnaSave Then bLutActive = False


    'Manual Ball Control
	If EnableBallControl = 1 Then
		If keycode = 203 Then BCleft = 0	' Left Arrow
		If keycode = 200 Then BCup = 0		' Up Arrow
		If keycode = 208 Then BCdown = 0	' Down Arrow
		If keycode = 205 Then BCright = 0	' Right Arrow
	End If

	If KeyUpHandler(keycode) Then Exit Sub
End Sub


'******************************************************
'			     	 Drop Targets
'******************************************************

Sub Sw22_Hit : DTHit 22 : End Sub
Sub Sw23_Hit : DTHit 23 : End Sub
Sub Sw24_Hit : DTHit 24 : End Sub

Sub Sw25_Hit : DTHit 25 : End Sub
Sub Sw26_Hit : DTHit 26 : End Sub
Sub Sw27_Hit : DTHit 27 : End Sub
Sub Sw28_Hit : DTHit 28 : End Sub
Sub Sw29_Hit : DTHit 29 : End Sub

Sub Sw17_Hit : DTHit 17 : End Sub
Sub Sw18_Hit : DTHit 18 : End Sub
Sub Sw19_Hit : DTHit 19 : End Sub
Sub Sw20_Hit : DTHit 20 : End Sub
Sub Sw21_Hit : DTHit 21 : End Sub

Sub Sw33_Hit : DTHit 33 : End Sub
Sub Sw34_Hit : DTHit 34 : End Sub
Sub Sw35_Hit : DTHit 35 : End Sub
Sub Sw36_Hit : DTHit 36 : End Sub

Sub SolCDTBank(enabled)
	if enabled then
		PlaySound SoundFX(DTResetSound,DOFDropTargets)
		DTRaise 22
		DTRaise 23
		DTRaise 24
	end if
End Sub

Sub SolLUBank(enabled)
	if enabled then
		PlaySound SoundFX(DTResetSound,DOFDropTargets)
		DTRaise 25
		DTRaise 26
		DTRaise 27
		DTRaise 28
		DTRaise 29
	end if
End Sub

Sub SolLUBankAuto(enabled)
	if enabled then
		DTDrop 25
		DTDrop 26
		DTDrop 27
		DTDrop 28
		DTDrop 29
	end if
End Sub

Sub SolLDTLwrBank(enabled)
	if enabled then
		PlaySound SoundFX(DTResetSound,DOFDropTargets)
		DTRaise 17
		DTRaise 18
		DTRaise 19
		DTRaise 20
		DTRaise 21
	end if
End Sub

Sub SolRDTBank(enabled)
	if enabled then
		PlaySound SoundFX(DTResetSound,DOFDropTargets)
		DTRaise 33
		DTRaise 34
		DTRaise 35
		DTRaise 36
		DTRaise 37
	end if
End Sub



'******************************************************
'		DROP TARGETS INITIALIZATION
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

'Define a variable for each drop target
Dim DT22, DT23, DT24, DT25, DT26, DT27, DT28, DT29, DT17, DT18, DT19, DT20, DT21, DT33, DT34, DT35, DT36

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
'	animate:			Arrary slot for handling the animation instrucitons, set to 0
'
'	Values for annimate: 1 - bend target (hit to primary), 2 - drop target (hit to secondary), 3 - brick target (high velocity hit to secondary), -1 - raise target 
'       isDropped:  Boolean which determines whether a drop target is dropped. Set to false if they are initially raised, true if initially dropped

' Top Bank
Set DT22 = (new DropTarget)(sw22, sw22offset, primsw22, 22, 0, false)
Set DT23 = (new DropTarget)(sw23, sw23offset, primsw23, 23, 0, false)
Set DT24 = (new DropTarget)(sw24, sw24offset, primsw24, 24, 0, false)

Set DT25 = (new DropTarget)(sw25, sw25offset, primsw25, 25, 0, false)
Set DT26 = (new DropTarget)(sw26, sw26offset, primsw26, 26, 0, false)
Set DT27 = (new DropTarget)(sw27, sw27offset, primsw27, 27, 0, false)
Set DT28 = (new DropTarget)(sw28, sw28offset, primsw28, 28, 0, false)
Set DT29 = (new DropTarget)(sw29, sw29offset, primsw29, 29, 0, false)

Set DT17 = (new DropTarget)(sw17, sw17offset, primsw17, 17, 0, false)
Set DT18 = (new DropTarget)(sw18, sw18offset, primsw18, 18, 0, false)
Set DT19 = (new DropTarget)(sw19, sw19offset, primsw19, 19, 0, false)
Set DT20 = (new DropTarget)(sw20, sw20offset, primsw20, 20, 0, false)
Set DT21 = (new DropTarget)(sw21, sw21offset, primsw21, 21, 0, false)

Set DT33 = (new DropTarget)(sw33, sw33offset, primsw33, 33, 0, false)
Set DT34 = (new DropTarget)(sw34, sw34offset, primsw34, 34, 0, false)
Set DT35 = (new DropTarget)(sw35, sw35offset, primsw35, 35, 0, false)
Set DT36 = (new DropTarget)(sw36, sw36offset, primsw36, 36, 0, false)

'Add all the Drop Target Arrays to Drop Target Animation Array
' DTAnimationArray = Array(DT1, DT2, ....)
Dim DTArray
DTArray = Array(DT22, DT23, DT24, DT25, DT26, DT27, DT28, DT29, DT17, DT18, DT19, DT20, DT21, DT33, DT34, DT35, DT36)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 		'in milliseconds
Const DTDropUpSpeed = 40 		'in milliseconds
Const DTDropUnits = 44 			'VP units primitive drops
Const DTDropUpUnits = 10 		'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 			'max degrees primitive rotates when hit
Const DTDropDelay = 20	 		'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40	 		'time in milliseconds before target drops back to normal up position after the solendoid fires to raise the target
Const DTBrickVel = 0			'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0			'Set to 0 to disable bricking, 1 to enable bricking
'Const DTHitSound = "target"	'Drop Target Hit sound
Const DTDropSound = "DropTarget_Down"	'Drop Target Drop sound
Const DTResetSound = "DropTarget_Up"	'Drop Target reset sound

Const DTMass = 0.2				'Mass of the Drop Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)

	PlayTargetSound
	DTArray(i).animate =  DTCheckBrick(Activeball,DTArray(i).prim)
	If DTArray(i).animate = 1 or DTArray(i).animate = 3 Then
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
	dim bangle, bangleafter, rangle, rangle2, Xintersect, Yintersect, cdist, perpvel, perpvelafter
	rangle = (dtprim.rotz - 90) * 3.1416 / 180
	rangle2 = dtprim.rotz * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)

	Xintersect = (aBall.y - dtprim.y - tan(bangle) * aball.x + tan(rangle2) * dtprim.x) / (tan(rangle2) - tan(bangle))
	Yintersect = tan(rangle2) * Xintersect + (dtprim.y - tan(rangle2) * dtprim.x)

	cdist = Distance(dtprim.x, dtprim.y, Xintersect, Yintersect)

	perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
	perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 

	If perpvel <= 0 or perpvelafter >= 0 Then
		DTCheckBrick = 0 
	ElseIf DTEnableBrick = 1 and  perpvel > DTBrickVel and DTBrickVel <> 0 and cdist < 8 Then
		DTCheckBrick = 3
	Else
		DTCheckBrick = 1
	End If
End Function


Sub DoDTAnim()
	Dim i
	For i=0 to Ubound(DTArray)
		DTArray(i).animate = DTAnimate(DTArray(i).primary,DTArray(i).secondary,DTArray(i).prim,DTArray(i).sw,DTArray(i).animate)
	Next
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

	If animate = 1 and animtime < DTDropDelay Then
		primary.collidable = 0
		secondary.collidable = 1
		prim.rotx = DTMaxBend * cos(rangle)
		prim.roty = DTMaxBend * sin(rangle)
		DTAnimate = 1
		Exit Function
	elseif animate = 1 and animtime > DTDropDelay Then
		primary.collidable = 0
		secondary.collidable = 1
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


'**************
' Flasher Subs
'**************

Sub FlashSkill(flstate)
	If Flstate Then
		SetLamp 125,1
		Objlevel(1) = 1 : FlasherFlash1_Timer
		Objlevel(2) = 1 : FlasherFlash2_Timer
	else
		SetLamp 125,0
	End If
End Sub

Sub FlashLLeft(flstate)
	If Flstate Then
		SetLamp 129,1
		Objlevel(3) = 1 : FlasherFlash3_Timer
		Objlevel(4) = 1 : FlasherFlash4_Timer
	else
		SetLamp 129,0
	End If
End Sub

Sub FlashPaddle(flstate)
	If Flstate Then
		SetLamp 128,1
		Objlevel(5) = 1 : FlasherFlash5_Timer
	else
		SetLamp 128,0
	End If
End Sub

Sub JokersFlash(flstate)
	If Flstate Then
		SetLamp 126,1
		Objlevel(6) = 1 : FlasherFlash6_Timer
	else
		SetLamp 126,0
	End If
End Sub



' #####################################
' ###### Flupper1 Flashers #####
' #####################################

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.6		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherOffBrightness = 0.5		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objlight(20)

'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
InitFlasher 1, "red" : InitFlasher 2, "red" : InitFlasher 3, "red" : InitFlasher 4, "red"
InitFlasher 5, "red" : InitFlasher 6, "yellow"

' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 4,17 : RotateFlasher 5,0 : RotateFlasher 6,90

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 60
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness
	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub 
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub 
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub 
Sub FlasherFlash6_Timer() : FlashFlasher(6) : End Sub 
Sub FlasherFlash7_Timer() : FlashFlasher(7) : End Sub
Sub FlasherFlash8_Timer() : FlashFlasher(8) : End Sub
Sub FlasherFlash9_Timer() : FlashFlasher(9) : End Sub
Sub FlasherFlash10_Timer() : FlashFlasher(10) : End Sub
Sub FlasherFlash11_Timer() : FlashFlasher(11) : End Sub

'******************************************************
'			     	 Gi & GI Effects
'******************************************************

dim GIState: GIState = 1
dim fon,foff

GiEffects.Enabled=1
fon=1:GiMaskOn.Enabled=1


'Playfield GI
Sub GIRelay(Enabled)
	dim xx
	If Enabled Then
		PlaySound "relay_off",0,0.2,0,0.02
		For each xx in GI:xx.State = 0: Next
		GIState = 0
		fon=1:GiMaskOn.Enabled=1
		'Primitive085.material = "Metal0.8"
		'Primitive086.material = "Metal0.8"
		Primitive085.image = "Spot-Glow-off"
		Primitive086.image = "Spot-Glow-off"
	Else
		For each xx in GI:xx.State = 1: Next
		PlaySound "relay_on",0,0.2,0,0.02
		GIState = 1
		foff=1:GiMaskOff.Enabled=1
		'Primitive085.material = "Plastic White"
		'Primitive086.material = "Plastic White"
		Primitive085.image = "Spot-Glow"
		Primitive086.image = "Spot-Glow"
	End If
End Sub

Sub GiEffects_timer()
'debug.print gi001.State
		FadeDisableLighting LFde, 0.6, 0, 0.2, 0.2
		FadeDisableLighting RFde, 0.6, 0, 0.2, 0.2
		FadeDisableLighting URFde, 0.6, 0, 0.2, 0.2
		FadeDisableLighting Primcab, 0.2, 0, 0.066, 0.066
		FadeDisableLighting Primitive039, 2, 0.2, 0.6, 0.6 'LaneGuides
		FadeDisableLighting Primitive028, 0.8, 0.2, 0.266, 0.266 'Wire Left
		FadeDisableLighting Primitive027, 0.8, 0.2, 0.266, 0.266 'Wire VUK
		FadeDisableLighting Primitive031, 0.8, 0.2, 0.266, 0.266  'Wire Cent
		FadeDisableLighting Primitive034, 0.8, 0.2, 0.266, 0.266  'Wire Plunger
		FadeDisableLighting LauraBelle, 0.3, 0, 0.1, 0.1 'LauraBelle
		FadeDisableLighting RedWheel, 0.1, 0, 0.033, 0.033 'Wheel
		FadeDisableLighting Primitive013, 0.1, 0, 0.033, 0.033  ' Plastics
		FadeDisableLighting Primitive042, 0.6, 0, 0.2, 0.2  ' Plastics Right
		FadeDisableLighting Primitive040, 2, 0, 0.3, 0.3 'Bulb-L
		FadeDisableLighting Primitive033, 2, 0, 0.3, 0.3 'Bulb-L2
		FadeDisableLighting Primitive041, 2, 0, 0.3, 0.3 'Bulb-R
		FadeDisableLighting Primitive106, 2, 0, 0.3, 0.3 'Bulb-R2
		FadeDisableLighting Primitive107, 2, 0, 0.3, 0.3 'Bulb-R3
		FadeDisableLighting Primitive108, 2, 0, 0.3, 0.3 'Bulb-R4
		FadeDisableLighting Primitive055, 1, 0, 0.3, 0.3 'Bulb-TT
		FadeDisableLighting Primitive056, 1, 0, 0.3, 0.3 'Bulb-TT
		FadeDisableLighting Primitive057, 1, 0, 0.3, 0.3 'Bulb-TT
		FadeDisableLighting Primitive058, 1, 0, 0.3, 0.3 'Bulb-TT
		FadeDisableLighting Primitive059, 1, 0, 0.3, 0.3 'Bulb-TT
		FadeDisableLighting Primitive060, 1, 0, 0.3, 0.3 'Bulb-TT
		FadeDisableLighting plasticRamp, 2, 0.2, 0.6, 0.6  'PRamp
		FadeDisableLighting lanesTop, 1, 0, 0.3, 0.3 'lanes
		FadeDisableLighting Primitive061, 0.25, 0, 0.066, 0.066 'Flasher Mount R
		FadeDisableLighting PrimBubArray, 1, 0, 0.3, 0.3 'Bulb Array
		FadeDisableLighting Primitive075, 0.3, 0, 0.1, 0.1 'Signpost
		FadeDisableLighting Primitive079, 0.2, 0, 0.066, 0.066 'BACK PLASTICS
		FadeDisableLighting Primitive078, 0.2, 0, 0.066, 0.066 'BACK PLASTICS
		FadeDisableLighting Primitive076, 0.6, 0, 0.2, 0.2 'spot wires
		FadeDisableLighting Primitive082, 0.2, 0, 0.066, 0.066  'PlateTR
		FadeDisableLighting PrimWalls, 0.2, 0, 0.066, 0.066  ' Walls
		FadeDisableLighting PrimWalls001, 0.4, 0, 0.133, 0.133  ' Wall fixing
		' Top Spots
		FadeDisableLighting Primitive085, 0.7, 0, 0.233, 0.233
		FadeDisableLighting Primitive086, 0.7, 0, 0.233, 0.233
End Sub

'Fade DisableLighting (object, starting brightness, ending brightness, speed on, speed off)

Sub FadeDisableLighting(a, alvlu, alvld, spdu, spdd)

	Select Case gi001.State

		Case 1: 
			a.UserValue = a.UserValue + spdu
				If a.UserValue > alvlu Then 
					a.UserValue = alvlu
				end If
			a.BlendDisableLighting = a.UserValue 'On

		Case 0:
			a.UserValue = a.UserValue - spdd
				If a.UserValue < alvld Then 
					a.UserValue = alvld
				end If
			a.BlendDisableLighting = a.UserValue 'Off

		End Select

End Sub

Sub GiMaskOn_timer()

	Select Case fon
		Case 1:GiMaskOff.Enabled=0:FlasherGI001.IntensityScale = 1		:FlasherSP001.IntensityScale = 1	:FlasherSP002.IntensityScale = 1	:fon=2
		Case 2:FlasherGI001.IntensityScale = 0.66	:FlasherSP001.IntensityScale = 0.66	:FlasherSP002.IntensityScale = 0.66	:fon=3
		Case 3:FlasherGI001.IntensityScale = 0.33	:FlasherSP001.IntensityScale = 0.33	:FlasherSP002.IntensityScale = 0.33	:fon=4
		Case 4:FlasherGI001.IntensityScale = 0		:FlasherSP001.IntensityScale = 0	:FlasherSP002.IntensityScale = 0	:fon=5
		Case 5:GiMaskOn.Enabled=0
	End Select

End Sub

Sub GiMaskOff_timer()
	
	Select Case foff
		Case 1:GiMaskOn.Enabled=0:FlasherGI001.IntensityScale = 0		:FlasherSP001.IntensityScale = 0	:FlasherSP002.IntensityScale = 0	:foff=2
		Case 2:FlasherGI001.IntensityScale = 0.33	:FlasherSP001.IntensityScale = 0.33	:FlasherSP002.IntensityScale = 0.33	:foff=3
		Case 3:FlasherGI001.IntensityScale = 0.66	:FlasherSP001.IntensityScale = 0.66	:FlasherSP002.IntensityScale = 0.66	:foff=4
		Case 4:FlasherGI001.IntensityScale = 1		:FlasherSP001.IntensityScale = 1	:FlasherSP002.IntensityScale = 1	:foff=5
		Case 5:GiMaskOff.Enabled=0
	End Select

End Sub

'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
'       Plus Anti fading disabled lighting
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampState(200), FadingState(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)
Dim PO199:PO199 = 1

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters

' Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp)Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0)) = chgLamp(ii, 1)       'keep the real state in an array
            FadingState(chgLamp(ii, 0)) = chgLamp(ii, 1) + 3 'fading step
        Next
    End If
    'UpdateLeds
    UpdateLamps
End Sub

Sub UpdateLamps

	Lamp 1, L1
	Lamp 2, L2	
	Lamp 3, L3	
	Lamp 4, L4	
	Lamp 5, L5	
	Lamp 6, L6	
	Lamp 7, L7	
	Lamp 8, L8	
	Flash 9, F9	
	Lamp 10, L10
	Lamp 11, L11

	Lampm 12, L12
	FDL 12,PrimL12, 3 , 0

	Lamp 14, L14
	Lamp 15, L15
	Lamp 16, L16
	Lamp 17, L17
	Lamp 18, L18
	Lamp 19, L19
	Lamp 20, L20
	Lamp 21, L21
	Lamp 22, L22
	Lamp 23, L23
	Lamp 24, L24

' Bumpers
	Lampm 25, L25a
	Lampm 25, L25
	FDL 25, Primitive023, 0.9 , 0

	Lampm 26, L26a
	Lampm 26, L26
	FDL 26, Primitive024, 0.9 , 0

	Lampm 27, L27a
	Lampm 27, L27
	FDL 27, Primitive7, 0.9 , 0

	Lamp 28, L28
	Lamp 29, L29
	Lamp 30, L30
	Lamp 31, L31
	Lamp 32, L32

	FDL 33,PrimL33, 1, 0
	FDL 34,PrimL34, 1, 0
	FDL 35,PrimL35, 1, 0

	Lampm 37, L37b
	Lamp 37, L37a
	Lamp 38, L38
	Lamp 39, L39
	Lamp 40, L40
	Lamp 41, L41
	Lamp 42, L42
	Lamp 43, L43
	Lamp 44, L44
	Lamp 45, L45
	Lampm 46, L46b
	Lamp 46, L46a
	Lamp 47, L47
	Lamp 48, L48
	Lamp 49, L49
	Lamp 50, L50
	Lamp 51, L51

	Lampm 52, L52
	FDL 52,PrimL52, 3 , 0

	Lamp 53, L53
	Lamp 54, L54
	Lampm 55, L55b
	Lamp 55, L55a
	Lamp 56, L56
	Lamp 57, L57
	Lamp 58, L58
	Lamp 59, L59
	Lamp 60, L60
	Lamp 61, L61
	Lamp 62, L62

	Lampm 125, L125a
	Lampm 125, L125
	FDLm 125,Primitive088, 1 , 0
	Flashm 125, f125a
	Flash 125, f125

	Flash 126, F126

	Lampm 127, L127b
	Lampm 127, L127a
	Lampm 127, L127
	Flashm 127, F127b
	Flashm 127, F127a
	Flash 127, F127


	Lampm 128, L128a
	Lampm 128, L128
	Flash 128, F128
	'FDL 128,Primitive078, 1 , 0.2
	'FDL 128,Primitive080, 1 , 0

	Flash 129, f129
'	FDL 129,Primitive012, 1 , 0
	'FadeObj 129, LauraBelle, "LaurenBell_ON3","LaurenBell_ON3","LaurenBellMap", "LaurenBellMap"

	Lampm 130, L130
	Flash 130, f130

	Lampm 131, L131
	Flash 131, f131

	Lampm 132, L132a
	Lampm 132, L132
	Flashm 132, f132a
	Flash 132, f132

End Sub

'*********************************************************************

' Lamp subs

' Normal Lamp & Flasher subs

Sub InitLamps()
    Dim x
    LampTimer.Interval = 30 ' flasher fading speed
    LampTimer.Enabled = 1
    For x = 0 to 200
        LampState(x) = 0
        FadingState(x) = 3 ' used to track the fading state
        FlashLevel(x) = 0
    Next
End Sub

Sub SetLamp(nr, value) ' 0 is off, 1 is on
    FadingState(nr) = abs(value) + 3
End Sub

' Lights: used for VPX standard lights, the fading is handled by VPX itself, they are here to be able to make them work together with the flashers

Sub Lamp(nr, object)
    Select Case FadingState(nr)
        Case 4:object.state = 1:FadingState(nr) = 0
        Case 3:object.state = 0:FadingState(nr) = 0
    End Select
End Sub

Sub Lampm(nr, object) ' used for multiple lights, it doesn't change the fading state
    Select Case FadingState(nr)
        Case 4:object.state = 1
        Case 3:object.state = 0
    End Select
End Sub


' Flashers: 4 is on,3,2,1 fade steps. 0 is off

Sub Flash(nr, object)
    Select Case FadingState(nr)
        Case 4:Object.IntensityScale = 1:FadingState(nr) = 0
        Case 3:Object.IntensityScale = 0.66:FadingState(nr) = 2
        Case 2:Object.IntensityScale = 0.33:FadingState(nr) = 1
        Case 1:Object.IntensityScale = 0:FadingState(nr) = 0
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it doesn't change the fading state
    Select Case FadingState(nr)
        Case 4:Object.IntensityScale = 1
        Case 3:Object.IntensityScale = 0.66
        Case 2:Object.IntensityScale = 0.33
        Case 1:Object.IntensityScale = 0
    End Select
End Sub


'Fade DisableLighting (object, start state, end state)


Sub FDL(nr, object, ss, es)

	Select Case FadingState(nr)

		Case 4:Object.BlendDisableLighting = ss:FadingState(nr) = 0
        Case 3:Object.BlendDisableLighting = ss / 1.4:FadingState(nr) = 2
        Case 2:Object.BlendDisableLighting = ss / 2.5:FadingState(nr) = 1
        Case 1:Object.BlendDisableLighting = es:FadingState(nr) = 0

	End Select

End Sub


Sub FDLm(nr, object, ss, es)

	Select Case FadingState(nr)

		Case 4:Object.BlendDisableLighting = ss
        Case 3:Object.BlendDisableLighting = ss / 1.4
        Case 2:Object.BlendDisableLighting = ss / 2.5
        Case 1:Object.BlendDisableLighting = es

	End Select

End Sub


'Fade DisableLighting two state (object, start state, end state)

Sub NFDLm(nr, object, starts, ends)

	Select Case FadingState(nr)

		Case 4:Object.BlendDisableLighting = starts
        Case 3:Object.BlendDisableLighting = ends


	End Select

End Sub


' Desktop Objects: Reels & texts (you may also use lights on the desktop)

' Reels

Sub Reel(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1:FadingState(nr) = 0
        Case 3:object.SetValue 2:FadingState(nr) = 2
        Case 2:object.SetValue 3:FadingState(nr) = 1
        Case 1:object.SetValue 0:FadingState(nr) = 0
    End Select
End Sub

Sub Reelm(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1
        Case 3:object.SetValue 2
        Case 2:object.SetValue 3
        Case 1:object.SetValue 0
    End Select
End Sub

Sub NFadeReel(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1:FadingState(nr) = 1
        Case 3:object.SetValue 0:FadingState(nr) = 0
    End Select
End Sub

Sub NFadeReelm(nr, object)
    Select Case FadingState(nr)
        Case 4:object.SetValue 1
        Case 3:object.SetValue 0
    End Select
End Sub

'Texts

Sub Text(nr, object, message)
    Select Case FadingState(nr)
        Case 4:object.Text = message:FadingState(nr) = 0
        Case 3:object.Text = "":FadingState(nr) = 0
    End Select
End Sub

Sub Textm(nr, object, message)
    Select Case FadingState(nr)
        Case 4:object.Text = message
        Case 3:object.Text = ""
    End Select
End Sub

' Modulated Subs for the WPC tables

Sub SetModLamp(nr, level)
    FlashLevel(nr) = level / 150 'lights & flashers
End Sub

Sub LampMod(nr, object)          ' modulated lights used as flashers
    Object.IntensityScale = FlashLevel(nr)
    Object.State = 1             'in case it was off
End Sub

Sub FlashMod(nr, object)         'sets the flashlevel from the SolModCallback
    Object.IntensityScale = FlashLevel(nr)
End Sub

'Walls and mostly Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingState(nr)
        Case 4:object.image = a:FadingState(nr) = 0 'fading to off...
        Case 3:object.image = b:FadingState(nr) = 2
        Case 2:object.image = c:FadingState(nr) = 1
        Case 1:object.image = d:FadingState(nr) = 0
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingState(nr)
        Case 4:object.image = a
        Case 3:object.image = b
        Case 2:object.image = c
        Case 1:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingState(nr)
        Case 4:object.image = a:FadingState(nr) = 0 'off
        Case 3:object.image = b:FadingState(nr) = 0 'on
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingState(nr)
        Case 4:object.image = a
        Case 3:object.image = b
    End Select
End Sub



'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 51
	RandomSoundSlingshotRight Sling1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
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
	vpmTimer.PulseSw 59
	RandomSoundSlingshotLeft Sling2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub


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

Sub BallControlTimer()
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




'********************************************************************
'      Rolling Sounds & Ballshadow
'********************************************************************

Const tnob = 5 ' total number of balls
Const lob = 0   'number of locked balls
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

Sub RollingTimer()
    Dim BOT, b, speedfactorx, speedfactory
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
                        PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)) + 20000, 1, 0, AudioFade(BOT(b))

                Else                     
                    rolling(b) = True
					PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * 10 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)) + 185000, 1, 0, AudioFade(BOT(b))                        
                End If

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



'*****************************************************
' RtxBS - Ray Tracing Ball Shadows by Iakki and Wylte
'*****************************************************


Dim fovY:		fovY		= 10	'Offset y axis to account for layback or inclination
Dim SizeOfBall:	SizeOfBall	= 50	'Regular ballsize isn't working right, as it's pulled from vpm?
Const RTXFactor 			= 0.9	'0 to 1, higher is darker

Dim sourcenames, currentShadowCount

sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)

Sub BallShadowUpdate_Timer()
	If RtxBSon=1 Then
		RtxBSUpdate
	Else
		me.enabled=false
	End If
End Sub

dim objrtx1(20), objrtx2(20), RtxBScnt
dim objBallShadow(20)
RtxInit

sub RtxInit()
	Dim iii
	For Each iii in RtxBS
		RtxBScnt = RtxBScnt + 1		'count lights
'		iii.state = 1 				'lights on
	next

	for iii = 0 to tnob
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = iii/1000 + 0.10
		objrtx1(iii).visible = 0
		'objrtx1(iii).uservalue=0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = (iii)/1000 + 0.12
		objrtx2(iii).visible = 0
		'objrtx2(iii).uservalue=0
		currentShadowCount(iii) = 0
		Set objBallShadow(iii) = Eval("BallShadow" & iii)
	Next
	'msgbox RtxBScnt
end sub


Sub RtxBSUpdate
	Dim falloff:        falloff 		= 120			'Max distance to light source for RTX BS calcs
	Dim ShadowOpacity, ShadowOpacity2 
	Dim s, Source, LSd, CntDwn, b, currentMat, AnotherSource, BOT
	BOT = GetBalls

	' hide shadow of deleted balls
	For s = UBound(BOT) + 1 to tnob
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
	Next

	If UBound(BOT) = lob - 1 Then Exit Sub		'No balls in play exit, same as JP's

	For s = lob to UBound(BOT)
		
		'Normal ambient shadow
		If BOT(s).X < tablewidth/2 Then
			objBallShadow(s).X = ((BOT(s).X) - (SizeOfBall/6) + ((BOT(s).X - (tablewidth/2))/10)) + 6
		Else
			objBallShadow(s).X = ((BOT(s).X) + (SizeOfBall/6) + ((BOT(s).X - (tablewidth/2))/10)) - 6
		End If
		objBallShadow(s).Y = BOT(s).Y + fovY
		objBallShadow(s).Z = s/1000 + 0.04 'make ball shadows to be on different z-order

		If BOT(s).Z < 30 Then 'or BOT(s).Z > 105 Then
			objBallShadow(s).visible = 1
		Else
			'other rules here, like ramps and upper pf
			objBallShadow(s).visible = 0
		end if

		'RTX shadows
		For Each Source in RtxBS							'Rename this to match your collection name
			'LSd=((BOT(s).x-Source.x)^2+(BOT(s).y-Source.y)^2)^0.5			'Calculating the Linear distance to the Source
			LSd=DistanceFast((BOT(s).x-Source.x),(BOT(s).y-Source.y))			'Calculating the Linear distance to the Source
			If BOT(s).Z < 30 Then 'Or BOT(s).Z > 105 Then
				If LSd < falloff and Source.state=1 Then	    'If the ball is within the falloff range of a light and light is on
					CntDwn = RtxBScnt
												 
					currentShadowCount(s) = currentShadowCount(s) + 1
								  

					if currentShadowCount(s) = 1 Then
						sourcenames(s) = source.name
						currentMat = objrtx1(s).material
						objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
						objrtx1(s).Z = s/1000 + 0.01
						objrtx1(s).rotz = AnglePP(Source.x, Source.y, BOT(s).X, BOT(s).Y) + 90
						ShadowOpacity = (falloff-LSd)/falloff
						objrtx1(s).size_y = 25*ShadowOpacity
																							  
						UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*RTXFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0
						'debug.print "update1" & source.name & " at:" & ShadowOpacity
					Elseif currentShadowCount(s) = 2 Then
						currentMat = objrtx1(s).material
						set AnotherSource = Eval(sourcenames(s))
						objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
						objrtx1(s).Z = s/1000 + 0.01
						objrtx1(s).rotz = AnglePP(AnotherSource.x, AnotherSource.y, BOT(s).X, BOT(s).Y) + 90
						ShadowOpacity = (falloff-(((BOT(s).x-AnotherSource.x)^2+(BOT(s).y-AnotherSource.y)^2)^0.5))/falloff
						objrtx1(s).size_y = 25*ShadowOpacity
																							  
						UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*RTXFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

						currentMat = objrtx2(s).material
						objrtx2(s).visible = 1 : objrtx2(s).X = BOT(s).X : objrtx2(s).Y = BOT(s).Y + fovY
						objrtx2(s).Z = s/1000 + 0.02
						objrtx2(s).rotz = AnglePP(Source.x, Source.y, BOT(s).X, BOT(s).Y) + 90
						ShadowOpacity2 = (falloff-LSd)/falloff
						objrtx2(s).size_y = 25*ShadowOpacity2
																								
						UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity2*RTXFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
						'debug.print "update2: " & source.name & " at:" & ShadowOpacity & " and "  & Eval(sourcenames(s)).name & " at:" & ShadowOpacity2
					end if
				Else
					CntDwn = CntDwn - 1				'If ball is not in range of any light, this will hit 0
					currentShadowCount(s) = 0
				End If
			Else
			'other rules here, like ramps and upper pf
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If

		Next
		If CntDwn <= 0 Then
			if CntDwn = -(RtxBScnt) Then
				For b = lob to UBound(BOT)
					objrtx1(b).visible = 0
					objrtx2(b).visible = 0
				Next
			end if

		End If
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




'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	FlipperURSh.RotZ = URightFlipper.currentangle
'Rotate Flipper prims
    LFde.RotY = LeftFlipper.CurrentAngle
    RFde.RotY = RightFlipper.CurrentAngle
	URFde.RotY = URightFlipper.CurrentAngle
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
'--------------------------------------------------------------------------------------------------------------------


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
RollingSoundFactor = 4          

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
WallImpactSoundFactor = 0.075/7                                                                                       'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/7
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers, Ramps and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel, WireRampSoundFactor, LoopSoundFactor

GateSoundLevel = 0.01                                                                                                       'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10                                                                                        'volume multiplier; must not be zero
DTSoundLevel = 0.25                                                                                                                'volume multiplier; must not be zero
RolloverSoundLevel = 0.25   
WireRampSoundFactor = 2 * 100
LoopSoundFactor = 1 * 20     


'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8                                                                                                                'volume level; range [0, 1]
BallReleaseSoundLevel = 1                                                                                                'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2                                                                        'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015                                                                                'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5                                                                                                        'volume multiplier; must not be zero



' *********************************************************************
'                     Fleep  Supporting Ball & Sound Functions
' *********************************************************************



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

Sub SoundKickbackReleaseBall()
        PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Kickback        
End Sub

Sub SoundSkillShotReleaseBall()
        PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, SkillShot        
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
		TargetBouncer Activeball, 1	
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


'/////////////////////////////////////////////////////////////////
'                                        End Mechanical Sounds
'/////////////////////////////////////////////////////////////////


' Ramps sounds
'Sub RampSound001_Hit:PlaySoundAtVolLoops("WireRamp"),RampSound001, Vol(ActiveBall),-1: End Sub
'Sub RampSound003_Hit:PlaySoundAtVolLoops("WireRamp1"),RampSound001, Vol(ActiveBall),0: End Sub

' Stop Ramps Sounds
'Sub RampSound002_Hit: StopSound "WireRamp": End Sub
'Sub RampSound004_Hit: StopSound "WireRamp1": End Sub


'** Extra math  **
'Dim Pi : Pi = CSng(4*Atn(1))

'Pi added to functions because it seems they are initialized before variables are set, which is why CONST worked as it was set before functions
Function dCos(degrees)
  Dim Pi:Pi = CSng(4*Atn(1))
	dcos = cos(degrees * Pi/180)
	if ABS(dCos) < 0.000001 Then dCos = 0
	if ABS(dCos) > 0.999999 Then dCos = 1 * sgn(dCos)
End Function

Function dSin(degrees)
  Dim Pi:Pi = CSng(4*Atn(1))
	dsin = sin(degrees * Pi/180)
	if ABS(dSin) < 0.000001 Then dSin = 0
	if ABS(dSin) > 0.999999 Then dSin = 1 * sgn(dSin)
End Function

Function dAtn(x)
  Dim Pi:Pi = CSng(4*Atn(1))
	datn = atn(x) * 180 / Pi
End Function

Function dAtn2(X, Y)
	If X > 0 Then
		dAtn2 = dAtn(Y / X)
	ElseIf X < 0 Then 
		dAtn2 = dAtn(Y / X) + 180 * Sgn(Y)
		If Y = 0 Then dAtn2 = dAtn2 + 180
		If Y < 0 Then dAtn2 = dAtn2 + 360
	Else
		dAtn2 = 90 * Sgn(Y)
	End If
	dAtn2 = dAtn2+90
End Function
'** End Extra math **


'******************************************************
'		FLIPPER CORRECTION INITIALIZATION
'******************************************************


dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
        dim x, a : a = Array(LF, RF)
        for each x in a
                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
                x.enabled = True
                x.TimeDelay = 60
        Next

        AddPt "Polarity", 0, 0, 0
        AddPt "Polarity", 1, 0.05, -5.5
        AddPt "Polarity", 2, 0.4, -5.5
        AddPt "Polarity", 3, 0.6, -5.0
        AddPt "Polarity", 4, 0.65, -4.5
        AddPt "Polarity", 5, 0.7, -4.0
        AddPt "Polarity", 6, 0.75, -3.5
        AddPt "Polarity", 7, 0.8, -3.0
        AddPt "Polarity", 8, 0.85, -2.5
        AddPt "Polarity", 9, 0.9,-2.0
        AddPt "Polarity", 10, 0.95, -1.5
        AddPt "Polarity", 11, 1, -1.0
        AddPt "Polarity", 12, 1.05, -0.5
        AddPt "Polarity", 13, 1.1, 0
        AddPt "Polarity", 14, 1.3, 0

        addpt "Velocity", 0, 0,         1
        addpt "Velocity", 1, 0.16, 1.06
        addpt "Velocity", 2, 0.41,         1.05
        addpt "Velocity", 3, 0.53,         1'0.982
        addpt "Velocity", 4, 0.702, 0.968
        addpt "Velocity", 5, 0.95,  0.968
        addpt "Velocity", 6, 1.03,         0.945

        LF.Object = LeftFlipper        
        LF.EndPoint = EndPointLp
        RF.Object = RightFlipper
        RF.EndPoint = EndPointRp
End Sub

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub


'******************************************************
'                        FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
        dim a : a = Array(LF, RF)
        dim x : for each x in a
                x.addpoint aStr, idx, aX, aY
        Next
End Sub

Class FlipperPolarity
        Public DebugOn, Enabled
        Private FlipAt        'Timer variable (IE 'flip at 723,530ms...)
        Public TimeDelay        'delay before trigger turns off and polarity is disabled TODO set time!
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

        Public Sub Report(aChooseArray)         'debug, reports all coords in tbPL.text
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
        Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        'Timer shutoff for polaritycorrect
        
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
                                        if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)                                'find safety coefficient 'ycoef' data
                                end if
                        Next

                        If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
                                BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
                                if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
                        End If

                        'Velocity correction
                        if not IsEmpty(VelocityIn(0) ) then
                                Dim VelCoef
         :                         VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

                                if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

                                if Enabled then aBall.Velx = aBall.Velx*VelCoef
                                if Enabled then aBall.Vely = aBall.Vely*VelCoef
                        End If

                        'Polarity Correction (optional now)
                        if not IsEmpty(PolarityIn(0) ) then
                                If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
                                dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
        
                                if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
                                'playsound "fx_knocker"
                        End If
                End If
                RemoveBall aBall
        End Sub
End Class



'******************************************************
'                        FLIPPER TRICKS
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
        Dim BOT, b

        If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
                EOSNudge1 = 1
                'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
                If Flipper2.currentangle = EndAngle2 Then 
                        BOT = GetBalls
                        For b = 0 to Ubound(BOT)
                                If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
                                        'Debug.Print "ball in flip1. exit"
                                         exit Sub
                                end If
                        Next
                        For b = 0 to Ubound(BOT)
                                If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
                                        BOT(b).velx = BOT(b).velx / 1.7
                                        BOT(b).vely = BOT(b).vely - 1
                                end If
                        Next
                End If
        Else 
                If Flipper1.currentangle <> EndAngle1 then 
                        EOSNudge1 = 0
                end if
        End If
End Sub

'*****************
' Maths
'*****************
Const PI = 3.1415927

Function dSin(degrees)
        dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
        dcos = cos(degrees * Pi/180)
End Function

'*************************************************
' Check ball distance from Flipper for Rem
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

' ' Used for drop targets and stand up targets
' Function Atn2(dy, dx)
        ' dim pi
        ' pi = 4*Atn(1)

        ' If dx > 0 Then
                ' Atn2 = Atn(dy / dx)
        ' ElseIf dx < 0 Then
                ' If dy = 0 Then 
                        ' Atn2 = pi
                ' Else
                        ' Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
                ' end if
        ' ElseIf dx = 0 Then
                ' if dy = 0 Then
                        ' Atn2 = 0
                ' else
                        ' Atn2 = Sgn(dy) * pi / 2
                ' end if
        ' End If
' End Function

'*************************************************
' End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 0.8 
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode 
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 6
	Case 2:
		SOSRampup = 8.5
End Select
Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.025

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
        End If
End Sub

'******************************************************
'		FLIPPER POLARITY, DAMPENER, AND DROP TARGET
'				SUPPORTING FUNCTIONS 
'******************************************************

' Used for flipper correction and rubber dampeners
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
Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
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
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
End Function

' Used for drop targets
Function Atn2(dy, dx)
	dim pi
	pi = 4*Atn(1)

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

' Used for drop targets
Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

'****************************************************************************
'PHYSICS DAMPENERS

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
	TargetBouncer Activeball, 0.7
End Sub


dim RubbersD : Set RubbersD = new Dampener	'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 0.96	'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener	'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	'debug, reports in debugger (in vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

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
		If cor.ballvel(aBall.id) <> 0 then
			RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
			coef = desiredcor / realcor 
		Else
			RealCOR = 0
			coef = 0 
		End If
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		if debugOn then TBPout.text = str
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

'******************************************************
'		TRACK ALL BALL VELOCITIES
' 		FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
	public ballvel, ballvelx, ballvely

	Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : End Sub 

	Public Sub Update()	'tracks in-ball-velocity
		dim str, b, AllBalls, highestID : allBalls = getballs

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

Sub RDampen()
	Cor.Update
End Sub


'******************************************************
'iaakki - TargetBouncer for targets and posts
'******************************************************
Dim zMultiplier

sub TargetBouncer(aBall,defvalue)
	if TargetBouncerEnabled <> 0 and aball.x < 30 then
		'debug.print "velz: " & activeball.velz
		Select Case Int(Rnd * 4) + 1
			Case 1: zMultiplier = defvalue+1.1
			Case 2: zMultiplier = defvalue+1.05
			Case 3: zMultiplier = defvalue+0.7
			Case 4: zMultiplier = defvalue+0.3
		End Select
		aBall.velz = aBall.velz * zMultiplier * TargetBouncerFactor
		'debug.print "----> velz: " & activeball.velz
	end if
end sub


'*********************
'Cabinet Mode
'*********************

If CabinetMode = 1 Then
	PinCab_Rails.visible = 0
'	PinCab_Blades.Size_y = 2000
Elseif CabinetMode = 2 Then
	PinCab_Rails.visible = 0
'	PinCab_Blades.visible = 0
Else
'	PinCab_Blades.Size_y = 1000
End If

'*********************
'VR Mode
'*********************
DIM VRThings
If VRRoom > 0 Then
	scoretext.visible = 0
	DMD.visible = 1
	for each VRThings in VR_Ramps:VRThings.BackfacesEnabled =0:Next
	If VRRoom = 1 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
	End If
	If VRRoom = 2 Then
		for each VRThings in VR_Cab:VRThings.visible = 0:Next
		PinCab_Backbox.visible = 1
		PinCab_Backglass.visible = 1
	End If
Else
	for each VRThings in VR_Cab:VRThings.visible = 0:Next
	if DesktopMode then
		scoretext.visible = 1
		PinCab_Rails.visible = 1
	else
		scoretext.visible = 0
		PinCab_Rails.visible = 0
	End if
End If

'*********************
'* RampRoll Sound Loop
'*********************
'=====================================
'		Ramp Rolling SFX updates nf
'=====================================
'Ball tracking ramp SFX 1.0
'	Usage:
'- Setup hit events with WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'- To stop tracking ball, use WireRampoff
'--	Otherwise, the ball will auto remove if it's below 30 vp units

'Example, from Space Station:
'Sub RampSoundPlunge1_hit() : WireRampOn  False : End Sub						'Enter metal habitrail
'Sub RampSoundPlunge2_hit() : WireRampOff : WireRampOn True : End Sub			'Exit Habitrail, enter onto Mini PF 
'Sub RampEntry_Hit() : If activeball.vely < -10 then WireRampOn True : End Sub 	'Ramp enterance
dim RampMinLoops : RampMinLoops = 4
dim RampBalls(6,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

dim RampType(6)	'Slapped together support for multiple ramp types... False = Wire Ramp, True = Plastic Ramp

Sub WireRampOn(input)  : Waddball ActiveBall, input : RampRollUpdate: End Sub
Sub WireRampOff() : WRemoveBall ActiveBall.ID	: End Sub

Sub Waddball(input, RampInput)	'Add ball
    'Debug.Print "In Waddball() + add ball to loop array"	
	dim x : for x = 1 to uBound(RampBalls)	'Check, don't add balls twice
		if RampBalls(x, 1) = input.id then 
			if Not IsEmpty(RampBalls(x,1) ) then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next
	
	For x = 1 to uBound(RampBalls)
		if IsEmpty(RampBalls(x, 1)) then 
			Set RampBalls(x, 0) = input
			RampBalls(x, 1)	= input.ID
			RampType(x) = RampInput
			RampBalls(x, 2)	= 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			exit Sub
		End If
		if x = uBound(RampBalls) then 	'debug
			Debug.print "WireRampOn error, ball queue is full: " & vbnewline & _
						 RampBalls(0, 0) & vbnewline & _
						 Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbnewline & _
						 Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbnewline & _
						 Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbnewline & _
						 Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbnewline & _
						 Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbnewline & _
						 " "
		End If
	next
End Sub

Sub WRemoveBall(ID)		'Remove ball
    'Debug.Print "In WRemoveBall() + Remove ball from loop array"
	dim ballcount : ballcount = 0
	dim x : for x = 1 to Ubound(RampBalls)
		if ID = RampBalls(x, 1) then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		if not IsEmpty(Rampballs(x,1)) then ballcount = ballcount + 1
	next
	if BallCount = 0 then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer():RampRollUpdate:End Sub

Sub RampRollUpdate()		'Timer update
	dim x : for x = 1 to uBound(RampBalls)
		if Not IsEmpty(RampBalls(x,1) ) then 
			if BallVel(RampBalls(x,0) ) > 1 then ' if ball is moving, play rolling sound
				If RampType(x) then 
					PlaySound("RampLoop" & x), -1, Vol(RampBalls(x,0) )*10, AudioPan(RampBalls(x,0) )*3, 0, BallPitchV(RampBalls(x,0) ), 1, 0,AudioFade(RampBalls(x,0) )'*3
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), -1, Vol(RampBalls(x,0) )*10, AudioPan(RampBalls(x,0) )*3, 0, BallPitch(RampBalls(x,0) ), 1, 0,AudioFade(RampBalls(x,0) )'*3
				End If
				RampBalls(x, 2)	= RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			end if
			if RampBalls(x,0).Z < 30 and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end if
	next
	if not RampBalls(0,0) then RampRoll.enabled = 0

End Sub


Sub tbWR_Timer()	'debug textbox
	me.text =	"on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbnewline & _
				 "1 " & Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbnewline & _
				 "2 " & Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbnewline & _
				 "3 " & Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbnewline & _
				 "4 " & Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbnewline & _
				 "5 " & Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbnewline & _
				 "6 " & Typename(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbnewline & _
				 " "
End Sub

Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
    BallPitch = pSlope(BallVel(ball), 1, -1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
    BallPitchV = pSlope(BallVel(ball), 1, -4000, 60, 7000)
End Function
