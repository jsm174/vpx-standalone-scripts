'VPX release by 32assassin:
'	Big thank you to:
'	Francisco 666 for his FP version,  the plastic/ramp primitives where  beyond my skill level.
'	CaptinNeo for his plastic scans    http://www.vpforums....s&showfile=6831
'	Plumb: PF scan    http://www.vpforums....s&showfile=6872
'	Hmueck for his VP9 version,  I took his robot primitives and code animation.
'	zany - primitives

'Wylte Table Tune-Up:  Alignment of every part, nFozzy physics, Roth Targets, Fleep ToM sounds, Dynamic Ball Shadows, Sling Corrections, TargetBouncer, LUT changer, RampRolling sounds, physical trough, Lampz

'	Thanks to nFozzy, Fleep, Rothbauerw, Iaakki, Wrd1972, Apophis, Fluffhead35, and Flupper for the code, models, and instructions to implement the above!
'	Thanks to Nestorgian for the trough code fix, it got all the ball handling code headed in the right direction.
'	Thanks to the testers BountyBob, Smaug, and PinStratsDan who are actually good at pinball and helped make this better.
'	And thanks to 32Assassin for the prior work!
'
'This table is available FOR FREE and should NOT have come to you from anywhere other than VPUniverse.com or PinballNirvana.com

Option Explicit
Randomize

'*********************************************
'**********		User Options		**********
'*********************************************
'----- Shadow Options -----
Const DynamicBallShadowsOn	= 1	'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn	= 1	'0 = Static shadow under ball ("flasher" image, like JP's)
								'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
								'2 = flasher image shadow, but it moves like ninuzzu's
'----- General Sound Options -----
Const VolumeDial		= 0.9	'Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Const BallRollVolume	= 0.5 	'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume	= 0.6 	'Level of ramp rolling volume. Value between 0 and 1

'----- Mods -----
Const GIShutDown	= True		'GI turns off between games or on Tilt
Const ExtraGI		= True		'Adds more GI bulbs under plastics
Const FlipperLights = False		'Simulates bulbs in the shaft of the flippers

DisableLUTSelector = 0  		'Disables the ability to change LUT option with magna saves in game when set to 1

'*** You can change LUT option within game with left and right MagnaSaves (CTRL keys)

'LUTset Types:
'0 = "VPW original 1on1"				]
'1 = "bassgeige"						] Better Pinks
'2 = "Skitso Natural and Balanced"		]
'3 = "Skitso Natural High Contrast"		]
'4 = "HauntFreaks Desaturated"			]
'5 = "Tomate washed out"
'6 = "B&W"
'7 = "Fleep Natural Dark 1"						]
'8 = "Fleep Natural Dark 2"						] Better Blues, Purple pinks
'9 = "CalleV Punchy Brightness and Contrast"	]
'10= "3rdaxis Referenced THX Standard"			]
'11= "blacklight"


Dim LUTset, DisableLUTSelector, LutToggleSound
LutToggleSound = True
LoadLUT
'LUTset = 4	'override saved LUT for debug
SetLUT

'*********************************************
'**********		End User Options	**********
'*********************************************

'********	Robot ROM options adjustment	********
'		adapted from user ta2686 on VPF

'1. Press F6 to display dip switch menu

'2. Check Dip Switch 4

'3. Click OK. You should hear a sound and see "TILT" and the backglass flashing, signifying you are in adjustment mode

'4. 7 & 8 are the "Advance-Return" buttons - 7(Cancel key) increases the Test Number, shown in "Match / Balls to Play" on backglass, 8(Down) decreases it

'5. When you press 7 to enter the test menu, the sound and flashing will stop

'Examples:
'	Setting Number of Balls:
'		a) Press 7 repeatedly until you see 20 in the match/ball in play display
'		b) Press 1(Start Game key) to change the number in the "Credit" display to select the number of balls (01 to 07)

'	Setting Red Special (defeating Robot) difficulty:
'		a) Press 7 repeatedly until you see 20 in the match/ball in play display
'		b) Press 1(Start Game key) to change the number in the "Credit" display
'		c) 00 (Normal/Max difficulty) requires hitting all robots 4 times
'		d) 01 (Medium) requires 3 rounds
'		e) 02 & 03 (Easy) require 2 rounds

'6. Press 7 to cycle through adjustment 37 and exit adjustments - the sound and flashing should resume and "Game Over" should lite solid to indicate the settings took

'7. Press F6 to display dip switch menu

'8. Uncheck Dip Switch 4

'9. Click OK

'**** My (Wylte) NVRAM settings: ***

'DIPs:		0000 for USA region (dips 1-3 just change region, 4 puts game into programming mode)
'Credits:	All coins give 1 credit, Max 15 (supposedly) (Settings 11-16 = 1, 19 = 15)
'Replay:	5m (22=5.0)
'HSTD:		7.5m, awards 1 replay (17=7.5, 18=0, 25=1)
'Balls:		3 balls per game, 3 Extra balls max, Game Time Bonus feature On (20=3, 32=1, 31=1)
'Match:		On (21=01)
'Red Spcl:	4 rounds, awards bonus ball (33=00, 27=01)
'Orange:	3 banks, awards bonus ball (34=00, 28=01)
'Outlanes:	Both ball returns lite (35=00) (If there were a setting for one then both I'd use that, but it's not an option)

'Other DIP regions (changes credit values): Italia 1110 & 0110, GB 1010, France 0010, Deutschland 1100, Belgie 0101, Jugoslavija 1001

'********	End Options Tutorial	********







'*******************************************
'  Constants and Global Variables
'*******************************************
Const BallSize = 50					'Ball size must be 50
Const BallMass = 1					'Ball mass must be 1
Const tnob = 1						'Total number of balls
Const lob = 0						'Locked balls
Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height
Dim DesktopMode: DesktopMode = Table1.ShowDT
Dim BIPL : BIPL = False				'Ball in plunger lane

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM "01200300","zac2.vbs",3.36

Const cGameName="robot"
Const UseSolenoids=1
Const UseLamps=0
Const UseGI=0
Const UseSync = 0
Const HandleMech = 0
Const SSolenoidOn="SolOn"
Const SSolenoidOff="SolOff"
Const SCoin=""

'*******************************************
'  Table Initialization and Exiting
'*******************************************

Dim rBall, gBOT
Dim bsTrough, dtR, SubSpeed
Dim gilvl
Dim BumperDefault, SlingDefault
 
Sub Table1_Init
	Dim i
	vpmInit Me
	On Error Resume Next
	With Controller
		.GameName = cGameName
		.SplashInfoLine = "Robot (Zaccaria 1985)" & vbNewLine & "VPW"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
		.hidden = 1
	End With
	On Error Resume Next
	Controller.Run
	If Err Then MsgBox Err.Description
	On Error Goto 0

	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled=1

	vpmNudge.TiltSwitch = swTilt
	vpmNudge.Sensitivity = 6
	vpmNudge.TiltObj = Array(Bumper1,Bumper2,Bumper3,LeftSlingShot,RightSlingShot,Right1SlingShot)

	BumperDefault=Bumper1.Threshold
	SlingDefault=LeftSlingShot.SlingshotThreshold

	Set rBall = sw16.CreateSizedballWithMass(Ballsize/2,Ballmass)
	gBOT = Array(rBall)
	Controller.Switch(16) = 1

	Dim xx
	If DesktopMode = True Then 'Show Desktop components
		for each xx in BGlights : xx.state = 1 : next
		Ramp16.visible=1
		Ramp15.visible=1
	Else
		for each xx in BGlights : xx.state = 0 : next
		Ramp16.visible=0
		Ramp15.visible=0
	End if

	If Not GIShutDown Then
		gilvl = 1
		Lampz.state(100) = 1
		If ExtraGI Then
			For Each xx in GIxtra
				xx.state = 1
			Next
		End If
		If FlipperLights Then
			FlipBulbL.state = 1 : FlipBulbR.state = 1
			FlipperT10.BlendDisableLighting = 0.7 : FlipperT12.BlendDisableLighting = 0.7
		Else
			FlipperT10.BlendDisableLighting = 0.5 : FlipperT12.BlendDisableLighting = 0.5
		End If
		PlayfieldOn.visible= 1
		Sidewall_On.visible = 1 : Sidewall_Off.visible = 0
		Primitive6.BlendDisableLighting=0.1:Primitive11.BlendDisableLighting=0.1:Primitive15.BlendDisableLighting=0.1:Primitive22.BlendDisableLighting=0.1:Primitive23.BlendDisableLighting=0.1
		sw43p.BlendDisableLighting=0.01:sw44p.BlendDisableLighting=0.01:sw45p.BlendDisableLighting=0.01:sw46p.BlendDisableLighting=0.01:sw47p.BlendDisableLighting=0.01
		psw33.BlendDisableLighting = 0.5 : psw34.BlendDisableLighting = 0.5
	End If
		
	' Turn on the bumper lights
	FlBumperFadeTarget(1) = 1
	FlBumperFadeTarget(2) = 1
	FlBumperFadeTarget(3) = 1

End Sub

Sub Table1_Paused:Controller.Pause = 1:StopSound("BallRoll_" & 0):rolling(0)=False:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub
Sub Table1_exit()
	SaveLUT
	Bumper1.Threshold=BumperDefault
	Bumper2.Threshold=BumperDefault
	Bumper3.Threshold=BumperDefault
	Right1SlingShot.SlingshotThreshold=SlingDefault
	RightSlingShot.SlingshotThreshold=SlingDefault
	LeftSlingShot.SlingshotThreshold=SlingDefault
	Controller.Stop
End Sub

'*******************************************
'  Timers
'*******************************************

' The game timer interval is 10 ms
Sub GameTimer_Timer
	Cor.Update 						'update ball tracking (this sometimes goes in the RDampen_Timer sub)
	RollingUpdate					'update rolling sounds
	DoDTAnim 						'handle drop target animations
	DoSTAnim						'handle stand up target animations
End Sub

' The frame timer interval is -1, so executes at the display frame rate
dim FrameTime, InitFrameTime : InitFrameTime = 0

Sub FrameTimer_Timer()
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime
    SoundCmdListener
	BackdropUpdate
	FlipperVisualUpdate
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
End Sub

Sub BackdropUpdate
	dim l
	For Each l in BGlights
		If l.GetInPlayIntensity > 0 Then
			l.Bulb = 0
		Else
			l.Bulb = 1
		End If
	Next
End Sub

'**********************************************
'Solenoid Call backs
'**********************************************
SolCallback(1)="TopFlap"		'Top Sling
SolCallback(2)="RightFlap"		'Right Sling
SolCallback(3)="LeftFlap"		'Left Sling
SolCallback(4)="SolKnocker"
SolCallback(5)="SolDropUp"		'Drop Targets
SolCallback(6)="SolRob1"
SolCallback(7)="SolRob2"
SolCallback(10)="SolRob3"
SolCallback(14)="SolRob4"
SolCallback(15)="SolRob5"
SolCallback(16)="solrobhit1"
SolCallback(18)="solrobhit2"
SolCallback(21)="solrobhit3"
SolCallback(22)="solrobhit4"
SolCallback(23)="solrobhit5"
SolCallback(24)="SolBallRelease"

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub TopFlap(Enabled)
	If Enabled Then
		Right1SlingShot.SlingshotThreshold = 2500
	Else
		Right1SlingShot.SlingshotThreshold = SlingDefault
	End If
End Sub

Sub RightFlap(Enabled)
	If Enabled Then
		RightSlingShot.SlingshotThreshold = 2500
	Else
		RightSlingShot.SlingshotThreshold = SlingDefault
	End If
End Sub

Sub LeftFlap(Enabled)
	If Enabled Then
		LeftSlingShot.SlingshotThreshold = 2500
	Else
		LeftSlingShot.SlingshotThreshold = SlingDefault
	End If
End Sub

Sub SolKnocker(Enabled)
	If Enabled Then
		KnockerSolenoid
	End If
End Sub

Sub SolDropUp(enabled)
	dim xx
	if enabled then
		RandomSoundDropTargetReset sw45p
		DTRaise 43
		DTRaise 44
		DTRaise 45
		DTRaise 46
		DTRaise 47
		For each xx in ShadowDT
			xx.visible=True
		Next
	end if
End Sub

Sub SolLFlipper(Enabled)
'	If Enabled Then
'		LF.Fire
'		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
'			RandomSoundReflipUpLeft LeftFlipper
'		Else 
'			SoundFlipperUpAttackLeft LeftFlipper
'			RandomSoundFlipperUpLeft LeftFlipper
'		End If		
'	Else
'		LeftFlipper.RotateToStart
'		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
'			RandomSoundFlipperDownLeft LeftFlipper
'		End If
'		FlipperLeftHitParm = FlipperUpSoundLevel
'	End If
End Sub

Sub SolRFlipper(Enabled)
'	If Enabled Then
'		RF.Fire
'		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
'			RandomSoundReflipUpRight RightFlipper
'		Else 
'			SoundFlipperUpAttackRight RightFlipper
'			RandomSoundFlipperUpRight RightFlipper
'		End If
'	Else
'		RightFlipper.RotateToStart
'		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
'			RandomSoundFlipperDownRight RightFlipper
'		End If	
'		FlipperRightHitParm = FlipperUpSoundLevel
'	End If
End Sub

Sub SolBallRelease(enabled)
	If enabled Then
		RandomSoundBallRelease sw16
		sw16.kick 60, 25
	End if
End Sub

'**********************************************
'************* Robot Drop Targets *************
'**********************************************
Dim sw26pos,sw27pos,sw28pos,sw29pos,sw30pos
sw26pos=0:sw27pos=0:sw28pos=0:sw29pos=0:sw30pos=0

Sub SolRob1(enabled)
	if enabled then sw30pos=50:sw30t.enabled=1:RandomSoundDropTargetReset sw30b:sw30.isdropped=0:sw30w.isdropped=0	'Robot 1 Up command
End Sub

Sub SolRob2(enabled)
	if enabled then sw29pos=50:sw29t.enabled=1:RandomSoundDropTargetReset sw29b:sw29.isdropped=0:sw29w.isdropped=0	'Robot 2 Up command
End Sub

Sub SolRob3(enabled)
	if enabled then sw28pos=50:sw28t.enabled=1:RandomSoundDropTargetReset sw28b:sw28.isdropped=0:sw28w.isdropped=0	'Robot 3 Up command
End Sub

Sub SolRob4(enabled)
	if enabled then sw27pos=50:sw27t.enabled=1:RandomSoundDropTargetReset sw27b:sw27.isdropped=0:sw27w.isdropped=0	'Robot 4 Up command
End Sub

Sub SolRob5(enabled)
	if enabled then sw26pos=50:sw26t.enabled=1:RandomSoundDropTargetReset sw26b:sw26.isdropped=0:sw26w.isdropped=0	'Robot 5 Up command
End Sub

Sub solRobhit1(enabled)
	if enabled then sw30pos=0:sw30b.rotandtra3=0:sw30b.rotandtra4=0:sw30.isdropped=1:sw30w.isdropped=1:sw30.timerenabled=1:SoundDropTargetDrop sw30b	'Robot 1 Down command
End Sub

Sub solRobhit2(enabled)
	if enabled then sw29pos=0:sw29b.rotandtra3=0:sw29b.rotandtra4=0:sw29.isdropped=1:sw29w.isdropped=1:sw29.timerenabled=1:SoundDropTargetDrop sw29b	'Robot 2 Down command
End Sub

Sub solRobhit3(enabled)
	if enabled then sw28pos=0:sw28b.rotandtra3=0:sw28b.rotandtra4=0:sw28.isdropped=1:sw28w.isdropped=1:sw28.timerenabled=1:SoundDropTargetDrop sw28b	'Robot 3 Down command
End Sub

Sub solRobhit4(enabled)
	if enabled then sw27pos=0:sw27b.rotandtra3=0:sw27b.rotandtra4=0:sw27.isdropped=1:sw27w.isdropped=1:sw27.timerenabled=1:SoundDropTargetDrop sw27b	'Robot 4 Down command
End Sub

Sub solRobhit5(enabled)
	if enabled then sw26pos=0:sw26b.rotandtra3=0:sw26b.rotandtra4=0:sw26.isdropped=1:sw26w.isdropped=1:sw26.timerenabled=1:SoundDropTargetDrop sw26b	'Robot 5 Down command
End Sub

Sub sw26_Hit:TargetBouncer Activeball, 1:sw26b.rotandtra3=0:sw26b.rotandtra4=0:sw26.isdropped=1:sw26w.isdropped=1:sw26pos=0:sw26.timerenabled=1:SoundDropTargetDrop sw26b:End Sub	'Robot 5 hit
Sub sw27_Hit:TargetBouncer Activeball, 1:sw27b.rotandtra3=0:sw27b.rotandtra4=0:sw27.isdropped=1:sw27w.isdropped=1:sw27pos=0:sw27.timerenabled=1:SoundDropTargetDrop sw27b:End Sub	'Robot 4 hit
Sub sw28_Hit:TargetBouncer Activeball, 1:sw28b.rotandtra3=0:sw28b.rotandtra4=0:sw28.isdropped=1:sw28w.isdropped=1:sw28pos=0:sw28.timerenabled=1:SoundDropTargetDrop sw28b:End Sub	'Robot 3 hit
Sub sw29_Hit:TargetBouncer Activeball, 1:sw29b.rotandtra3=0:sw29b.rotandtra4=0:sw29.isdropped=1:sw29w.isdropped=1:sw29pos=0:sw29.timerenabled=1:SoundDropTargetDrop sw29b:End Sub	'Robot 2 hit
Sub sw30_Hit:TargetBouncer Activeball, 1:sw30b.rotandtra3=0:sw30b.rotandtra4=0:sw30.isdropped=1:sw30w.isdropped=1:sw30pos=0:sw30.timerenabled=1:SoundDropTargetDrop sw30b:End Sub	'Robot 1 hit

Sub sw26_timer()																				'Robot 5 going Down
	sw26pos = sw26pos+5 : sw26b.rotandtra5 = 0-sw26pos
	if sw26pos=50 then controller.switch(26)=1:me.timerenabled=0
End Sub

Sub sw27_timer()																				'Robot 4 going Down
	sw27pos = sw27pos+5 : sw27b.rotandtra5 = 0-sw27pos
	if sw27pos=50 then controller.switch(27)=1:me.timerenabled=0
End Sub

Sub sw28_timer()																				'Robot 3 going Down
	sw28pos = sw28pos+5 : sw28b.rotandtra5 = 0-sw28pos
	if sw28pos=50 then controller.switch(28)=1:me.timerenabled=0
End Sub

Sub sw29_timer()																				'Robot 2 going Down
	sw29pos = sw29pos+5 : sw29b.rotandtra5 = 0-sw29pos
	if sw29pos=50 then controller.switch(29)=1:me.timerenabled=0
End Sub

Sub sw30_timer()																				'Robot 1 going Down
	sw30pos = sw30pos+5 : sw30b.rotandtra5 = 0-sw30pos
	if sw30pos=50 then controller.switch(30)=1:me.timerenabled=0
End Sub

Sub sw30t_timer()																				'Robot 1 going Up
	sw30pos = sw30pos-5 : sw30b.rotandtra5 = 0-sw30pos
	if sw30pos=0 then sw30b.rotandtra3=-7:sw30b.rotandtra4=7:controller.switch(30)=0:me.enabled=0
End Sub

Sub sw29t_timer()																				'Robot 2 going Up
	sw29pos = sw29pos-5 : sw29b.rotandtra5 = 0-sw29pos
	if sw29pos=0 then sw29b.rotandtra3=-7:sw29b.rotandtra4=7:controller.switch(29)=0:me.enabled=0
End Sub

Sub sw28t_timer()																				'Robot 3 going Up
	sw28pos = sw28pos-5 : sw28b.rotandtra5 = 0-sw28pos
	if sw28pos=0 then sw28b.rotandtra3=-7:sw28b.rotandtra4=7:controller.switch(28)=0:me.enabled=0
End Sub

Sub sw27t_timer()																				'Robot 4 going Up
	sw27pos = sw27pos-5 : sw27b.rotandtra5 = 0-sw27pos
	if sw27pos=0 then sw27b.rotandtra3=-7:sw27b.rotandtra4=7:controller.switch(27)=0:me.enabled=0
End Sub

Sub sw26t_timer()																				'Robot 5 going Up
	sw26pos = sw26pos-5 : sw26b.rotandtra5 = 0-sw26pos
	if sw26pos=0 then sw26b.rotandtra3=-7:sw26b.rotandtra4=7:controller.switch(26)=0:me.enabled=0
End Sub


'**********************************************************************************************************

 ' Drain hole
Sub sw16_Hit:Controller.Switch(16) = 1:RandomSoundDrain sw16:End Sub
Sub sw16_unhit:Controller.Switch(16)=0:end sub

'Star Triggers
Sub sw17_hit:Controller.Switch(17)=1 : End Sub
Sub sw17_unhit:Controller.Switch(17)=0:end sub
Sub sw25_hit:Controller.Switch(25)=1 : End Sub
Sub sw25_unhit:Controller.Switch(25)=0:end sub
Sub sw36_hit:Controller.Switch(36)=1 : End Sub
Sub sw36_unhit:Controller.Switch(36)=0:end sub
Sub sw48a_hit:Controller.Switch(48)=1 : End Sub
Sub sw48a_unhit:Controller.Switch(48)=0:end sub
Sub sw48b_hit:Controller.Switch(48)=1 : End Sub
Sub sw48b_unhit:Controller.Switch(48)=0:end sub

'Wire Triggers
Sub sw18_hit:Controller.Switch(18)=1 : End Sub
sub sw18_unhit:Controller.Switch(18)=0:end sub
Sub sw19_hit:Controller.Switch(19)=1 : End Sub
sub sw19_unhit:Controller.Switch(19)=0:end sub
Sub sw22_hit:Controller.Switch(22)=1 : End Sub
sub sw22_unhit:Controller.Switch(22)=0:end sub
Sub sw23_hit:Controller.Switch(23)=1 : RampExit.rotx=0:End Sub	'Reset Robot Bridge exit prim
sub sw23_unhit:Controller.Switch(23)=0:end sub
Sub sw24_hit:Controller.Switch(24)=1 : End Sub
sub sw24_unhit:Controller.Switch(24)=0:end sub
Sub sw50_hit:Controller.Switch(50)=1 : End Sub
sub sw50_unhit:Controller.Switch(50)=0:end sub
Sub sw51_hit:Controller.Switch(51)=1 : End Sub
sub sw51_unhit:Controller.Switch(51)=0:end sub
Sub sw52_hit:Controller.Switch(52)=1 : End Sub
sub sw52_unhit:Controller.Switch(52)=0:end sub

'Drop Targets
Sub Sw43_Hit:DTHit 43 End Sub':dtR.Hit 1 :End Sub  
Sub Sw44_Hit:DTHit 44 End Sub':dtR.Hit 2 :End Sub  
Sub Sw45_Hit:DTHit 45 End Sub':dtR.Hit 3 :End Sub
Sub Sw46_Hit:DTHit 46 End Sub':dtR.Hit 4 :End Sub  
Sub Sw47_Hit:DTHit 47 End Sub':dtR.Hit 5 :End Sub

'Bumpers
Sub Bumper1_Hit : vpmTimer.PulseSw(40) : RandomSoundBumperMiddle Bumper1: End Sub
Sub Bumper2_Hit : vpmTimer.PulseSw(41) : RandomSoundBumperTop Bumper2: End Sub
Sub Bumper3_Hit : vpmTimer.PulseSw(42) : RandomSoundBumperBottom Bumper3: End Sub

'Stand Up Targets
Sub sw31_hit:STHit 31:End Sub'vpmTimer.PulseSw 31:End Sub
Sub sw32_hit:STHit 32:End Sub'vpmTimer.PulseSw 32:End Sub
Sub sw33_hit:STHit 33:End Sub'vpmTimer.PulseSw 33:End Sub
Sub sw34_hit:STHit 34:End Sub'vpmTimer.PulseSw 34:End Sub
Sub Sw37_Hit:STHit 37:End Sub'vpmTimer.PulseSw 37:End Sub 
Sub Sw38_Hit:STHit 38:End Sub'vpmTimer.PulseSw 38:End Sub 
Sub Sw39_Hit:STHit 39:End Sub'vpmTimer.PulseSw 39:End Sub

'Scoring Rubber
Sub sw49_hit:vpmTimer.PulseSw 49:End Sub

'Wire trigger on ramp
Sub sw53_hit:vpmTimer.PulseSw 53:End Sub


'**********************************************************************************************************
'KeyHandling
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)

	If keycode = LeftFlipperKey Then
		If Not FlippersDisabled Then
			FlipperActivate LeftFlipper, LFPress
			LF.Fire
			If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
				RandomSoundReflipUpLeft LeftFlipper
			Else 
				SoundFlipperUpAttackLeft LeftFlipper
				RandomSoundFlipperUpLeft LeftFlipper
			End If
		End If
	End If

	If keycode = RightFlipperKey Then
		If Not FlippersDisabled Then
			FlipperActivate RightFlipper, RFPress
			RF.Fire
			If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
				RandomSoundReflipUpRight RightFlipper
			Else 
				SoundFlipperUpAttackRight RightFlipper
				RandomSoundFlipperUpRight RightFlipper
			End If
		End If
	End If

	If keycode = PlungerKey Then Plunger.Pullback : SoundPlungerPull
	If keycode = LeftTiltKey Then Nudge 90, 1.5 : SoundNudgeLeft
	If keycode = RightTiltKey Then Nudge 270, 1.5 : SoundNudgeRight
	If keycode = CenterTiltKey Then Nudge 0, 1.5 : SoundNudgeCenter
	If keycode = StartGameKey Then SoundStartButton
	If keycode = MechanicalTilt Then SoundNudgeCenter()
	If keycode = AddCreditKey or keycode = AddCreditKey2 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If
	If KeyCode = RightMagnaSave Then
		if DisableLUTSelector = 0 then
            LUTSet = LUTSet  + 1
			if LutSet > 11 then LUTSet = 0
			lutsetsounddir = 1
			If LutToggleSound then
				If lutsetsounddir = 1 Then
					Playsound "robotup", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, 1
				End If
				If lutsetsounddir = -1 Then
					Playsound "robotdrop", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, -1
				End If
			end if
			SetLUT
			ShowLUT
		end if
	End If
	If keycode = LeftMagnaSave Then
		if DisableLUTSelector = 0 then
			LUTSet = LUTSet - 1
			if LutSet < 0 then LUTSet = 11
			lutsetsounddir = -1
			If LutToggleSound then
				If lutsetsounddir = 1 Then
					Playsound "robotup", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, 1
				End If
				If lutsetsounddir = -1 Then
					Playsound "robotdrop", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, -1
				End If
			end if
			SetLUT
			ShowLUT
		end if
	end if

	If vpmKeyDown(keycode) Then Exit Sub
End Sub

' Key Up
Sub Table1_KeyUp(ByVal KeyCode)
	If KeyCode = PlungerKey Then
		Plunger.Fire
		If BIPL = True Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
	End If

	If keycode = LeftFlipperKey Then 
		FlipperDeActivate LeftFlipper, LFPress
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then RandomSoundFlipperDownLeft LeftFlipper
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If

	If keycode = RightFlipperKey Then
		FlipperDeActivate RightFlipper, RFPress
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then RandomSoundFlipperDownRight RightFlipper
		FlipperRightHitParm = FlipperUpSoundLevel
	End If

	If vpmKeyUp(keycode) Then Exit Sub
End Sub


'********************************************************
'  ROM SoundCommand Listener
'********************************************************

'*** Sound Commands ***

'It's good to note them here for reference

' Sound commands in Robot
'D4,D4,54,	- Challenge (New Game)
'D2,D2,52,	- Mission Completed (End Game)
'C4,C4,44,	- Whoa Whoa (Tilt)
'C3,C3,43,	- Ow (Tilt)
'E3,E3,63,	- New Ball


'*** Constants ***

' The commands from the ROM come through in hex
' so add constants for the numbers you're listening for

'Const hexFD = 253
Const hexE3 = 227
Const hexD4 = 212
Const hexD2 = 210
Const hexC3 = 196
Const hexC4 = 195
'Const hex7F = 127
'Const hex54 = 84
'Const hex52 = 82
'Const hex2C = 44
'Const hex1D = 29
'Const hex0D = 13
'Const hex0C = 12
'Const hex0B = 11
'Const hex09 = 9
'Const hex08 = 8
'Const hex04 = 4
'Const hex03 = 3
'Const hex02 = 2

'Dim SndCmdStr
Dim FlippersDisabled
Dim LastSnd : LastSnd = 0

FlippersDisabled = True

' *** Listener ***
'***WHAT YOU NEED:***

' Copy in the 10 "Listener###" textboxes from the backglass view
' Add the SoundCmdListener call to a fast timer that always runs
' Play the game, writing down the values for the event you need
' The event you want may have multiple sounds including sounds played at other times
' If unsure, you can note the values and watch to see if they are used elsewhere
' You may want to record your screen if it's moving fast

Sub SoundCmdListener
    Dim NewSounds,ii,Snd
    NewSounds=Controller.NewSoundCommands				' Listening to the ROM
    If Not IsEmpty(NewSounds) Then

	'* Finding the Values *

' Comment out or delete this part once you know the commands
' And delete the text boxes

'        SndCmdStr = ""									' Empty the string first
'		For ii=0 To UBound(NewSounds)					' Setting the new to a string to display (used for investigating commands)
'			Snd=NewSounds(ii,0)
'			If Snd<>0 And Snd<>255 Then
'				SndCmdStr = SndCmdStr & Hex(Snd) & ","
'			End If
'		Next

'		Listener010.Text = Listener009.Text				' Display sound commands in textboxes
'		Listener009.Text = Listener008.Text				' ^^^
'		Listener008.Text = Listener007.Text				' ^^^
'		Listener007.Text = Listener006.Text				' ^^^
'		Listener006.Text = Listener005.Text				' ^^^
'		Listener005.Text = Listener004.Text				' ^^^
'		Listener004.Text = Listener003.Text				' ^^^
'		Listener003.Text = Listener002.Text				' ^^^
'		Listener002.Text = Listener001.Text				' ^^^
'		Listener001.Text = SndCmdStr					' Arrange them vertically so the sound commands scroll up the screen

	'* Using them to do stuff *

		For ii=0 To UBound(NewSounds)				' Listen for specific commands; if a specified combination occurs, do a thing
			Snd=NewSounds(ii,0)
'From Robot:
			If LastSnd = hexD4 And Snd = hexD4 Then	FlippersDisabled = False : GIon		'New Game, Flippers On, GI Lights On (Lampz number specific to Robot)
			If LastSnd = hexE3 And Snd = hexE3 Then	FlippersDisabled = False : GIon		'New Ball, "
			If LastSnd = hexC4 And Snd = hexC4 Then FlippersDisabled = True : GIoff		'Tilt, Flippers Off, Lights Off
			If LastSnd = hexC3 And Snd = hexC3 Then FlippersDisabled = True : GIoff		' "
			If LastSnd = hexD2 And Snd = hexD2 Then FlippersDisabled = True	: GIoff		'End Game, Flippers and GI Off

			LastSnd = Snd							' Remembering the previous bit of hex
'			debug.print "LastSnd: " & LastSnd
		Next
    End If
End Sub

Sub GIoff
	dim xx
	If GIShutDown Then
		gilvl = 0
		Lampz.state(100) = 0
		If ExtraGI Then
			For Each xx in GIxtra
				xx.state = 0
			Next
		End If
		If FlipperLights Then FlipBulbL.state = 0 : FlipBulbR.state = 0
		For each xx in ShadowDT
			xx.visible=False
		Next
		PlayfieldOn.visible= 0
		Sidewall_On.visible = 0 : Sidewall_Off.visible = 1
		Primitive6.BlendDisableLighting=0:Primitive11.BlendDisableLighting=0:Primitive15.BlendDisableLighting=0:Primitive22.BlendDisableLighting=0:Primitive23.BlendDisableLighting=0
		sw43p.BlendDisableLighting=0:sw44p.BlendDisableLighting=0:sw45p.BlendDisableLighting=0:sw46p.BlendDisableLighting=0:sw47p.BlendDisableLighting=0
		FlipperT10.BlendDisableLighting = 0 : FlipperT12.BlendDisableLighting = 0
		psw33.BlendDisableLighting = 0 : psw34.BlendDisableLighting = 0
	End If

	Bumper1.Threshold=2500:Bumper2.Threshold=2500:Bumper3.Threshold=2500
	Right1SlingShot.SlingshotThreshold=2500:RightSlingShot.SlingshotThreshold=2500:LeftSlingShot.SlingshotThreshold=2500
	LeftFlipper.RotateToStart : RightFlipper.RotateToStart
End Sub

Sub GIon
	dim xx
	If GIShutDown Then
		gilvl = 1
		Lampz.state(100) = 1
		If ExtraGI Then
			For Each xx in GIxtra
				xx.state = 1
			Next
		End If
		If FlipperLights Then FlipBulbL.state = 1 : FlipBulbR.state = 1
		If FlipperLights Then
			FlipBulbL.state = 1 : FlipBulbR.state = 1
			FlipperT10.BlendDisableLighting = 0.7 : FlipperT12.BlendDisableLighting = 0.7
		Else
			FlipperT10.BlendDisableLighting = 0.5 : FlipperT12.BlendDisableLighting = 0.5
		End If
		PlayfieldOn.visible= 1
		Sidewall_On.visible = 1 : Sidewall_Off.visible = 0
		Primitive6.BlendDisableLighting=0.1:Primitive11.BlendDisableLighting=0.1:Primitive15.BlendDisableLighting=0.1:Primitive22.BlendDisableLighting=0.01:Primitive23.BlendDisableLighting=0.01
		sw43p.BlendDisableLighting=0.01:sw44p.BlendDisableLighting=0.01:sw45p.BlendDisableLighting=0.01:sw46p.BlendDisableLighting=0.01:sw47p.BlendDisableLighting=0.01
		psw33.BlendDisableLighting = 0.5 : psw34.BlendDisableLighting = 0.5
	End If

	Bumper1.Threshold=BumperDefault:Bumper2.Threshold=BumperDefault:Bumper3.Threshold=BumperDefault
	Right1SlingShot.SlingshotThreshold=SlingDefault:RightSlingShot.SlingshotThreshold=SlingDefault:LeftSlingShot.SlingshotThreshold=SlingDefault
End Sub

'********************************************************
'  END ROM SoundCommand Listener
'********************************************************


'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

'Dim FadingLevel(150)
'Lights, Ramps & Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

'        FadingLevel(nr) = abs(value) + 4
'Sub FadeObj(nr, object, a, b, c, d)
'    Select Case FadingLevel(nr)
'        Case 4:object.image = b:FadingLevel(nr) = 6                   'fading to off...
'        Case 5:object.image = a:FadingLevel(nr) = 1                   'ON
'        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
'        Case 9:object.image = c:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
'        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1         'wait
'        Case 13:object.image = d:FadingLevel(nr) = 0                  'Off
'    End Select
'End Sub

'	FadeObj 11,sw30b,"robot_on","robot_med","robot_low","robot"

'	FadeObj 21,sw29b,"robot_on","robot_med","robot_low","robot"

'	FadeObj 29,sw28b,"robot_on","robot_med","robot_low","robot"

'	FadeObj 35,sw27b,"robot_on","robot_med","robot_low","robot"

'	FadeObj 40,sw26b,"robot_on","robot_med","robot_low","robot"

'******************************************************
'****  LAMPZ by nFozzy
'******************************************************
' 
' Lampz is a utility designed to manage and fade the lights and light-related objects on a table that is being driven by a ROM.
' To set up Lampz, one must populate the Lampz.MassAssign array with VPX Light objects, where the index of the MassAssign array
' corrisponds to the ROM index of the associated light. More that one Light object can be associated with a single MassAssign index (not shown in this example)
' Optionally, callbacks can be assigned for each index using the Lampz.Callback array. This is very useful for allowing 3D Insert primitives
' to be controlled by the ROM. Note, the aLvl parameter (i.e. the fading level that ranges between 0 and 1) is appended to the callback call.


Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
InitLampsNF              	' Setup lamp assignments
LampTimer.Interval = -1
LampTimer.Enabled = 1

Sub LampTimer_Timer()
	dim x, chglamp
	chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	Lampz.Update2	'update (fading logic only)
End Sub

Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub

Sub SetModLamp(id, val)
	Lampz.state(id) = val
End Sub


Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (max level / full MS fading time). The Modulate property must be set to 1 / max level if lamp is modulated.
	dim x : for x = 0 to 150 : Lampz.FadeSpeedUp(x) = 1/40 : Lampz.FadeSpeedDown(x) = 1/120 : Lampz.Modulate(x) = 1 : next
	

	'Lampz Assignments
	'  In a ROM based table, the lamp ID is used to set the state of the Lampz objects

	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays

	Lampz.MassAssign(1)= l1
	Lampz.MassAssign(1)= L1a
'	Lampz.Callback(1) = "DisableLighting p1, 200,"
	Lampz.MassAssign(2)= l2
	Lampz.MassAssign(2)= L2a
'	Lampz.Callback(2) = "DisableLighting p2, 200,"
	Lampz.MassAssign(3)= l3
	Lampz.MassAssign(3)= L3a
'	Lampz.Callback(3) = "DisableLighting p3, 200,"
	Lampz.MassAssign(4)= l4
	Lampz.MassAssign(4)= L4a
'	Lampz.Callback(4) = "DisableLighting p4, 200,"
	Lampz.MassAssign(5)= l5
	Lampz.MassAssign(5)= L5a
'	Lampz.Callback(5) = "DisableLighting p5, 200,"
	Lampz.MassAssign(6)= L6
	Lampz.MassAssign(7)= L7
	Lampz.MassAssign(8)= l8
	Lampz.MassAssign(8)= L8a
'	Lampz.Callback(8) = "DisableLighting p8, 200,"
	Lampz.MassAssign(9)= l9
	Lampz.MassAssign(9)= L9a
'	Lampz.Callback(9) = "DisableLighting p9, 200,"
'	Lampz.MassAssign(11)= l11
	Lampz.Callback(11) = "DisableLighting sw30b, 0.35,"	'Robot 1
	Lampz.MassAssign(12)= l12
	Lampz.MassAssign(12)= L12a
'	Lampz.Callback(12) = "DisableLighting p12, 130,"
	Lampz.MassAssign(13)= L13
'	Lampz.MassAssign(14)= l14
	Lampz.MassAssign(14)= bumperbiglight3
	Lampz.MassAssign(14)= bumpersmalllight3
'	Lampz.Callback(14) = "DisableLighting p14, 200,"
'	Lampz.MassAssign(15)= l15
	Lampz.MassAssign(15)= bumperbiglight2
	Lampz.MassAssign(15)= bumpersmalllight2
'	Lampz.Callback(15) = "DisableLighting p15, 200,"
'	Lampz.MassAssign(16)= l16
	Lampz.MassAssign(16)= bumperbiglight1
	Lampz.MassAssign(16)= bumpersmalllight1
'	Lampz.Callback(16) = "DisableLighting p16, 200,"
	Lampz.MassAssign(17)= L17
	Lampz.MassAssign(18)= l18
	Lampz.MassAssign(18)= L18a
'	Lampz.Callback(18) = "DisableLighting p18, 50,"
	Lampz.MassAssign(19)= l19
	Lampz.MassAssign(19)= L19a
'	Lampz.Callback(19) = "DisableLighting p19, 200,"
'	Lampz.MassAssign(21)= l21
	Lampz.Callback(21) = "DisableLighting sw29b, 0.35,"	'Robot 2
	Lampz.MassAssign(22)= l22
	Lampz.MassAssign(22)= L22a
'	Lampz.Callback(22) = "DisableLighting p22, 60,"
	Lampz.MassAssign(23)= l23
	Lampz.MassAssign(23)= L23a
'	Lampz.Callback(23) = "DisableLighting p23, 200,"
	Lampz.MassAssign(24)= l24
	Lampz.MassAssign(24)= L24a
'	Lampz.Callback(24) = "DisableLighting p24, 200,"
	Lampz.MassAssign(25)= l25
	Lampz.MassAssign(25)= L25a
'	Lampz.Callback(25) = "DisableLighting p25, 200,"
	Lampz.MassAssign(26)= l26
	Lampz.MassAssign(26)= L26a
'	Lampz.Callback(26) = "DisableLighting p26, 200,"
	Lampz.MassAssign(27)= l27
	Lampz.MassAssign(27)= L27a
	Lampz.MassAssign(27)= L27B
'	Lampz.Callback(27) = "DisableLighting p27, 200,"
	Lampz.MassAssign(28)= l28
	Lampz.MassAssign(28)= L28a
'	Lampz.Callback(28) = "DisableLighting p28, 200,"
'	Lampz.MassAssign(29)= l29
	Lampz.Callback(29) = "DisableLighting sw28b, 0.35,"	'Robot 3
	Lampz.MassAssign(30)= l30
	Lampz.MassAssign(30)= L30a
'	Lampz.Callback(30) = "DisableLighting p30, 130,"
	Lampz.MassAssign(31)= L31
	Lampz.MassAssign(32)= l32
	Lampz.MassAssign(32)= L32a
'	Lampz.Callback(32) = "DisableLighting p32, 200,"
	Lampz.MassAssign(34)= l34
	Lampz.MassAssign(34)= L34a
'	Lampz.Callback(34) = "DisableLighting p34, 200,"
'	Lampz.MassAssign(35)= l35
	Lampz.Callback(35) = "DisableLighting sw27b, 0.35,"	'Robot 4
	Lampz.MassAssign(36)= l36
	Lampz.MassAssign(36)= L36a
'	Lampz.Callback(36) = "DisableLighting p36, 200,"
	Lampz.MassAssign(38)= l38
	Lampz.MassAssign(38)= L38a
'	Lampz.Callback(38) = "DisableLighting p38, 50,"
	Lampz.MassAssign(39)= l39
	Lampz.MassAssign(39)= L39a
'	Lampz.Callback(39) = "DisableLighting p39, 200,"
'	Lampz.MassAssign(40)= l40
	Lampz.Callback(40) = "DisableLighting sw26b, 0.35,"	'Robot 5
	Lampz.MassAssign(41)= l41
	Lampz.MassAssign(41)= L41a
'	Lampz.Callback(41) = "DisableLighting p41, 60,"
	Lampz.MassAssign(42)= l42
	Lampz.MassAssign(42)= L42a
'	Lampz.Callback(42) = "DisableLighting p42, 200,"
	Lampz.MassAssign(43)= l43
	Lampz.MassAssign(43)= L43a
'	Lampz.Callback(43) = "DisableLighting p43, 200,"
	Lampz.MassAssign(44)= l44
	Lampz.MassAssign(44)= L44a
'	Lampz.Callback(44) = "DisableLighting p44, 200,"
	Lampz.MassAssign(45)= l45
	Lampz.MassAssign(45)= L45a
	Lampz.MassAssign(45)= l45B
'	Lampz.Callback(45) = "DisableLighting p45, 200,"
	Lampz.MassAssign(47)= l47
	Lampz.MassAssign(47)= L47a
'	Lampz.Callback(47) = "DisableLighting p47, 200,"
	Lampz.MassAssign(48)= l48
	Lampz.MassAssign(48)= L48a
'	Lampz.Callback(48) = "DisableLighting p48, 130,"
	Lampz.MassAssign(49)= l49
	Lampz.MassAssign(49)= L49a
'	Lampz.Callback(49) = "DisableLighting p49, 200,"
	Lampz.MassAssign(51)= l51
	Lampz.MassAssign(51)= L51a
'	Lampz.Callback(51) = "DisableLighting p51, 200,"
	Lampz.MassAssign(53)= l53
	Lampz.MassAssign(53)= L53a
'	Lampz.Callback(53) = "DisableLighting p53, 200,"
	Lampz.MassAssign(55)= l55
	Lampz.MassAssign(55)= L55a
'	Lampz.Callback(55) = "DisableLighting p55, 50,"
	Lampz.MassAssign(56)= L56
	Lampz.MassAssign(57)= l57
	Lampz.MassAssign(57)= L57a
'	Lampz.Callback(57) = "DisableLighting p57, 200,"
	Lampz.MassAssign(58)= l58
	Lampz.MassAssign(58)= L58a
'	Lampz.Callback(58) = "DisableLighting p58, 60,"
	Lampz.MassAssign(59)= l59
	Lampz.MassAssign(59)= L59a
'	Lampz.Callback(59) = "DisableLighting p59, 200,"
	Lampz.MassAssign(60)= L60
	Lampz.MassAssign(61)= l61
	Lampz.MassAssign(61)= L61a
'	Lampz.Callback(61) = "DisableLighting p61, 60,"
	Lampz.MassAssign(62)= L62
	Lampz.MassAssign(63)= l63
	Lampz.MassAssign(63)= L63a
'	Lampz.Callback(63) = "DisableLighting p63, 200,"
	Lampz.MassAssign(64)= l64
	Lampz.MassAssign(64)= L64a
'	Lampz.Callback(64) = "DisableLighting p64, 200,"
	Lampz.MassAssign(65)= l65
	Lampz.MassAssign(65)= L65a
'	Lampz.Callback(65) = "DisableLighting p65, 200,"
	Lampz.MassAssign(66)= L66
	Lampz.MassAssign(67)= L67
	Lampz.MassAssign(68)= l68
	Lampz.MassAssign(68)= L68a
'	Lampz.Callback(68) = "DisableLighting p68, 200,"
	Lampz.MassAssign(69)= l69
	Lampz.MassAssign(69)= L69a
'	Lampz.Callback(69) = "DisableLighting p69, 200,"
	Lampz.MassAssign(70)= l70
	Lampz.Callback(70) = "DisableLighting psw37, 0.5,"
	Lampz.MassAssign(71)= l71
	Lampz.Callback(71) = "DisableLighting psw38, 0.5,"
	Lampz.MassAssign(72)= L72
	Lampz.MassAssign(73)= l73
	Lampz.Callback(73) = "DisableLighting psw39, 0.5,"
	Lampz.MassAssign(75)= l75
	Lampz.MassAssign(75)= L75a
'	Lampz.Callback(75) = "DisableLighting p75, 200,"
	Lampz.MassAssign(76)= L76
	Lampz.MassAssign(77)= L77
	Lampz.MassAssign(78)= L78
	Lampz.MassAssign(79)= l79
	Lampz.MassAssign(79)= L79a
'	Lampz.Callback(79) = "DisableLighting p79, 50,"

					'GI
	Lampz.obj(100) = ColtoArray(GI)
	Lampz.state(100) = 0
'	Lampz.obj(200) = ColtoArray(RightGI)
'	Lampz.state(200) = 0
'	Lampz.obj(300) = ColtoArray(LeftGI)
'	Lampz.state(300) = 0
	'Turn off all lamps on startup

	Lampz.Init	'This just turns state of any lamps to 1

	'Immediate update to turn on GI, turn off lamps
	Lampz.Update

End Sub


'====================
'Class jungle nf
'====================

'No-op object instead of adding more conditionals to the main loop
'It also prevents errors if empty lamp numbers are called, and it's only one object
'should be g2g?

Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
' Note: if using multiple 'LampFader' objects, set the 'name' variable to avoid conflicts with callbacks
'Version 0.14 - Updated to support modulated signals - Niwak

Class LampFader
	Public FadeSpeedDown(150), FadeSpeedUp(150)
	Private Lock(150), Loaded(150), OnOff(150)
	Public UseFunction
	Private cFilter
	Public UseCallback(150), cCallback(150)
	Public Lvl(150), Obj(150)
	Private Mult(150)
	Public FrameTime
	Private InitFrame
	Public Name

	Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(OnOff) 	'Set up fade speeds
			FadeSpeedDown(x) = 1/100	'fade speed down
			FadeSpeedUp(x) = 1/80		'Fade speed up
			UseFunction = False
			lvl(x) = 0
			OnOff(x) = 0
			Lock(x) = True : Loaded(x) = False
			Mult(x) = 1
		Next
		Name = "LampFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OF THESE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
		for x = 0 to uBound(OnOff) 		'clear out empty obj
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		''debug.print Lampz.Locked(100)	'debug
	Public Property Get state(idx) : state = OnOff(idx) : end Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function
	'Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property
	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'cCallback(idx) = String 'old execute method
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _

		dim tmp : tmp = Split(cCallback(idx), "___")

		dim str, x : for x = 0 to uBound(tmp)	'build proc contents
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x)="" then str = str & tmp(x) & " aLVL:"
		Next
		'msgbox "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		ExecuteGlobal Out

	End Property

	Public Property Let state(ByVal idx, input) 'Major update path
		if TypeName(input) <> "Double" and typename(input) <> "Integer"  and typename(input) <> "Long" then
			If input Then
				input = 1
			Else
				input = 0
			End If
		End If
		if Input <> OnOff(idx) then  'discard redundant updates
			OnOff(idx) = input
			Lock(idx) = False
			Loaded(idx) = False
		End If
	End Property

	'Mass assign, Builds arrays where necessary
	'Sub MassAssign(aIdx, aInput)
	Public Property Let MassAssign(aIdx, aInput)
		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
			if IsArray(aInput) then
				obj(aIdx) = aInput
			Else
				Set obj(aIdx) = aInput
			end if
		Else
			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
		end if
	end Property

	Sub SetLamp(aIdx, aOn) : state(aIdx) = aOn : End Sub	'Solenoid Handler

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x)' : debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.001 ' this can prevent init stuttering
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx)' : debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.001 ' this can prevent init stuttering
			end if
		Next
		''debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'Just runs TurnOnStates right now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) > 0 then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= OnOff(x) then Lvl(x) = OnOff(x) : Lock(x) = True
				else 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) > 0 then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= OnOff(x) then Lvl(x) = OnOff(x) : Lock(x) = True
				else 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx, aLvl : for x = 0 to uBound(OnOff)
			if not Loaded(x) then
				aLvl = Lvl(x)*Mult(x)
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(aLvl) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = aLvl : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(aLvl)
					Else
						obj(x).Intensityscale = aLvl
					End If
				end if
				'if TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" and typename(lvl(x)) <> "Long" then msgbox "uhh " & 2 & " = " & lvl(x)
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x))	'Callback
				If UseCallBack(x) then Proc name & x,aLvl	'Proc
				If Lock(x) Then
					if Lvl(x) = OnOff(x) or Lvl(x) = 0 then Loaded(x) = True	'finished fading
				end if
			end if
		Next
	End Sub
End Class


'Lamp Filter
Function LampFilter(aLvl)
	LampFilter = aLvl^1.6	'exponential curve?
End Function


'Helper functions
Sub Proc(string, Callback)	'proc using a string and one argument
	'On Error Resume Next
	dim p : Set P = GetRef(String)
	P Callback
	If err.number = 13 then  msgbox "Proc error! No such procedure: " & vbnewline & string
	if err.number = 424 then msgbox "Proc error! No such Object"
End Sub

Function AppendArray(ByVal aArray, aInput)	'append one value, object, or Array onto the end of a 1 dimensional array
	if IsArray(aInput) then 'Input is an array...
		dim tmp : tmp = aArray
		If not IsArray(aArray) Then	'if not array, create an array
			tmp = aInput
		Else						'Append existing array with aInput array
			Redim Preserve tmp(uBound(aArray) + uBound(aInput)+1)	'If existing array, increase bounds by uBound of incoming array
			dim x : for x = 0 to uBound(aInput)
				if isObject(aInput(x)) then
					Set tmp(x+uBound(aArray)+1 ) = aInput(x)
				Else
					tmp(x+uBound(aArray)+1 ) = aInput(x)
				End If
			Next
			AppendArray = tmp	 'return new array
		End If
	Else 'Input is NOT an array...
		If not IsArray(aArray) Then	'if not array, create an array
			aArray = Array(aArray, aInput)
		Else
			Redim Preserve aArray(uBound(aArray)+1)	'If array, increase bounds by 1
			if isObject(aInput) then
				Set aArray(uBound(aArray)) = aInput
			Else
				aArray(uBound(aArray)) = aInput
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

'******************************************************
'******  FLUPPER BUMPERS
'******************************************************
' Based on FlupperBumpers 0.145 final

' Explanation of how these bumpers work:
' There are 10 elements involved per bumper:
' - the shadow of the bumper ( a vpx flasher object)
' - the bumper skirt (primitive)
' - the bumperbase (primitive)
' - a vpx light which colors everything you can see through the bumpertop
' - the bulb (primitive)
' - another vpx light which lights up everything around the bumper
' - the bumpertop (primitive)
' - the VPX bumper object
' - the bumper screws (primitive)
' - the bulb highlight VPX flasher object
' All elements have a special name with the number of the bumper at the end, this is necessary for the fading routine and the initialisation.
' For the bulb and the bumpertop there is a unique material as well per bumpertop.
' To use these bumpers you have to first copy all 10 elements to your table.
' Also export the textures (images) with names that start with "Flbumper" and "Flhighlight" and materials with names that start with "bumper".
' Make sure that all the ten objects are aligned on center, if possible with the exact same x,y coordinates
' After that copy the script (below); also copy the BumperTimer vpx object to your table
' Every bumper needs to be initialised with the FlInitBumper command, see example below; 
' Colors available are red, white, blue, orange, yellow, green, purple and blacklight.
' In a GI subroutine you can then call set the bumperlight intensity with the "FlBumperFadeTarget(nr) = value" command
' where nr is the number of the bumper, value is between 0 (off) and 1 (full on) (so you can also use 0.3 0.4 etc).

' Notes:
' - There is only one color for the disk; you can photoshop it to a different color
' - The bumpertops are angle independent up to a degree; my estimate is -45 to + 45 degrees horizontally, 0 (topview) to 70-80 degrees (frontview)
' - I built in correction for the day-night slider; this might not work perfectly, depending on your table lighting
' - These elements, textures and materials do NOT integrate with any of the lighting routines I have seen in use in many VPX tables
'   (just find the GI handling routine and insert the FlBumperFadeTarget statement)
' - If you want to use VPX native bumperdisks just copy my bumperdisk but make it invisible



' prepare some global vars to dim/brighten objects when using day-night slider
Dim DayNightAdjust , DNA30, DNA45, DNA90
If NightDay < 10 Then
	DNA30 = 0 : DNA45 = (NightDay-10)/20 : DNA90 = 0 : DayNightAdjust = 0.4
Else
	DNA30 = (NightDay-10)/30 : DNA45 = (NightDay-10)/45 : DNA90 = (NightDay-10)/90 : DayNightAdjust = NightDay/25
End If

Dim FlBumperFadeActual(6), FlBumperFadeTarget(6), FlBumperColor(6), FlBumperTop(6), FlBumperSmallLight(6), Flbumperbiglight(6)
Dim FlBumperDisk(6), FlBumperBase(6), FlBumperBulb(6), FlBumperscrews(6), FlBumperActive(6), FlBumperHighlight(6)
Dim cnt : For cnt = 1 to 6 : FlBumperActive(cnt) = False : Next

' colors available are red, white, blue, orange, yellow, green, purple and blacklight
Dim ind
If LUTset = 11 Then
	For ind = 1 to 3 : FlInitBumper ind, "blacklight" : next
Else
	For ind = 1 to 3 : FlInitBumper ind, "white" : next
End If

' ### uncomment the statement below to change the color for all bumpers ###
' Dim ind : For ind = 1 to 5 : FlInitBumper ind, "green" : next

Sub FlInitBumper(nr, col)
	FlBumperActive(nr) = True
	' store all objects in an array for use in FlFadeBumper subroutine
	FlBumperFadeActual(nr) = 1 : FlBumperFadeTarget(nr) = 1.1: FlBumperColor(nr) = col
	Set FlBumperTop(nr) = Eval("bumpertop" & nr) : FlBumperTop(nr).material = "bumpertopmat" & nr
	Set FlBumperSmallLight(nr) = Eval("bumpersmalllight" & nr) : Set Flbumperbiglight(nr) = Eval("bumperbiglight" & nr)
	Set FlBumperDisk(nr) = Eval("bumperdisk" & nr) : Set FlBumperBase(nr) = Eval("bumperbase" & nr)
'	Set FlBumperBulb(nr) = Eval("bumperbulb" & nr) : FlBumperBulb(nr).material = "bumperbulbmat" & nr
'	Set FlBumperscrews(nr) = Eval("bumperscrews" & nr): FlBumperscrews(nr).material = "bumperscrew" & col
	Set FlBumperHighlight(nr) = Eval("bumperhighlight" & nr)
	' set the color for the two VPX lights
	select case col
		Case "red"
			FlBumperSmallLight(nr).color = RGB(255,4,0) : FlBumperSmallLight(nr).colorfull = RGB(255,24,0)
			FlBumperBigLight(nr).color = RGB(255,32,0) : FlBumperBigLight(nr).colorfull = RGB(255,32,0)
			FlBumperHighlight(nr).color = RGB(64,255,0)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.98
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "blue"
			FlBumperBigLight(nr).color = RGB(32,80,255) : FlBumperBigLight(nr).colorfull = RGB(32,80,255)
			FlBumperSmallLight(nr).color = RGB(0,80,255) : FlBumperSmallLight(nr).colorfull = RGB(0,80,255)
			FlBumperSmallLight(nr).TransmissionScale = 0 : MaterialColor "bumpertopmat" & nr, RGB(8,120,255)
			FlBumperHighlight(nr).color = RGB(255,16,8)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "green"
			FlBumperSmallLight(nr).color = RGB(8,255,8) : FlBumperSmallLight(nr).colorfull = RGB(8,255,8)
			FlBumperBigLight(nr).color = RGB(32,255,32) : FlBumperBigLight(nr).colorfull = RGB(32,255,32)
			FlBumperHighlight(nr).color = RGB(255,32,255) : MaterialColor "bumpertopmat" & nr, RGB(16,255,16) 
			FlBumperSmallLight(nr).TransmissionScale = 0.005
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "orange"
			FlBumperHighlight(nr).color = RGB(255,130,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).color = RGB(255,130,0) : FlBumperSmallLight(nr).colorfull = RGB (255,90,0)
			FlBumperBigLight(nr).color = RGB(255,190,8) : FlBumperBigLight(nr).colorfull = RGB(255,190,8)
		Case "white"
			FlBumperBigLight(nr).color = RGB(255,230,190) : FlBumperBigLight(nr).colorfull = RGB(255,230,190)
			FlBumperHighlight(nr).color = RGB(255,180,100) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.99
		Case "blacklight"
			FlBumperBigLight(nr).color = RGB(32,32,255) : FlBumperBigLight(nr).colorfull = RGB(32,32,255)
			FlBumperHighlight(nr).color = RGB(48,8,255) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "yellow"
			FlBumperSmallLight(nr).color = RGB(255,230,4) : FlBumperSmallLight(nr).colorfull = RGB(255,230,4)
			FlBumperBigLight(nr).color = RGB(255,240,50) : FlBumperBigLight(nr).colorfull = RGB(255,240,50)
			FlBumperHighlight(nr).color = RGB(255,255,220)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "purple"
			FlBumperBigLight(nr).color = RGB(80,32,255) : FlBumperBigLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).color = RGB(80,32,255) : FlBumperSmallLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).TransmissionScale = 0 : 
			FlBumperHighlight(nr).color = RGB(32,64,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
	end select
End Sub

Sub FlFadeBumper(nr, Z)
	FlBumperBase(nr).BlendDisableLighting = 0.5 * DayNightAdjust
'	UpdateMaterial(string, float wrapLighting, float roughness, float glossyImageLerp, float thickness, float edge, float edgeAlpha, float opacity,
'               OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive,
'               float elasticity, float elasticityFalloff, float friction, float scatterAngle) - updates all parameters of a material
	FlBumperDisk(nr).BlendDisableLighting = (0.5 - Z * 0.3 )* DayNightAdjust	

	select case FlBumperColor(nr)

		Case "blue" :
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(38-24*Z,130 - 98*Z,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20  + 500 * Z / (0.5 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
'			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 5000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 25 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 10000 * (Z^3) / (0.5 + DNA90)

		Case "green"	
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(16 + 16 * sin(Z*3.14),255,16 + 16 * sin(Z*3.14)), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 10 + 150 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 2 * DayNightAdjust + 20 * Z
'			FlBumperBulb(nr).BlendDisableLighting = 7 * DayNightAdjust + 6000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 6000 * (Z^3) / (1 + DNA90)
		
		Case "red" 
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 16 - 11*Z + 16 * sin(Z*3.14),0), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 100 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 18 * Z / (1 + DNA90)
'			FlBumperBulb(nr).BlendDisableLighting = 20 * DayNightAdjust + 9000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,20 + Z*4,8-Z*8)
		
		Case "orange"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 100 - 22*z  + 16 * sin(Z*3.14),Z*32), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 250 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z / (1 + DNA90)
'			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 2500 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,100 + Z*50, 0)

		Case "white"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 180 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 5 * DayNightAdjust + 30 * Z
'			FlBumperBulb(nr).BlendDisableLighting = 18 * DayNightAdjust + 3000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 8 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255,255 - 20*Z,255-65*Z) : FlBumperSmallLight(nr).colorfull = RGB(255,255 - 20*Z,255-65*Z)
			MaterialColor "bumpertopmat" & nr, RGB(255,235 - z*36,220 - Z*90)

		Case "blacklight"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 1, RGB(30-27*Z^0.03,30-28*Z^0.01, 255), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 900 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 60 * Z
'			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 30000 * Z^3
			Flbumperbiglight(nr).intensity = 25 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255-240*(Z^0.1),255 - 240*(Z^0.1),255) : FlBumperSmallLight(nr).colorfull = RGB(255-200*z,255 - 200*Z,255)
			MaterialColor "bumpertopmat" & nr, RGB(255-190*Z,235 - z*180,220 + 35*Z)

		Case "yellow"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 180 + 40*z, 48* Z), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 200 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 40 * Z / (1 + DNA90)
'			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 2000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 10 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,200, 24 - 24 * z)

		Case "purple" :
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(128-118*Z - 32 * sin(Z*3.14), 32-26*Z ,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 15  + 200 * Z / (0.5 + DNA30) 
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
'			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 10000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 25 * Z / (1 + DNA45) 
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (0.5 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(128-60*Z,32,255)


	end select
End Sub

Sub BumperTimer_Timer
	dim nr
	For nr = 1 to 6
		If FlBumperFadeActual(nr) < FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.8
			If FlBumperFadeActual(nr) > 0.99 Then FlBumperFadeActual(nr) = 1 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
		If FlBumperFadeActual(nr) > FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.4 / (FlBumperFadeActual(nr) + 0.1)
			If FlBumperFadeActual(nr) < 0.01 Then FlBumperFadeActual(nr) = 0 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
	next
End Sub


'******************************************************
'******  END FLUPPER BUMPERS
'******************************************************

'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************
Dim Digits(40)

Digits(0) = Array(LED10,LED11,LED12,LED13,LED14,LED15,LED16)
Digits(1) = Array(LED20,LED21,LED22,LED23,LED24,LED25,LED26)
Digits(2) = Array(LED30,LED31,LED32,LED33,LED34,LED35,LED36)
Digits(3) = Array(LED40,LED41,LED42,LED43,LED44,LED45,LED46)
Digits(4) = Array(LED50,LED51,LED52,LED53,LED54,LED55,LED56)
Digits(5) = Array(LED60,LED61,LED62,LED63,LED64,LED65,LED66)
Digits(6) = Array(LED70,LED71,LED72,LED73,LED74,LED75,LED76)
Digits(7) = Array(LED80,LED81,LED82,LED83,LED84,LED85,LED86)
Digits(8) = Array(LED90,LED91,LED92,LED93,LED94,LED95,LED96)
Digits(9) = Array(LED100,LED101,LED102,LED103,LED104,LED105,LED106)
Digits(10) = Array(LED110,LED111,LED112,LED113,LED114,LED115,LED116)
Digits(11) = Array(LED120,LED121,LED122,LED123,LED124,LED125,LED126)
Digits(12) = Array(LED130,LED131,LED132,LED133,LED134,LED135,LED136)
Digits(13) = Array(LED140,LED141,LED142,LED143,LED144,LED145,LED146)
Digits(14) = Array(LED150,LED151,LED152,LED153,LED154,LED155,LED156)
Digits(15) = Array(LED160,LED161,LED162,LED163,LED164,LED165,LED166)
Digits(16) = Array(LED170,LED171,LED172,LED173,LED174,LED175,LED176)
Digits(17) = Array(LED180,LED181,LED182,LED183,LED184,LED185,LED186)
Digits(18) = Array(LED190,LED191,LED192,LED193,LED194,LED195,LED196)
Digits(19) = Array(LED200,LED201,LED202,LED203,LED204,LED205,LED206)
Digits(20) = Array(LED210,LED211,LED212,LED213,LED214,LED215,LED216)
Digits(21) = Array(LED220,LED221,LED222,LED223,LED224,LED225,LED226)
Digits(22) = Array(LED230,LED231,LED232,LED233,LED234,LED235,LED236)
Digits(23) = Array(LED240,LED241,LED242,LED243,LED244,LED245,LED246)
Digits(24) = Array(LED250,LED251,LED252,LED253,LED254,LED255,LED256)
Digits(25) = Array(LED260,LED261,LED262,LED263,LED264,LED265,LED266)
Digits(26) = Array(LED270,LED271,LED272,LED273,LED274,LED275,LED276)
Digits(27) = Array(LED280,LED281,LED282,LED283,LED284,LED285,LED286)
Digits(28) = Array(LED4,LED2,LED6,LED7,LED5,LED1,LED3)
Digits(29) = Array(LED18,LED9,LED27,LED28,LED19,LED8,LED17)
Digits(30) = Array(LED39,LED37,LED48,LED49,LED47,LED29,LED38)
Digits(31) = Array(LED67,LED58,LED69,LED77,LED68,LED57,LED59)
Digits(32) = Array(LED88,LED79,LED97,LED98,LED89,LED78,LED87)
Digits(33) = Array(LED109,LED107,LED118,LED119,LED117,LED99,LED108)
Digits(34) = Array(LED137,LED128,LED139,LED147,LED138,LED127,LED129)
Digits(35) = Array(LED158,LED149,LED167,LED168,LED159,LED148,LED157)
Digits(36) = Array(LED179,LED177,LED188,LED189,LED187,LED169,LED178)
Digits(37) = Array(LED207,LED198,LED209,LED217,LED208,LED197,LED199)
Digits(38) = Array(LED228,LED219,LED237,LED238,LED229,LED218,LED227)
Digits(39) = Array(LED249,LED247,LED258,LED259,LED257,LED239,LED248)

 Sub DisplayTimer_Timer
    Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
    ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
		If DesktopMode = True Then
       For ii=0 To UBound(chgLED)
          num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
			if (num < 40) then
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

'*****************************
'	Sling Shot Animations
'*****************************
' Rstep and Lstep  are the variables that increment the animation
Dim RStep, Lstep, R1Step

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 20
	RandomSoundSlingshotLeft SLING2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 21
	RandomSoundSlingshotRight SLING1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub Right1SlingShot_Slingshot
	RS1.VelocityCorrect(ActiveBall)
	vpmTimer.PulseSw 35
	RandomSoundSlingshotRight SLING3
    R1Sling.Visible = 0
    R1Sling1.Visible = 1
    sling3.TransZ = -20
    R1Step = 0
    Right1SlingShot.TimerEnabled = 1
End Sub

Sub Right1SlingShot_Timer
    Select Case R1Step
        Case 3:R1Sling1.Visible = 0:R1Sling2.Visible = 1:sling3.TransZ = -10
        Case 4:R1Sling2.Visible = 0:R1Sling.Visible = 1:sling3.TransZ = 0:me.TimerEnabled = 0
    End Select
    R1Step = R1Step + 1
End Sub

'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

'Ambient (Room light source)
Const AmbientBSFactor 		= 0.9	'0 to 1, higher is darker
Const AmbientMovement		= 1		'1 to 4, higher means more movement as the ball moves left and right
Const offsetX				= 0		'Offset x position under ball	(These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY				= 5		'Offset y position under ball	 (for example 5,5 if the light is in the back left corner)
'Dynamic (Table light sources)
Const DynamicBSFactor 		= 0.99	'0 to 1, higher is darker
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness is technically most accurate for lights at z ~25 hitting a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source

' ***														***

' *** Trim or extend these to *match* the number of balls/primitives/flashers on the table!
dim objrtx1(2), objrtx2(2)
dim objBallShadow(2)
Dim OnPF(2)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1)
Dim DSSources(30), numberofsources

'Initialization
DynamicBSInit

sub DynamicBSInit()
	Dim iii, source

	for iii = 0 to tnob - 1								'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = 1 + iii/1000 + 0.01			'Separate z for layering without clipping
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = 1 + iii/1000 + 0.02
		objrtx2(iii).visible = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 1 + iii/1000 + 0.04
		objBallShadow(iii).visible = 0

		BallShadowA(iii).Opacity = 100*AmbientBSFactor
		BallShadowA(iii).visible = 0
	Next

	iii = 0

	For Each Source in DynamicSources
		DSSources(iii) = Array(Source.x, Source.y)
		iii = iii + 1
	Next
	numberofsources = iii
end sub

Sub BallOnPlayfieldNow(yeh, num)		'Only update certain things once, save some cycles
	If yeh Then
		OnPF(num) = True
		bsRampOff gBOT(num).ID
'		debug.print "Back on PF"
		UpdateMaterial objBallShadow(num).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(num).size_x = 5
		objBallShadow(num).size_y = 4.5
		objBallShadow(num).visible = 1
		BallShadowA(num).visible = 0
	Else
		OnPF(num) = False
'		debug.print "Leaving PF"
	End If
End Sub

Sub DynamicBSUpdate
	Dim falloff: falloff = 150 'Max distance to light sources, can be changed dynamically if you have a reason
	Dim ShadowOpacity1, ShadowOpacity2 
	Dim LSd, iii
	Dim dist1, dist2, src1, src2
	Dim bsRampType
'	Dim gBOT: gBOT=getballs	'Uncomment if you're deleting balls - Don't do it! #SaveTheBalls

	'Hide shadow of deleted balls
'	For s = UBound(gBOT) + 1 to tnob - 1
'		objrtx1(s).visible = 0
'		objrtx2(s).visible = 0
'		objBallShadow(s).visible = 0
'		BallShadowA(s).visible = 0
'	Next

'	If UBound(gBOT) < lob Then Exit Sub		'No balls in play, exit

'The Magic happens now
'	For s = lob to UBound(gBOT)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 units and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

	'Primitive shadow on playfield, flasher shadow in ramps
		If AmbientBallShadowOn = 1 Then
			If rBall.Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If OnPF(0) Then BallOnPlayfieldNow False, 0		'One-time update
				bsRampType = getBsRampType(gBOT(0).id)

				If Not bsRampType = bsRamp Then		'Primitive visible on PF
					objBallShadow(0).visible = 1
					objBallShadow(0).X = rBall.X + (rBall.X - (tablewidth/2))/(Ballsize/AmbientMovement) + offsetX
					objBallShadow(0).Y = rBall.Y + offsetY
					objBallShadow(0).size_x = 5 * ((rBall.Z+BallSize)/80)			'Shadow gets larger and more diffuse as it moves up
					objBallShadow(0).size_y = 4.5 * ((rBall.Z+BallSize)/80)
					UpdateMaterial objBallShadow(0).material,1,0,0,0,0,0,AmbientBSFactor*(30/(rBall.Z)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objBallShadow(0).visible = 0
				End If

				If bsRampType = bsRampClear Or bsRampType = bsRamp Then		'Flasher visible on opaque ramp
					BallShadowA(0).visible = 1
					BallShadowA(0).X = rBall.X + offsetX
					BallShadowA(0).Y = rBall.Y + offsetY + BallSize/10
					BallShadowA(0).height=rBall.z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				Elseif bsRampType = bsWire or bsRampType  = bsNone Then		'Turn it off on wires or falling out of a ramp
					BallShadowA(0).visible = 0
				End If

			Elseif rBall.Z <= 30 And rBall.Z > 20 Then	'On pf, primitive only
				If Not OnPF(0) Then BallOnPlayfieldNow True, 0
				objBallShadow(0).X = rBall.X + (rBall.X - (tablewidth/2))/(Ballsize/AmbientMovement) + offsetX
				objBallShadow(0).Y = rBall.Y + offsetY

			Else												'Under pf, flasher shadow
				If OnPF(0) Then BallOnPlayfieldNow False, 0
				objBallShadow(0).visible = 0
				BallShadowA(0).visible = 1
				BallShadowA(0).X = rBall.X + offsetX
				BallShadowA(0).Y = rBall.Y + offsetY
				BallShadowA(0).height=rBall.z - BallSize/4
			end if

	'Flasher shadow everywhere
		Elseif AmbientBallShadowOn = 2 Then
			If rBall.Z > 30 Then							'In a ramp
				BallShadowA(0).X = rBall.X + offsetX
				BallShadowA(0).Y = rBall.Y + offsetY + BallSize/10
				BallShadowA(0).height=rBall.z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			Elseif rBall.Z <= 30 And rBall.Z > 20 Then	'On pf
				BallShadowA(0).visible = 1
				BallShadowA(0).X = rBall.X + (rBall.X - (tablewidth/2))/(Ballsize/AmbientMovement) + offsetX
				BallShadowA(0).Y = rBall.Y + offsetY
				BallShadowA(0).height= 1.04
			Else											'Under pf
				BallShadowA(0).X = rBall.X + offsetX
				BallShadowA(0).Y = rBall.Y + offsetY
				BallShadowA(0).height=rBall.z - BallSize/4
			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If rBall.Z < 35 And rBall.X < 850 Then	'Parameters for where the shadows can show, here they are not visible above the table (no upper pf) or in the plunger lane
				dist1 = falloff:
				dist2 = falloff
				For iii = 0 to numberofsources - 1 ' Search the 2 nearest influencing lights
					LSd = Distance(rBall.x, rBall.y, DSSources(iii)(0), DSSources(iii)(1)) 'Calculating the Linear distance to the Source
					If LSd < falloff And gilvl > 0 Then
'					If LSd < dist2 And ((DSGISide(iii) = 0 And Lampz.State(100)>0) Or (DSGISide(iii) = 1 And Lampz.State(104)>0)) Then	'Adapted for TZ with GI left / GI right
						dist2 = dist1
						dist1 = LSd
						src2 = src1
						src1 = iii
					End If
				Next
				ShadowOpacity1 = 0
				If dist1 < falloff Then
					objrtx1(0).visible = 1 : objrtx1(0).X = rBall.X : objrtx1(0).Y = rBall.Y
					objrtx1(0).rotz = AnglePP(DSSources(src1)(0), DSSources(src1)(1), rBall.X, rBall.Y) + 90
					ShadowOpacity1 = 1 - dist1 / falloff
					objrtx1(0).size_y = Wideness * ShadowOpacity1 + Thinness
					UpdateMaterial objrtx1(0).material,1,0,0,0,0,0,ShadowOpacity1*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx1(0).visible = 0
				End If
				ShadowOpacity2 = 0
				If dist2 < falloff Then
					objrtx2(0).visible = 1 : objrtx2(0).X = rBall.X : objrtx2(0).Y = rBall.Y + offsetY
					objrtx2(0).rotz = AnglePP(DSSources(src2)(0), DSSources(src2)(1), rBall.X, rBall.Y) + 90
					ShadowOpacity2 = 1 - dist2 / falloff
					objrtx2(0).size_y = Wideness * ShadowOpacity2 + Thinness
					UpdateMaterial objrtx2(0).material,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx2(0).visible = 0
				End If
				If AmbientBallShadowOn = 1 Then
					'Fades the ambient shadow (primitive only) when it's close to a light
					UpdateMaterial objBallShadow(0).material,1,0,0,0,0,0,AmbientBSFactor*(1 - max(ShadowOpacity1, ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					BallShadowA(0).Opacity = 100 * AmbientBSFactor * (1 - max(ShadowOpacity1, ShadowOpacity2))
				End If
			Else 'Hide dynamic shadows everywhere else, just in case
				objrtx2(0).visible = 0 : objrtx1(0).visible = 0
			End If
		End If
'	Next
End Sub
'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************

Dim bsDict
Set bsDict = New cvpmDictionary
Const bsNone = "None"
Const bsWire = "Wire"
Const bsRamp = "Ramp"
Const bsRampClear = "Clear"

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



'**********************************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'**********************************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7

sub TargetBouncer(aBall,defvalue)
    dim zMultiplier, vel, vratio
    if TargetBouncerEnabled = 1 and aball.z < 30 then
 '       debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
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

'******************************************************
'****  GNEREAL ADVICE ON PHYSICS
'******************************************************
'
' It's advised that flipper corrections, dampeners, and general physics settings should all be updated per these 
' examples as all of these improvements work together to provide a realistic physics simulation.
'
' Tutorial videos provided by Bord
' Flippers: 	https://www.youtube.com/watch?v=FWvM9_CdVHw
' Dampeners: 	https://www.youtube.com/watch?v=tqsxx48C6Pg
' Physics: 		https://www.youtube.com/watch?v=UcRMG-2svvE
'
'
' Note: BallMass must be set to 1. BallSize should be set to 50 (in other words the ball radius is 25) 
'
' Recommended Table Physics Settings
' | Gravity Constant             | 0.97      |
' | Playfield Friction           | 0.15-0.25 |
' | Playfield Elasticity         | 0.25      |
' | Playfield Elasticity Falloff | 0         |
' | Playfield Scatter            | 0         |
' | Default Element Scatter      | 2         |
'
' Bumpers
' | Force         | 9.5-10.5 |
' | Hit Threshold | 1.6-2    |
' | Scatter Angle | 2        |
' 
' Slingshots
' | Hit Threshold      | 2    |
' | Slingshot Force    | 4-5  |
' | Slingshot Theshold | 2-3  |
' | Elasticity         | 0.85 |
' | Friction           | 0.8  |
' | Scatter Angle      | 1    |



'******************************************************
'****  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level well need the following:
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
'


'******************************************************
' Flippers Polarity (Select appropriate sub based on era) 

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

'*******************************************
' Mid 80's

Sub InitPolarity()
        dim x, a : a = Array(LF, RF)
        for each x in a
                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
                x.enabled = True
                x.TimeDelay = 80
        Next

        AddPt "Polarity", 0, 0, 0
        AddPt "Polarity", 1, 0.05, -3.7        
        AddPt "Polarity", 2, 0.33, -3.7
        AddPt "Polarity", 3, 0.37, -3.7
        AddPt "Polarity", 4, 0.41, -3.7
        AddPt "Polarity", 5, 0.45, -3.7 
        AddPt "Polarity", 6, 0.576,-3.7
        AddPt "Polarity", 7, 0.66, -2.3
        AddPt "Polarity", 8, 0.743, -1.5
        AddPt "Polarity", 9, 0.81, -1
        AddPt "Polarity", 10, 0.88, 0

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

' Flipper trigger hit subs
Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub

' Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub

' This subroutine updates the flipper shadows and visual primitives
Sub FlipperVisualUpdate
	FlipperLSh.RotZ = LeftFlipper.CurrentAngle
	FlipperRSh.RotZ = RightFlipper.CurrentAngle
	FlipperT12.RotY = LeftFlipper.CurrentAngle
	FlipperT10.RotY = RightFlipper.CurrentAngle
End Sub

'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

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
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR

				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
		End If
		RemoveBall aBall
	End Sub
End Class


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
'	Dim b

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
'		debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
'			For b = 0 to Ubound(gBOT)
				If FlipperTrigger(rBall.x, rBall.y, Flipper1) Then
'					Debug.Print "ball in flip1. exit"
					exit Sub
				end If
'			Next
'			For b = 0 to Ubound(gBOT)
				If FlipperTrigger(rBall.x, rBall.y, Flipper2) Then
					rBall.velx = rBall.velx / 1.3
					rBall.vely = rBall.vely - 0.5
				end If
'			Next
		End If
	Else 
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then EOSNudge1 = 0
	End If
End Sub

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

Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
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
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 6
	Case 2:
		SOSRampup = 8.5
End Select

Const ReflipAngle = 20
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
'		Dim b
'		For b = 0 to UBound(gBOT)
			If Distance(rBall.x, rBall.y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If rBall.vely >= -0.4 Then rBall.vely = -0.4
			End If
'		Next
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
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************
' To add these slingshot corrections:
' 	- On the table, add the endpoint primitives that define the two ends of the Slingshot
'	- Initialize the SlingshotCorrection objects in InitSlingCorrection
' 	- Call the .VelocityCorrect methods from the respective _Slingshot event sub


dim LS : Set LS = New SlingshotCorrection
dim RS : Set RS = New SlingshotCorrection
dim RS1 : Set RS1 = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection

	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS

	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS

	RS1.Object = RightSlingshot
	RS1.EndPoint1 = EndPoint1RS1
	RS1.EndPoint2 = EndPoint2RS1

	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	' These values are best guesses. Retune them if needed based on specific table research.
	AddSlingsPt 0, 0.00,	-4
	AddSlingsPt 1, 0.45,	-7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4

End Sub


Sub AddSlingsPt(idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LS, RS)
	dim x : for each x in a
		x.addpoint idx, aX, aY
	Next
End Sub

Class SlingshotCorrection
	Public DebugOn, Enabled
	private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2

	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): Enabled = True : End Sub 

	Public Property let Object(aInput) : Set Slingshot = aInput : End Property
	Public Property Let EndPoint1(aInput) : SlingX1 = aInput.x: SlingY1 = aInput.y: End Property
	Public Property Let EndPoint2(aInput) : SlingX2 = aInput.x: SlingY2 = aInput.y: End Property

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
		If gametime > 100 then Report
	End Sub

	Public Sub Report()         'debug, reports all coords in tbPL.text
		If not debugOn then exit sub
		dim a1, a2 : a1 = ModIn : a2 = ModOut
		dim str, x : for x = 0 to uBound(a1) : str = str & x & ": " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		TBPout.text = str
	End Sub


	Public Sub VelocityCorrect(aBall)
		dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then 
			XL = SlingX1 : YL = SlingY1 : XR = SlingX2 : YR = SlingY2
		Else
			XL = SlingX2 : YL = SlingY2 : XR = SlingX1 : YR = SlingY1
		End If

		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then 
			If ABS(XR-XL) > ABS(YR-YL) Then 
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If

		'Velocity angle correction
		If not IsEmpty(ModIn(0) ) then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'debug.print " BallPos=" & BallPos &" Angle=" & Angle 
			'debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled then aBall.Velx = RotVxVy(0)
			If Enabled then aBall.Vely = RotVxVy(1)
			'debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely 
			'debug.print " " 
		End If
	End Sub

End Class


'******************************************************
'  FLIPPER POLARITY. RUBBER DAMPENER, AND SLINGSHOT CORRECTION SUPPORTING FUNCTIONS 
'******************************************************


Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub


' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)        'Shuffle objects in a temp array
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
	redim aArray(aCount-1+offset)        'Resize original array
	for x = 0 to aCount-1                'set objects back into original array
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
Function PSlope(Input, X1, Y1, X2, Y2)        'Set up line via two points, no clamping. Input X, output Y
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
	dim ii : for ii = 1 to uBound(xKeyFrame)        'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)        'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )         'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )        'Clamp upper

	LinearEnvelope = Y
End Function


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


'******************************************************
'****  PHYSICS DAMPENERS
'******************************************************
'
' These are data mined bounce curves, 
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx)
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 5 then		
		RandomSoundRubberStrong 1
	End if
	If finalspeed <= 5 then
		RandomSoundRubberWeak()
	End If
	RubbersD.dampen Activeball
	TargetBouncer Activeball, 1
End Sub

Sub dPostsB_Hit(idx)
	dim finalspeed
	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
	If finalspeed > 5 then		
		RandomSoundRubberStrong 1
	End if
	If finalspeed <= 5 then
		RandomSoundRubberWeak()
	End If
	RubbersD.dampen Activeball
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


'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************


'******************************************************
'****  DROP TARGETS by Rothbauerw
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
Dim DT43, DT44, DT45, DT46, DT47

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

Set DT43 = (new DropTarget)(sw43, sw43a, sw43p, 43, 0, false)
Set DT44 = (new DropTarget)(sw44, sw44a, sw44p, 44, 0, false)
Set DT45 = (new DropTarget)(sw45, sw45a, sw45p, 45, 0, false)
Set DT46 = (new DropTarget)(sw46, sw46a, sw46p, 46, 0, false)
Set DT47 = (new DropTarget)(sw47, sw47a, sw47p, 47, 0, false)

Dim DTArray
DTArray = Array(DT43, DT44, DT45, DT46, DT47)

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
'Const DTHitSound = "" 'Drop Target Hit sound
'Const DTDropSound = "DropTarget_Down" 'Drop Target Drop sound
'Const DTResetSound = "DropTarget_Up" 'Drop Target reset sound

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

	'Shadow animation, from Bord's BK
	DTShadow sw43p, dtsh43
	DTShadow sw44p, dtsh44
	DTShadow sw45p, dtsh45
	DTShadow sw46p, dtsh46
	DTShadow sw47p, dtsh47
End Sub

' Table specific function
Sub	DTShadow(prim, shadow)
	If prim.transz < -DTDropUnits/2 Then
		shadow.visible = false
	Else
		shadow.visible = true 
	End If
End Sub

Function DTAnimate(primary, secondary, prim, switch,  animate)
	dim transz, switchid
	Dim animtime, rangle

	switchid = switch

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
			Dim b
'			For b = 0 to UBound(gBOT)
				If InRotRect(rBall.x,rBall.y,prim.x, prim.y, prim.rotz, -25,-10,25,-10,25,25,-25,25) and rBall.z < prim.z+DTDropUnits+25 Then
					rBall.velz = 20
				End If
'			Next
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
'		STAND-UP TARGET INITIALIZATION
'******************************************************

Class StandupTarget
	Private m_primary, m_prim, m_sw, m_animate
  
	Public Property Get Primary(): Set Primary = m_primary: End Property
	Public Property Let Primary(input): Set m_primary = input: End Property
  
	Public Property Get Prim(): Set Prim = m_prim: End Property
	Public Property Let Prim(input): Set m_prim = input: End Property
  
	Public Property Get Sw(): Sw = m_sw: End Property
	Public Property Let Sw(input): m_sw = input: End Property
  
	Public Property Get Animate(): Animate = m_animate: End Property
	Public Property Let Animate(input): m_animate = input: End Property
  
	Public default Function init(primary, prim, sw, animate)
	  Set m_primary = primary
	  Set m_prim = prim
	  m_sw = sw
	  m_animate = animate
  
	  Set Init = Me
	End Function
  End Class

'Define a variable for each stand-up target
Dim ST31, ST32, ST33, ST34, ST37, ST38, ST39

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

Set ST31 = (new StandupTarget)(sw31, psw31,31, 0)
Set ST32 = (new StandupTarget)(sw32, psw32,32, 0)
Set ST33 = (new StandupTarget)(sw33, psw33,33, 0)
Set ST34 = (new StandupTarget)(sw34, psw34,34, 0)
Set ST37 = (new StandupTarget)(sw37, psw37,37, 0)
Set ST38 = (new StandupTarget)(sw38, psw38,38, 0)
Set ST39 = (new StandupTarget)(sw39, psw39,39, 0)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST31, ST32, ST33, ST34, ST37, ST38, ST39)

'Configure the behavior of Stand-up Targets
Const STAnimStep =  1.5 				'vpunits per animation step (control return to Start)
Const STMaxOffset = 9 			'max vp units target moves when hit

Const STMass = 0.4				'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

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
		prim.transy = STMaxOffset
		vpmTimer.PulseSw switch
		STAnimate = 2
		Exit Function
	elseif animate = 2 Then
		prim.transy = prim.transy - STAnimStep
		If prim.transy <= 0 Then
			prim.transy = 0
			primary.collidable = 1
			STAnimate = 0
			Exit Function
		Else 
			STAnimate = 2
		End If
	End If	
End Function

Sub STAction(Switch)
	Select Case Switch
		Case 11:
			Addscore 1000
			Flash1 True								'Demo of the flasher
			vpmTimer.AddTimer 150,"Flash1 False'"	'Disable the flash after short time, just like a ROM would do
		Case 12:
			Addscore 1000
			Flash2 True								'Demo of the flasher
			vpmTimer.AddTimer 150,"Flash2 False'"	'Disable the flash after short time, just like a ROM would do
		Case 13:
			Addscore 1000
			Flash3 True								'Demo of the flasher
			vpmTimer.AddTimer 150,"Flash3 False'"	'Disable the flash after short time, just like a ROM would do
	End Select
End Sub

'******************************************************
'		END STAND-UP TARGETS
'******************************************************

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************
Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / Table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

'******************************************************
'****  BALL ROLLING AND DROP SOUNDS
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
'	Dim b

	' stop the sound of deleted balls
'	For b = UBound(gBOT) + 1 to tnob - 1
		' Comment the next line if you are not implementing Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then BallShadowA(0).visible = 0
'		rolling(0) = False
'		StopSound("BallRoll_" & 0)
'	Next

	' exit the sub if no balls on the table
'	If UBound(gBOT) = -1 Then Exit Sub

	' play the rolling sound for each ball

'	For b = 0 to UBound(gBOT)
		If BallVel(rBall) > 1 AND rBall.z < 30 Then
			rolling(0) = True
			PlaySound ("BallRoll_" & 0), -1, VolPlayfieldRoll(rBall) * BallRollVolume * VolumeDial, AudioPan(rBall), 0, PitchPlayfieldRoll(rBall), 1, 0, AudioFade(rBall)
		Else
			If rolling(0) = True Then
				StopSound("BallRoll_" & 0)
				rolling(0) = False
			End If
		End If

		' Ball Drop Sounds
		If rBall.VelZ < -1 and rBall.z < 55 Then 'height adjust for ball drop sounds
			If DropCount(0) >= 5 Then
				DropCount(0) = 0
				If rBall.velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft rBall
				Else
					RandomSoundBallBouncePlayfieldHard rBall
				End If				
			End If
		End If
		If DropCount(0) < 5 Then
			DropCount(0) = DropCount(0) + 1
		End If

		' "Static" Ball Shadows
		' Comment the next If block, if you are not implementing the Dyanmic Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If rBall.Z > 30 Then
				BallShadowA(0).height=rBall.z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			Else
				BallShadowA(0).height=rBall.z - BallSize/2 + 5
			End If
			BallShadowA(0).Y = rBall.Y + Ballsize/5 + offsetY
			BallShadowA(0).X = rBall.X + offsetX
			BallShadowA(0).visible = 1
		End If
'	Next
End Sub
'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************


'******************************************************
'**** RAMP ROLLING SFX
'******************************************************

'Ball tracking ramp SFX 1.0
'   Reqirements:
'          * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'          * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'          * Create a Timer called RampRoll, that is enabled, with a interval of 100
'          * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'          * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'          * To stop tracking ball
'                 * call WireRampOff
'                 * Otherwise, the ball will auto remove if it's below 30 vp units
'

dim RampMinLoops : RampMinLoops = 4

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
'      Description:  
dim RampBalls(2,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(2)	

Sub Trigger1_Hit:WireRampOn False:bsRampOnWire:BIPL=True:End Sub		'Entering Plunge Wire
Sub Trigger1_unHit:BIPL=False:End Sub
Sub Trigger2_Hit:WireRampOff:bsRampOff ActiveBall.id:BIPL=False:End Sub			'Leaving Plunge Wire
Sub Trigger3_Hit:WireRampOn True:bsRampOn:End Sub				'Entering Robot Bridge
Sub Trigger4_Hit:WireRampOff:bsRampOff ActiveBall.id:RampExit.rotx=-30:End Sub	'Leaving Robot Bridge

Sub WireRampOn(input)  : Waddball ActiveBall, input : RampRollUpdate: End Sub
Sub WireRampOff() : WRemoveBall ActiveBall.ID	: End Sub


' WaddBall (Active Ball, Boolean)
'     Description: This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
Sub Waddball(input, RampInput)	'Add ball
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	dim x : for x = 1 to uBound(RampBalls)	'Check, don't add balls twice
		if RampBalls(x, 1) = input.id then 
			if Not IsEmpty(RampBalls(x,1) ) then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next

	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
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

' WRemoveBall (BallId)
'    Description: This subroutine is called from the RampRollUpdate subroutine 
'                 and is used to remove and stop the ball rolling sounds
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
					PlaySound("RampLoop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
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

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
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

'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************


'******************************************************
'****  FLEEP MECHANICAL SOUNDS
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
' Many places in the script need to be modified to include the correct sound effect subroutine calls. The tutorial videos linked below demonstrate 
' how to make these updates. But in summary the following needs to be updated:	
'	- Nudging, plunger, coin-in, start button sounds will be added to the keydown and keyup subs.
'	- Flipper sounds in the flipper solenoid subs. Flipper collision sounds in the flipper collide subs.
'	- Bumpers, slingshots, drain, ball release, knocker, spinner, and saucers in their respective subs
'	- Ball rolling sounds sub
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
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
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
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

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
' 					LUT
'******************************************************
dim lutsetsounddir
Dim LutToggleSoundLevel
LutToggleSoundLevel = 0.01



Sub SetLUT  'AXS
	Table1.ColorGradeImage = "LUT" & LUTset
end sub 

Sub LUTBox_Timer
	LUTBox.TimerEnabled = 0 
	LUTBox.Visible = 0
End Sub

Sub ShowLUT
	LUTBox.visible = 1
	Select Case LUTSet
        Case 0: LUTBox.text = "VPW original 1on1"
        Case 1: LUTBox.text = "bassgeige"
		Case 2: LUTBox.text = "Skitso Natural and Balanced"
		Case 3: LUTBox.text = "Skitso Natural High Contrast"
		Case 4: LUTBox.text = "HauntFreaks Desaturated"
  		Case 5: LUTBox.text = "Tomate washed out"
		Case 6: LUTBox.text = "B&W"
		Case 7: LUTBox.text = "Fleep Natural Dark 1"
		Case 8: LUTBox.text = "Fleep Natural Dark 2"
		Case 9: LUTBox.text = "CalleV Punchy Brightness and Contrast"
		Case 10: LUTBox.text = "3rdaxis Referenced THX Standard"
        Case 11: LUTBox.text = "blacklight"
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

	if LUTset = "" then LUTset = 0 'failsafe

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "ROBOTLUT.txt",True)
	ScoreFile.WriteLine LUTset
	Set ScoreFile=Nothing
	Set FileObj=Nothing
End Sub

Sub LoadLUT
	Dim FileObj, ScoreFile, TextStr
	dim rLine

	Set FileObj=CreateObject("Scripting.FileSystemObject")
	If Not FileObj.FolderExists(UserDirectory) then 
		LUTset=0
		Exit Sub
	End if
	If Not FileObj.FileExists(UserDirectory & "ROBOTLUT.txt") then
		LUTset=0
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "ROBOTLUT.txt")
	Set TextStr=ScoreFile.OpenAsTextStream(1,0)
		If (TextStr.AtEndOfStream=True) then
			Exit Sub
		End if
		rLine = TextStr.ReadLine
		If rLine = "" then
			LUTset=0
			Exit Sub
		End if
		LUTset = int (rLine) 
		Set ScoreFile = Nothing
	    Set FileObj = Nothing
End Sub


'	Also:	Implemented FastFlips(hacked), sorted the layers, formatted script, removed duplicate objects, fixed dt's, fixed assignments, made u-turns real,
'			made robots animate better and need front collision, re-origined and animated RampExit, made ramp walls into wall walls, probably lots of other stuff..."physics/lights/lighting"
'	0.9.1 Wylte	- Lowered flipper up angle, tied flippers to rom sound bytes so they properly shut off between games (thanks Apophis!)
'	0.9.2 Wylte - Apron work, bumper force decrease, sling force decrease, slope increased, flipper power increased, plastics real
'	0.9.3 Wylte - Flupper Bumpers (ish), Lampz, Lamps, GI, robot lighting, target meshes, leaf switch meshes, target lights, ramp walls -> wall walls, general cleanup
'	1.0   Wylte - Small tweak to arch geometry (skip the lanes less), lit up SPECIAL targets, extra light mods, adjusted u-turn, TargetBouncer on Robots, Release
'	1.1	 Wylte - Knocker position, GI on/off, SPECIAL lights off with GI, slings won't fire while firing, flipper friction 0.9->0.8, targetbouncer on robots up, new environment image, lamp blooms, ball shadow update
'	1.11 Gedankekojote97 - baked playfield
'	1.12 Wylte - Lighting tweaks, droptarget shadows
'	1.13 Wylte - Bumper and slingshots disabled with GI on game end or tilt
'	1.14 Wylte - Plunger trigger tweak, TargetBouncer removed from bottom sling posts, droptarget shadows improved, target animations fixed, experiment with alternate material on switch secondary collidable
'	1.15 Wylte - Lumi DT backdrop, target DL way up, lane feed to bumpers corrected, GI mod option, rollover stars given lights to simulate buttons (no model yet)
'	1.16 Lumi  - Backdrop light tuning
'	1.2  Wylte - Droptarget DL code, user options set, fixed plastic cutting through riser ramp, sounds cleaned up, images cleaned up, Apron/tunnel/ramp light up (thanks HauntFreaks), release