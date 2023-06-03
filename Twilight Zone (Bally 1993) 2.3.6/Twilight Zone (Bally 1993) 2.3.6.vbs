'  +              `s
' yNhssyyyydddhhyhhNo
'+/`      oMN      `+:                    ....`                  ```  ::  .//+/:
'         hMm        : `:/++.    ```.`    :mMd`    :ddo-    `/yyooooyhmm   +MM`  -sNNh/ `          `
'         hMN `/odmh:`   :NMy   .Nm:..     hMd`     yM/   `oNd.       -s+  /MM`    NM: `NhhyyhhssshN.
'         dMd    oMm`     hMM-  .N:`:hmh`  hMh      sM+  `dMm.          .  /MM`    NM. ss`  oMh    :+
'         NMd     dMh`   -MMMo `d+   mMs   hMs      sMo  yMM-           ```:MM+//++MM. `    yMy
'         mMd     `mMh   ds+MN`sy    mMy   hMs      oMo `NMm      -.:yNMN:`/MM/-.`.MM.      yMy
'         NMm      `hMy oh  NMdd     hMo   dMo      yM: `MMd         `NMd  /MM.   `MM`      hMs
'         MMd       `hMdN.  +Mm`     dMs   dMo     .ss+` dMN.         NMd  -Mm`   .MM`      mMo
'        `MMo        `dM:    d-      mMs   NMs   `.:/    .dMN/      `+Ns.  :MM`   -MM.      NM+
'        .MMy         `:            :MMs  :yyssyyhdm.      /ymmsooooo-    -dNd:` .+oo+/    `MM+`.
'        oMMy                       ..``                                                  ./+/:--
'      -+oo+/:-     ``
'                  -o.                  -
'                 /Nmdddddysysoooo+yNNNy.   `-+s+:-  .-//+/     -+sss:           +`
'                +s/:-.`         :dMMs.  `+ho:-:/yMNo`  /MMy      mM.   +hdysossyms
'               `              /dMm+`   oMy`      .dMm. `MMMm`    oN    .NM:      .:
'                           `oNMd/     yMh         `MMy `N+hMN:   +N    .MM-       `
'                         `oNMh:      `NM:          mMy  N+ oMMo  :M    `NMdhhhhdmmy
'                       .smMh-        -MM/         `NN.  m+  :NMh`-M`   `NM+``   `:
'                     :hMMy.           yMd-       `hy.   N+   .hMm:M.   `NM.
'                   :dMMs.          .+/ +dMdo:-:/oo-    `M/     oMMM-   `NM:       -
'                 :hMMd///++osyhhdddMy    .:++/-`       oNy+:.   -mM:   `MM- .-/+ym.
'                /o+/::--.`````     o                  -/-.`       s/  -ohysoo+/:/-

' Twilight Zone - IPDB No. 2684
' © Bally/Midway 1993

' Remastered by Skitso, rothbauerw and Bord. Fixed gumball mechanics not eating balls (thanks nFozzy), completely new gameplay and physics,
' mesh playfield, re-modeled ramps and other primitives, all new lighting and flashers, tons of visual polish and detailing.  Most sounds from
' Fleep's excellent sound packaged developed for TOM

' VPX recreation by ninuzzu
' This table started as a FP conversion by coindropper. Clark Kent continued improving it at graphics and physics level
' Then he asked me an help with lighting and other things and it evolved into something different.
' We have completely rebuild the table, so I would call it a "from scratch" build, rather than a conversion.
' Flupper and Tom Tower joined the team later and enhanced the visuals with some stunning models. You guys rock!
' This table is maybe the most modded ever, lots of options in the script. Lots of fun!

' Credits/Thanks
' coindropper early development
' nFozzy for scripting the trough, the lock, the magnets and a lot of other things
' JPSalas for the help with the inserts and the script (you know who is THE MAN!)
' Tom Tower for the mini clock, pyramid, piano, camera, gumballmachine models, wich I retextured.
' Hauntfreaks for the Mystic Seer Toy, wich I retextured
' Flupper for retexturing and remeshing the plastic ramp
' Zany for the domes, flippers and bumpers models
' rom for "Robbie the Robot" model, wich I edited and retextured
' knorr and pacdude for some sound effects I borrowed from their tables
'
' 2018-07-24 Thalamus - Added/Updated "Positional Sound Playback Functions" and "Supporting Ball & Sound Functions", Changed UseSolenoids=1 to 2, No special SSF tweaks yet.
' 2018-08-09 Wob - Added vpmInit Me to table init and cSingleLFlip
' 2018-08-18 DJRobX - Remove CSingleLFlip - there is an ULFlip sub, Add more SSF and VPMModSol code.
' 2018-10-09 nf  - rescripted gumball machine, slot machine kickout, clock sfx, added ballsearch on hard pinmame reset / find lost balls
' 2022-07-17 rothbauerw - updated physics to latest including adjusting the slot kickout, slings, adding stand up target code, flipper updates, dampener update, added VR plunger and a few other minor VR tweaks
' 2022-07-22 Niwak  - Complete bake/lightmap of the table, fix ramp height and small adjustmennts, interactive UI for the options
' 2022-07-25 leojreimrc - VR/FSS Backglass

Option Explicit
Randomize

Dim Romset, PowerballStart

'******************************************************************************************
'* TABLE OPTIONS **************************************************************************
'* Except options shown below, all other options are accessed by pressing both magna      *
'* during runtime                                                                         *
'******************************************************************************************
Romset = 1				' Choose the ROM: arcade rom with credits (tz_94ch),  1 = home rom (free play) (tz_94h)
PowerBallStart = 7		' Powerball Starting Location: 1-3 = gumball machine, 4-6 = trough, 0 = random, 7 = random gumball

'******************************************************************************************
'* END OF TABLE OPTIONS *******************************************************************
'******************************************************************************************
Const TableVersion = "2.3.4"		'Table version (shown in option UI)
Const BallSize = 50					'Ball size must be 50
Const BallMass = 1					'Ball mass must be 1
Const tnob = 5						'Total number of balls
Const lob = 0						'Locked balls
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's

Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height
Dim BIPL : BIPL = False				'Ball in plunger lane

Dim DesktopMode:DesktopMode = Table1.ShowDT
Dim UseVPMDMD:UseVPMDMD = 0' DesktopMode
Const UseVPMColoredDMD = true

If Table1.ShowFSS = True or DesktopMode = False Then
	ScoreText.visible = False
End If

TextBox.visible = False ' Used for debugging stuck balls

Const UseVPMModSol = 1

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Dim cGameName
Select Case Romset
	Case 0:	cGameName = "tz_94ch"
	Case 1:	cGameName = "tz_94h"
End Select

LoadVPM "02000000", "WPC.VBS", 3.49

'********************
'Standard definitions
'********************

Const UseSolenoids = 2
Const UseLamps = 0
Const UseGI = 0
Const UseSync = 1
Const HandleMech = 0

Const SSolenoidOn = "fx_solon"
Const SSolenoidOff = ""
Const SFlipperOn = ""
Const SFlipperOff = ""
Const SCoin = ""

'Set MotorCallback = GetRef("GameTimer")
Set GiCallback2 = GetRef("UpdateGI")


'*******************************************
'  Timers
'*******************************************

' The game timer interval is 10 ms
Sub GameTimer_timer()
	cor.Update 						'update ball tracking (this sometimes goes in the RDampen_Timer sub)
	RollingUpdate					'update rolling sounds
	DoSTAnim						'handle stand up target animations
End Sub

' The frame timer interval is -1, so executes at the display frame rate
Sub FrameTimer_Timer()
	FlipperVisualUpdate				'update flipper shadows and primitives
	MovableVisualUpdate				'update baked movable visual 
	BallBrightnessUpdate			'adjust ball brightness
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
	Options_UpdateDMD
End Sub


'************************************************************************
'						 INIT TABLE
'************************************************************************

Dim TZBall1, TZBall2, TZBall3, TZBall4, TZBall5, TZBall6, TZBalls
Dim bsSlot, bsAutoPlunger, bsRocket, mLeftMini, mRightMini, mslot, mLeftMagnet, mLowerRightMagnet, mUpperRightMagnet

Sub Table1_Init

	Options_Load
	UpdateMods

	vpmInit Me
	With Controller
		.GameName = cGameName
		.SplashInfoLine = "Twilight Zone (Bally 1992)"
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.Hidden = DesktopMode
		.HandleMechanics = 1' + 2 'Gumball + Clock	'just clock
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
	End With

	' Init switches
	Controller.Switch(22) = 1 'close coin door

    '************  Main Timer init  ********************

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

    '************   Nudging   **************************

	vpmNudge.TiltSwitch = 14
	vpmNudge.Sensitivity = 4
	vpmNudge.TiltObj = Array(Bumper1, Bumper2, Bumper3, Leftslingshot, Rightslingshot)

	'**************** Slot Machine Kickout ****************
	slotkick_vel = 40			'velocity 
	slotkick_vel_variance = 0.5	'velocity variance
	slotkick_angle = Loopy_New.objrotz	'adjust objrotz on loop mesh to adjust kickout direction
	slotkick_angle_variance = 1	'Angle variance

	'****************   Magnets   ******************

	Set mLeftMini = New cvpmMagnet
	With mLeftMini
		.InitMagnet TLMiniFlip, 60 		' Left Powerfield Real Magnet Strength (adjust to taste) - 70
		.GrabCenter = False: .Size = 195
		.CreateEvents "mLeftMini"
	End With

	Set mRightMini = New cvpmMagnet
	With mRightMini
		.InitMagnet TRMiniFlip, 60 		' Right Powerfield Real Magnet Strength (adjust to taste) - 70
		.GrabCenter = False: .Size = 195
		.CreateEvents "mRightMini"
	End With

	Set mLeftMagnet = New cvpmMagnet
	With mLeftMagnet
		.InitMagnet LeftMagnet, 67
		.CreateEvents "mLeftMagnet"
		.GrabCenter = True
	End With

	Set mUpperRightMagnet = New cvpmMagnet
	With mUpperRightMagnet
		.InitMagnet UpperRightMagnet, 63
		.CreateEvents "mUpperRightMagnet"
		.GrabCenter = True
	End With

	Set mLowerRightMagnet = New cvpmMagnet
	With mLowerRightMagnet
		.InitMagnet LowerRightMagnet, 63
		.CreateEvents "mLowerRightMagnet"
		.GrabCenter = True
	End With

	'Controller.Switch(55) = 0

	'****************   Init GI   ******************
	dim x

	UpdateGI 0, 1:UpdateGI 1, 1:UpdateGI 2, 1:UpdateGI 3, 1:UpdateGI 4, 1

	'for ballsearch
	 for each x in Array(sw84, sw85, sw88, sw58,sw18,sw25,sw16,sw15,sw17)	
		x.UserValue = cInt(mid(x.name, 3, 2))
	Next
	GumballPopper.uservalue = 74

	'************  Trough	**************************
	Set TZBall6 = sw17.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set TZBall5 = sw16.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set TZBall4 = sw15.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set TZBall3 = FreezeKicker2.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set TZBall2 = FreezeKicker1.CreateSizedballWithMass(Ballsize/2,Ballmass)
	Set TZBall1 = FreezeKicker0.CreateSizedballWithMass(Ballsize/2,Ballmass)
	TZBalls = Array(TZBall1, TZBall2, TZBall3, TZBall4, TZBall5, TZBall6)
	
	Controller.Switch(17) = 1
	Controller.Switch(16) = 1
	Controller.Switch(15) = 1
	Controller.Switch(26) = 1

'	Freezekicker2.kick 0,0,0
'	Freezekicker1.kick 0,0,0
'	Freezekicker0.kick 0,0,0

	If PowerballStart = 0 then PowerballStart = RndNumO(1, 6) end if	' 0 = completely random powerball start
	If PowerballStart = 7 then PowerballStart = RndNumO(1, 3) end if	' 7 = Random start in the gumball machine

	If powerballstart = 1 Then SetPowerBall TZBall1
	If powerballstart = 2 Then SetPowerBall TZBall2
	If powerballstart = 3 Then SetPowerBall TZBall3
	If powerballstart = 4 Then SetPowerBall TZBall4:Controller.switch(26) = 0
	If powerballstart = 5 Then SetPowerBall TZBall5
	If powerballstart = 6 Then SetPowerBall TZBall6
	
	'SpawnBalls	'spawn all balls in gumball machine and in trough

	'****************   Init flashers   ******************

	SetModLamp 117, 0
	SetModLamp 118, 0
	SetModLamp 119, 0
	SetModLamp 120, 0
	SetModLamp 128, 0
	SetModLamp 137, 0
	SetModLamp 138, 0
	SetModLamp 139, 0
	SetModLamp 140, 0
	SetModLamp 141, 0

	'************  FSS & VR	Backglass **************************

	If RenderingMode = 2 Then ' Enable VR stuff if run in VR mode
		
		For each x in VRStuff
			x.visible = True
		Next
		For each x in VRbackglass
			x.visible = True
		Next
		Setbackglass
		BGTimer.enabled = true : BGTimer2.enabled = true : BGTimer3.enabled = true
	ElseIf Table1.ShowFSS Then ' Enable FSS
		Setbackglass
		For each x in VRStuff
			x.visible = True
		Next
	Else
		for each x in FSS: x.visible = false: Next
		BGTimer.enabled = false : BGTimer2.enabled = false : BGTimer3.enabled = false
	End If
End Sub



'******************************************************
' 						KEYS
'******************************************************

Sub Table1_KeyDown(ByVal keycode)
	If keycode = LeftFlipperKey Then 
		FlipperActivate LeftFlipper, LFPress
		If StagedFlipperMod <> 1 Then FlipperActivate2 LeftFlipper1, LFPress1
	End If
	If keycode = RightFlipperKey Then 
		FlipperActivate RightFlipper, RFPress
		If StagedFlipperMod <> 1 Then FlipperActivate RightFlipper1, RFPress1
	End If
	If StagedFlipperMod = 1 Then
		If keycode = KeyUpperLeft Then FlipperActivate2 LeftFlipper1, LFPress1
		If keycode = KeyUpperRight Then FlipperActivate RightFlipper1, RFPress1
	End If

	If bInOptions Then
		Options_KeyDown keycode
		Exit Sub
	End If
    If keycode = LeftMagnaSave Then
		If bOptionsMagna Then Options_Open() Else bOptionsMagna = True
    ElseIf keycode = RightMagnaSave Then
		If bOptionsMagna Then Options_Open() Else bOptionsMagna = True
	End If

	If keycode = LeftTiltKey Then Nudge 90, 2:SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 2:SoundNudgeRight()
	If keycode = CenterTiltKey Then Nudge 0, 3:SoundNudgeCenter()
	If keycode = MechanicalTilt Then SoundNudgeCenter() : Controller.Switch(14) = 1
	If Keycode = KeyFront Then Controller.Switch(23) = 1 ' Buy in

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySoundAtLevelStatic ("Coin_In_1"), CoinSoundLevel, sw18
			Case 1: PlaySoundAtLevelStatic ("Coin_In_2"), CoinSoundLevel, sw18
			Case 2: PlaySoundAtLevelStatic ("Coin_In_3"), CoinSoundLevel, sw18
		End Select
	End If

	If KeyCode = PlungerKey Then Plunger.Pullback:SoundPlungerPull()
	If vpmKeyDown(KeyCode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If keycode = LeftFlipperKey Then
		FlipperDeActivate LeftFlipper, LFPress
		If StagedFlipperMod <> 1 Then
			FlipperDeActivate2 LeftFlipper1, LFPress1
		End If
	End If
	If keycode = RightFlipperKey Then 
		FlipperDeActivate RightFlipper, RFPress
		If StagedFlipperMod <> 1 Then
			FlipperDeActivate RightFlipper1, RFPress1		
		End If
	End If

	If StagedFlipperMod = 1 Then
		If keycode = KeyUpperLeft Then 
			FlipperDeActivate2 LeftFlipper1, LFPress1
		End If
		If keycode = KeyUpperRight Then 
			FlipperDeActivate RightFlipper1, RFPress1
		End If
	End If

    If keycode = LeftMagnaSave And Not bInOptions Then bOptionsMagna = False
    If keycode = RightMagnaSave And Not bInOptions Then bOptionsMagna = False

	If keycode = MechanicalTilt Then Controller.Switch(14) = 0
	If KeyCode = KeyFront Then Controller.Switch(23) = 0
	
	If KeyCode = PlungerKey Then
		Plunger.Fire
		If BIPL Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
	End If
	If vpmKeyUp(KeyCode) Then Exit Sub
End Sub

'************************************************
'*********   BallInit ***************************
'************************************************

Dim PowerBall, PowerBallID

Sub BallSearch() 'on hard pinmame reset check all these triggers and kickers
	dim x : for each x in Array(GumballPopper, sw84, sw85, sw88, sw58,sw18,sw25,sw16,sw15,sw17)
		if x.ballcntover then controller.Switch(x.uservalue) = True
	Next
	for each x in TZBalls : if x.y > 2500 then x.x = 204 : x.y = 519 : x.velx = 0 : x.vely = 0 : x.z = -30 : end if

	if sw15.ballcntover then 	'Fix the trough powerball detecting switch
		if sw15.lastcapturedball.id <> PowerBallID then controller.Switch(26) = 1
	end if

: Next	'reset balls that have fallen off the table
End Sub

Sub SetPowerBall(ball)
'	if IsObject(PowerballLocation) then 
'		Set PowerBall = PowerBallLocation.CreateSizedBall(Ballsize/2)
'	End If

	With ball
		.image = "powerball"
		.color = RGB(255,255,255)
		'.id = 666
		.Mass = 0.8*Ballmass
		.BulbIntensityScale = 0.05
	End With

	PowerBallID = ball.id
	Set PowerBall = ball
End Sub

sub ballupdate_timer()

	textbox.text = TZBall1.x & " " & TZBall1.y & " " & TZBall1.z  & vbnewline & _
		TZBall2.x & " " & TZBall2.y & " " & TZBall2.z  & vbnewline & _
		TZBall3.x & " " & TZBall3.y & " " & TZBall3.z  & vbnewline & _
		TZBall4.x & " " & TZBall4.y & " " & TZBall4.z  & vbnewline & _
		TZBall5.x & " " & TZBall5.y & " " & TZBall5.z  & vbnewline & _
		TZBall6.x & " " & TZBall6.y & " " & TZBall6.z 

end sub

'**************************************************************
' SOLENOIDS
'**************************************************************

'	  (*) - only in prototype, supported by rom 9.4
'	 (**) - the additional GUM and BALL flashers were removed ro reduce cost
'	(***) - Gumball and Clock Mechanics are handled by vpm classes

'standard coils
SolCallback(1) = "SlotMachineKickout"									'(01) Slot Kickout
SolCallback(2) = "SolRocket"												'(02) Rocket Kicker
SolCallback(3) = "SolAutoKicker"											'(03) Auto-Fire Kicker
SolCallback(4) = "SolGumballPopper"										'(04) Gumball Popper
SolCallback(5) = "SolRightRampDiverter"									'(05) Right Ramp Diverter
SolCallback(6) = "SolGumballDiverter"									'(06) Gumball Diverter
SolCallback(7) = "SolKnocker"											'(07) Knocker
SolCallback(8) = "SolOuthole"											'(08) Outhole
SolCallback(9) = "SolBallRelease"										'(09) Ball Release
'SolCallback(10) = "SolRightSling"										'(10) Right Slingshot
'SolCallback(11) = "SolLeftSling"										'(11) Left Slingshot
'SolCallback(12) = "SolLowerBumper"										'(12) Lower Jet Bumper
'SolCallback(13) = "SolLeftBumper"										'(13) Left Jet Bumper
'SolCallback(14) = "SolRightBumper"										'(14) Right Jet Bumper
SolCallback(15) = "LockKickout"											'(15) Lock Release nf
SolCallback(16) = "SolShootDiverter"									'(16) Shooter Diverter
SolModCallback(17) = "SetModLamp 117,"										'(17) Flasher bumpers x2	
SolModCallback(18) = "SetModLamp 118,"										'(18) Flasher Power Payoff x2
SolModCallback(19) = "SetModLamp 119,"										'(19) Flasher Mini-Playfield x2
SolModCallback(20) = "SetModLamp 120,"										'(20) Flasher Upper Left Ramp x2 (**)
SolCallback(21) = "SolLeftMagnet"										'(21) Left Magnet
SolCallback(22) = "SolUpperRightMagnet"									'(22) Upper Right Magnet (*)
SolCallback(23) = "SolLowerRightMagnet"									'(23) Lower Right Magnet
SolCallback(24) = "SolGumballMotor"										'(24) Gumball Motor
SolCallback(25) = "SolMiniMagnet mLeftMini,"							'(25) Left Mini-Playfield Magnet
SolCallback(26) = "SolMiniMagnet mRightMini,"							'(26) Right Mini-Playfield Magnet
SolCallback(27) = "SolLeftRampDiverter"									'(27) Left Ramp Diverter
SolModCallback(28) = "SetModLamp 128,"									'(28) Flasher Inside Ramp
'aux board coils
SolModCallback(51) = "SetModLamp 137,"										'(37) Flasher Upper Right Flipper
SolModCallback(52) = "SetModLamp 138,"										'(38) Flasher Gumball Machine Higher
SolModCallback(53) = "SetModLamp 139,"										'(39) Flasher Gumball Machine Middle
SolModCallback(54) = "SetModLamp 140,"										'(40) Flasher Gumball Machine Lower
SolModCallback(55) = "SetModLamp 141,"										'(41) Flasher Upper Right Ramp x2 (**)
'SolCallback(56) = ""													'(42) Clock Reverse (***)
'SolCallback(57) = ""													'(43) Clock Forward (***)
'SolCallback(58) = ""													'(44) Clock Switch Strobe (***)
'SolCallback(59) = "SolGumRelease"										'(??) Gumball Release (***)	'pinmame hack unreliable with solmodcallbacks
'fliptronic board
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
SolCallback(sURFlipper) = "SolURFlipper"
SolCallback(sULFlipper) = "SolULFlipper"

'******************************************************
'					KNOCKER
'******************************************************
Sub SolKnocker(Enabled)
	If enabled Then
		KnockerSolenoid
	End If
End Sub


'***********   Rocket   ********************************

Sub RocketKicker_Hit
	PlaysoundAtBallVol "fx_power", 0.5
	Controller.Switch(28) = 1
End Sub

Sub SolRocket(Enabled)
	If enabled Then 
		If RocketKicker.BallCntOver = 0 Then
			PlaySoundAt SoundFX(SSolenoidOn,DOFContactors), RocketKicker
		Else
			PlaySoundAtVol SoundFX("fx_rocket_exit",DOFContactors), 1, RocketKicker
		End If
		RocketKicker.kick 302 + (Rnd*6), 45 + (Rnd* 20)
		Controller.Switch(28) = 0
	End If
End Sub


'***********   Autoplunger   ********************************

Sub AutoPlungerKicker_Hit
	Controller.Switch(72) = 1
	PlaysoundAt "fx_Lock_enter", AutoPlungerKicker
End Sub

Sub SolAutoKicker(Enabled)
	If enabled Then 
		If AutoPlungerKicker.BallCntOver = 0 Then
			PlaySoundAt SoundFX("fx_AutoPlunger",DOFContactors), AutoPlungerKicker
		Else
			PlaySoundAtVol SoundFX("fx_launch",DOFContactors), 0.5, AutoPlungerKicker
		End If
		AutoPlungerKicker.kick 0, 52 + (Rnd* 16)
		Controller.Switch(72) = 0
	End If
End Sub

'***********   Gumball Popper   ******************************

Dim BIK:BIK=0		'ball in kicker

sub GumballPopper_Hit()
	Controller.Switch(74) = 1
	BIK=BIK+1
	PlaySoundAtBall "fx_kicker_catch"
end sub

sub GumballPopper_UnHit()
	Controller.Switch(74) = 0
	BIK=BIK-1
end sub

Sub GumballPopperHole_Hit
	PlaySoundAtVol "fx_Hole",0.5, GumballPopperHole
	vpmTimer.PulseSw 51
End Sub

Sub SolGumballPopper(enabled)	'VUK
    If enabled Then
		BallSearch
		GumballPopper.Kick 0, 65, 1.5
        If BIK = 0 Then
            PlaySoundAt SoundFX(SSolenoidOn,DOFContactors), GumballPopper
        Else
            PlaySoundAt SoundFX("fx_GumPop",DOFContactors), GumballPopper
        End If
    End If
End Sub

'**************   Drain  and Release  ************************************

Dim BIP:BIP = 0				'Balls In Play

sub sw18_hit()
	RandomSoundDrain sw18
	Controller.Switch(18) = 1
	BIP = BIP - 1
	If TVMod = 1 then TVTimer.enabled = 0:Frame.imageA = "tv_gameover"
end sub

sub sw18_unhit():controller.Switch(18) = 0:end sub

Sub SolOuthole(enabled)
	If Enabled Then
		'BallSearch
		sw18.kick 60, 9
		Updatetrough
		Playsoundat SoundFX(SSolenoidOn,DOFContactors), sw18
	End If
end sub

Sub SolBallrelease(enabled)
	If Enabled Then
		sw15.kick 60, 9
		Updatetrough					'this is important to reset trough intervals
		BIP = BIP + 1
	End If
End sub

'******************************************************
'						TROUGH 
'******************************************************

sub sw25_hit():controller.Switch(25) = 1:updatetrough:end sub
sub sw25_unhit():controller.Switch(25) = 0:updatetrough:end sub

sub sw17_hit():controller.Switch(17) = 1:updatetrough:end sub
sub sw17_unhit():controller.Switch(17) = 0:updatetrough:end sub

sub sw16_hit():controller.Switch(16) = 1:updatetrough:end sub
sub sw16_unhit():controller.Switch(16) = 0:updatetrough:end sub

sub sw15_hit()
	if activeball.id = PowerBallID then 	'opto handler
		controller.Switch(26) = 0	'if powerball
	Else	
		controller.Switch(26) = 1	'if regular ball
	end if
	controller.Switch(15) = 1
	updatetrough:
end sub
sub sw15_unhit()
	controller.Switch(15) = 0
	controller.switch(26) = 0
	RandomSoundBallRelease sw15
	updatetrough
end sub

sub Updatetrough()
	updatetroughTimer.interval = 300
	updatetroughTimer.enabled = 1
end sub

sub updatetroughTimer_timer()
	if sw15.BallCntOver = 0 then sw16.kick 58, 8 end If
	if sw16.BallCntOver = 0 then sw17.kick 58, 8 end If
	if sw17.BallCntOver = 0 then sw25.Kick 58, 8 end If
	me.enabled = 0
end sub

'*******************   Lock   ******************

sub sw85_hit():controller.Switch(85) = 1:PlaysoundAt "fx_Lock_enter", sw85:updatelock:end sub
sub sw85_unhit():controller.Switch(85) = 0:end sub
sub sw84_hit():controller.Switch(84) = 1:updatelock:end sub
sub sw84_unhit():controller.Switch(84) = 0:end sub
sub sw88_hit():controller.Switch(88) = 1:updatelock:end sub
sub sw88_unhit():controller.Switch(88) = 0:end sub

sub lockramp_hit
	PlaySoundAtBallVolME "fx_metal_ramp_hit", 0.5
end sub

sub updatelock
	updatelocktimer.interval = 32
	updatelocktimer.enabled = 1
end sub

sub updatelocktimer_timer()
	if sw88.BallCntOver = 0 then sw84.kick 180, 2 end If
	if sw84.BallCntOver = 0 then sw85.kick 180, 2 end if
	me.enabled = 0
end sub

sub LockKickout(enabled)
	If enabled then
	sw88.kick 88, 10
	Playsoundat SoundFX("fx_Lock_exit",DOFContactors), sw88
	updatelock
	End If
end sub

'******************************************************************
'** SUBWAY, SHOOTER LANE, SLOTMACHINE; CAMERA, PIANO, DEAD END  ***
'******************************************************************

Sub SubwaySound(dummy)
	PlaySoundat "fx_subway", sw57
End sub

'********	Slot Machine	****************************************

Sub SlotMachine_Hit()
    PlaySoundat "fx_SlotM_enter", slotMachine
End Sub

Sub sw57_Hit()											'submarine switch, Tslot proximity
	'debug.print activeball.id  & " " &  powerballid
	if activeball.id <> PowerBallID then
		vpmTimer.PulseSw 57
	end if	
end Sub

Sub Sw58_Hit() 
	Controller.Switch(58) = 1
	SlotKickerOverflow.Enabled = True
	Playsoundat "fx_kicker_catch", sw58
End Sub

Sub Sw58_UnHit() 
	Controller.Switch(58) = 0
	SlotKickerOverflow.Enabled = False
End Sub

dim slotkick_vel, slotkick_vel_variance
dim slotkick_angle, slotkick_angle_variance

sub slotmachinekickout(enabled)
	if enabled Then
		Playsoundat SoundFX("fx_SlotM_exit",DOFContactors), sw58
		'If SlotKickerOverflow.ballcntover > 0 Then
			SlotKickerOverflow.Kick KickoutVariance(slotkick_angle,slotkick_angle_variance), KickoutVariance(slotkick_vel, slotkick_vel_variance)
		'else
			sw58.Kick KickoutVariance(slotkick_angle,slotkick_angle_variance), KickoutVariance(slotkick_vel, slotkick_vel_variance)
		'end if
	end if
end sub

Function KickoutVariance(aNumber, aVariance)	'strength, variance
	KickoutVariance = aNumber + ((Rnd*2)-1)*aVariance
End Function





'********	Shooter Lane	***************************************

Sub ShooterLaneKicker_Hit
    PlaySoundAtVol "fx_hole", 0.5,  ShooterLaneKicker
	vpmtimer.addtimer 100, "SubwaySound"
End Sub

'********   Dead End   ********************************************

Sub DeadEnd_Hit
    vpmTimer.PulseSw 41
	PlaySoundAtVol "fx_DeadEnd", 0.5, DeadEnd
	vpmtimer.addtimer 100, "SubwaySound"
End Sub

'********   Camera   ***********************************************

Sub CameraKicker_Hit
	PlaySoundAtVol "fx_hole", 0.5, CameraKicker
	vpmtimer.addtimer 100, "SubwaySound"
End Sub

Sub sw42_Hit():vpmTimer.PulseSw 42:end Sub		'submarine switch, camera / upper playfield

Sub Hitch001_hit():PlaySoundAtBallVol "fx_lr2", 0.5:End Sub
Sub Hitch002_hit():PlaySoundAtBallVol "fx_lr3", 0.5:End Sub
Sub Hitch003_hit():PlaySoundAtBallVol "fx_lr4", 0.5:End Sub
Sub Hitch004_hit():If activeball.vely < 0 then:PlaySoundAtBallVol "fx_lr5", 0.5:End If:End Sub

'********  Piano   *************************************************

Sub Piano_Hit()
    PlaySoundAtVol "fx_Piano", 0.5, Piano
	vpmtimer.addtimer 100, "SubwaySound"
End Sub

Sub sw43_Hit():vpmTimer.PulseSw 43:end Sub		'submarine switch, piano

''************************************************************************************
''*****************       SLINGSHOTS                      ****************************
''************************************************************************************

Dim LStep: LStep = 4: Update_LSling
Dim RStep: RStep = 4: Update_RSling

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(ActiveBall)
	RandomSoundSlingshotLeft SLING2_BM_Dark_Room
	LStep = 0
	Update_LSling
	Me.TimerEnabled = 1
	vpmTimer.PulseSw 34
End Sub

Sub LeftSlingShot_Timer
	Update_LSling
	If LStep = 4 Then Me.TimerEnabled = 0
    LStep = LStep + 1
End Sub

Sub Update_LSling
	Dim x1, x2, y
	If LStep <= 2 Then
		x1 = True : x2 = False : y = 20
	ElseIf LStep = 3 Then
		x1 = False : x2 = True : y = 10
	Else
		x1 = False : x2 = False : y = 0
	End If
	LSling1_BM_Dark_Room.Visible = x1 ' VLM.Props;BM;1;LSling1
	LSling1_LM_Lit_Room.Visible = x1 ' VLM.Props;LM;1;LSling1
	LSling1_LM_GI_Left.Visible = x1 ' VLM.Props;LM;1;LSling1
	LSling1_LM_GI_Right.Visible = x1 ' VLM.Props;LM;1;LSling1
	LSling1_LM_Flashers_f17.Visible = x1 ' VLM.Props;LM;1;LSling1
	LSling1_LM_Flashers_f18.Visible = x1 ' VLM.Props;LM;1;LSling1
	LSling1_LM_Inserts_l13.Visible = x1 ' VLM.Props;LM;1;LSling1
	LSling1_LM_Inserts_l41.Visible = x1 ' VLM.Props;LM;1;LSling1
	LSling1_LM_Inserts_l42.Visible = x1 ' VLM.Props;LM;1;LSling1
	LSling1_LM_Inserts_l43.Visible = x1 ' VLM.Props;LM;1;LSling1
	LSling1_LM_Inserts_l44.Visible = x1 ' VLM.Props;LM;1;LSling1
	LSling1_LM_Inserts_l45.Visible = x1 ' VLM.Props;LM;1;LSling1
	LSling2_BM_Dark_Room.Visible = x2 ' VLM.Props;BM;1;LSling2
	LSling2_LM_Lit_Room.Visible = x2 ' VLM.Props;LM;1;LSling2
	LSling2_LM_GI_Left.Visible = x2 ' VLM.Props;LM;1;LSling2
	LSling2_LM_GI_Right.Visible = x2 ' VLM.Props;LM;1;LSling2
	LSling2_LM_Flashers_f18.Visible = x2 ' VLM.Props;LM;1;LSling2
	LSling2_LM_Inserts_l42.Visible = x2 ' VLM.Props;LM;1;LSling2
	LSling2_LM_Inserts_l43.Visible = x2 ' VLM.Props;LM;1;LSling2
	LSling2_LM_Inserts_l44.Visible = x2 ' VLM.Props;LM;1;LSling2
	SLING2_BM_Dark_Room.transy = y ' VLM.Props;BM;1;SLING2
	SLING2_LM_Lit_Room.transy = y ' VLM.Props;LM;1;SLING2
	SLING2_LM_GI_Left.transy = y ' VLM.Props;LM;1;SLING2
End Sub

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(ActiveBall)
	RandomSoundSlingshotRight SLING1_BM_Dark_Room
	RStep = 0
	Update_RSling
	Me.TimerEnabled = 1
	vpmTimer.PulseSw 35
End Sub

Sub RightSlingShot_Timer
	Update_RSling
	If RStep = 4 Then Me.TimerEnabled = 0
    RStep = RStep + 1
End Sub

Sub Update_RSling
	Dim x1, x2, y
	If RStep <= 2 Then
		x1 = True : x2 = False : y = 20
	ElseIf RStep = 3 Then
		x1 = False : x2 = True : y = 10
	Else
		x1 = False : x2 = False : y = 0
	End If
	RSling1_BM_Dark_Room.Visible = x1 ' VLM.Props;BM;1;RSling1
	RSling1_LM_Lit_Room.Visible = x1 ' VLM.Props;LM;1;RSling1
	RSling1_LM_GI_Left.Visible = x1 ' VLM.Props;LM;1;RSling1
	RSling1_LM_GI_Right.Visible = x1 ' VLM.Props;LM;1;RSling1
	RSling1_LM_Mod_l111.Visible = x1 ' VLM.Props;LM;1;RSling1
	RSling1_LM_Flashers_f17.Visible = x1 ' VLM.Props;LM;1;RSling1
	RSling1_LM_Inserts_l23.Visible = x1 ' VLM.Props;LM;1;RSling1
	RSling1_LM_Inserts_l43.Visible = x1 ' VLM.Props;LM;1;RSling1
	RSling1_LM_Inserts_l44.Visible = x1 ' VLM.Props;LM;1;RSling1
	RSling1_LM_Inserts_l45.Visible = x1 ' VLM.Props;LM;1;RSling1
	RSling2_BM_Dark_Room.Visible = x2 ' VLM.Props;BM;1;RSling2
	RSling2_LM_Lit_Room.Visible = x2 ' VLM.Props;LM;1;RSling2
	RSling2_LM_GI_Left.Visible = x2 ' VLM.Props;LM;1;RSling2
	RSling2_LM_GI_Right.Visible = x2 ' VLM.Props;LM;1;RSling2
	RSling2_LM_Mod_l111.Visible = x2 ' VLM.Props;LM;1;RSling2
	RSling2_LM_Flashers_f17.Visible = x2 ' VLM.Props;LM;1;RSling2
	RSling2_LM_Inserts_l43.Visible = x2 ' VLM.Props;LM;1;RSling2
	RSling2_LM_Inserts_l44.Visible = x2 ' VLM.Props;LM;1;RSling2
	RSling2_LM_Inserts_l45.Visible = x2 ' VLM.Props;LM;1;RSling2
	SLING1_BM_Dark_Room.transy = y ' VLM.Props;BM;1;SLING1
	SLING1_LM_Lit_Room.transy = y ' VLM.Props;LM;1;SLING1
	SLING1_LM_GI_Left.transy = y ' VLM.Props;LM;1;SLING1
	SLING1_LM_GI_Right.transy = y ' VLM.Props;LM;1;SLING1
	SLING1_LM_Mod_l111.transy = y ' VLM.Props;LM;1;SLING1
End Sub


'******************************************************
'  SLINGSHOT CORRECTION FUNCTIONS
'******************************************************
' To add these slingshot corrections:
' 	- On the table, add the endpoint primitives that define the two ends of the Slingshot
'	- Initialize the SlingshotCorrection objects in InitSlingCorrection
' 	- Call the .VelocityCorrect methods from the respective _Slingshot event sub


dim LS : Set LS = New SlingshotCorrection
dim RS : Set RS = New SlingshotCorrection

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

'' The following sub are needed, however they may exist somewhere else in the script. Uncomment below if needed
'Dim PI: PI = 4*Atn(1)
'Function dSin(degrees)
'	dsin = sin(degrees * Pi/180)
'End Function
'Function dCos(degrees)
'	dcos = cos(degrees * Pi/180)
'End Function

Function RotPoint(x,y,angle)
    dim rx, ry
    rx = x*dCos(angle) - y*dSin(angle)
    ry = x*dSin(angle) + y*dCos(angle)
    RotPoint = Array(rx,ry)
End Function

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

''************************************************************************************
''*****************               Bumpers                 ****************************
''************************************************************************************

Dim bump1, bump2, bump3

Sub Bumper1_Hit
	vpmTimer.PulseSw 31
	RandomSoundBumper Bumper1
	bump1 = 1:Me.TimerEnabled = 1
End Sub

Sub Bumper1_Timer()
	Dim z
    Select Case bump1
        Case 1:z = 15:bump1 = 2
        Case 2:z = 25:bump1 = 3
        Case 3:z = 35:bump1 = 4
        Case 4:z = 45:Me.TimerEnabled = 0
    End Select
	z = z - 41.5
	BR1_BM_Dark_Room.Z = z ' VLM.Props;BM;1;BR1
	BR1_LM_Lit_Room.Z = z ' VLM.Props;LM;1;BR1
	BR1_LM_GI_Left.Z = z ' VLM.Props;LM;1;BR1
	BR1_LM_GI_Right.Z = z ' VLM.Props;LM;1;BR1
	BR1_LM_Flashers_f17.Z = z ' VLM.Props;LM;1;BR1
	BR1_LM_Bumpers_l61.Z = z ' VLM.Props;LM;1;BR1
End Sub

Sub Bumper2_Hit
	vpmTimer.PulseSw 32
	RandomSoundBumper Bumper2
	bump2 = 1:Me.TimerEnabled = 1
End Sub

Sub Bumper2_Timer()
	Dim z
    Select Case bump2
        Case 1:z = 15:bump2 = 2
        Case 2:z = 25:bump2 = 3
        Case 3:z = 35:bump2 = 4
        Case 4:z = 45:Me.TimerEnabled = 0
    End Select
	z = z - 41.5
	BR2_BM_Dark_Room.Z = z ' VLM.Props;BM;1;BR2
	BR2_LM_Mod_l110.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_Lit_Room.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_GI_Left.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_GI_Right.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_Mod_l109.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_Flashers_f17.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_Flashers_f18.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_Inserts_l15.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_Inserts_l16.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_Inserts_l17.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_Inserts_l18.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_Inserts_l27.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_Inserts_l37.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_Inserts_l38.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_Inserts_l41.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_Inserts_l42.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_Inserts_l43.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_Inserts_l44.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_Bumpers_l63.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_Inserts_l64.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_Inserts_l65.Z = z ' VLM.Props;LM;1;BR2
	BR2_LM_Inserts_l71.Z = z ' VLM.Props;LM;1;BR2
End Sub

Sub Bumper3_Hit
	vpmTimer.PulseSw 33
	RandomSoundBumper Bumper3
	bump3 = 1:Me.TimerEnabled = 1
End Sub

Sub Bumper3_Timer()
	Dim z
    Select Case bump3
        Case 1:z = 15:bump3 = 2
        Case 2:z = 25:bump3 = 3
        Case 3:z = 35:bump3 = 4
        Case 4:z = 45:Me.TimerEnabled = 0
    End Select
	z = z - 41.5
	BR3_BM_Dark_Room.Z = z ' VLM.Props;BM;1;BR3
	BR3_LM_Mod_l110.Z = z ' VLM.Props;LM;1;BR3
	BR3_LM_Lit_Room.Z = z ' VLM.Props;LM;1;BR3
	BR3_LM_GI_Left.Z = z ' VLM.Props;LM;1;BR3
	BR3_LM_GI_Right.Z = z ' VLM.Props;LM;1;BR3
	BR3_LM_Mod_l109.Z = z ' VLM.Props;LM;1;BR3
	BR3_LM_Flashers_f17.Z = z ' VLM.Props;LM;1;BR3
	BR3_LM_Flashers_f18.Z = z ' VLM.Props;LM;1;BR3
	BR3_LM_Inserts_l33.Z = z ' VLM.Props;LM;1;BR3
	BR3_LM_Inserts_l35.Z = z ' VLM.Props;LM;1;BR3
	BR3_LM_Inserts_l37.Z = z ' VLM.Props;LM;1;BR3
	BR3_LM_Inserts_l38.Z = z ' VLM.Props;LM;1;BR3
	BR3_LM_Bumpers_l62.Z = z ' VLM.Props;LM;1;BR3
	BR3_LM_Inserts_l64.Z = z ' VLM.Props;LM;1;BR3
End Sub

'*******************************************************************
'****************   Diverters   ************************************
'*******************************************************************

Sub solShootDiverter(Enabled)
    If Enabled Then
        shooterdiverter.rotatetoend : PlaysoundAt SoundFX("fx_DivSS",DOFContactors), ShooterDiverter
    Else
        shooterdiverter.rotatetostart
	End If
End Sub

Sub SolGumballDiverter (enabled)
    If Enabled Then
		GumballDiverter.rotatetoend : PlaysoundAt SoundFX("fx_DivGM",DOFContactors), GumballDiverter
	Else
		GumballDiverter.rotatetostart
	End If
End Sub

'********  Left Ramp Diverter   **********************

Sub SolLeftRampDiverter(enabled)
	If Enabled Then
		PlaysoundAt SoundFX("fx_DivLR",DOFContactors), Plunger1
		RampDivWall.IsDropped=1
		RampDiverter.RotatetoEnd
		Plunger1.pullback
	Else
		RampDivWall.IsDropped=0
		RampDiverter.Rotatetostart
		Plunger1.fire
	End If
End Sub

'********  Right Ramp Diverter   **********************

Dim divDir, divPos
divPos = 0

Dim KickerBall:Kickerball = Empty

Sub divWall_Hit()
	WireRampOff
	'StopSound "fx_metalrolling"
	PlaySoundAtBallVol "fx_metalHit", 0.1
	If activeball.velx > 6 then activeball.velx = 6
End Sub

Sub divTrig_Hit()
	Set KickerBall = Activeball
End Sub

Sub divTrig_unHit()
	KickerBall = Empty
End Sub

Sub SolRightRampDiverter(enabled)
	If enabled Then
		Playsoundat SoundFX("fx_DivRR",DOFContactors), DivTrig
		if Not IsEmpty(Kickerball) Then
			Kickball Kickerball, -10, 10, 0, 50
		End If
		divDir = 9
		Kickerball = Empty
	Else
		divDir = -9
	End If
	DiverterTimer.Enabled = 1
	DivWall.collidable = not enabled
End Sub

Sub diverterTimer_Timer()
    divPos = divPos + divDir
    If divPos > 90 Then
        divPos = 90
        DiverterTimer.Enabled = 0
    End If
    If divPos < 0 Then
        divPos = 0
        diverterTimer.Enabled = 0
    End If
	
	Dim a: a = -divPos
	RDiv_BM_Dark_Room.RotX = a ' VLM.Props;BM;1;RDiv
	RDiv_LM_Lit_Room.RotX = a ' VLM.Props;LM;1;RDiv
	RDiv_LM_GI_Left.RotX = a ' VLM.Props;LM;1;RDiv
	RDiv_LM_Flashers_f20.RotX = a ' VLM.Props;LM;1;RDiv
	RDiv_LM_Flashers_f28.RotX = a ' VLM.Props;LM;1;RDiv
	RDiv_LM_Flashers_f40.RotX = a ' VLM.Props;LM;1;RDiv
	RDiv_LM_Flashers_f41.RotX = a ' VLM.Props;LM;1;RDiv
	RDiv_LM_Inserts_l54.RotX = a ' VLM.Props;LM;1;RDiv

	SpiralToy_BM_Dark_Room.RotX = a ' VLM.Props;BM;1;SpiralToy
	SpiralToy_LM_Lit_Room.RotX = a ' VLM.Props;LM;1;SpiralToy
	SpiralToy_LM_GI_Left.RotX = a ' VLM.Props;LM;1;SpiralToy
	SpiralToy_LM_Flashers_f18.RotX = a ' VLM.Props;LM;1;SpiralToy
	SpiralToy_LM_Flashers_f20.RotX = a ' VLM.Props;LM;1;SpiralToy
	SpiralToy_LM_Flashers_f28.RotX = a ' VLM.Props;LM;1;SpiralToy
	SpiralToy_LM_Flashers_f38.RotX = a ' VLM.Props;LM;1;SpiralToy
	SpiralToy_LM_Flashers_f40.RotX = a ' VLM.Props;LM;1;SpiralToy
End Sub

' set KickerBall = ActiveBall
' Kickerball = Empty
' If Not IsEmpty(KickerBall) Then

Sub KickBall(kball, kangle, kvel, kvelz, kzlift)
	dim rangle
	rangle = PI * (kangle - 90) / 180

	kball.z = kball.z + kzlift
	kball.velz = kvelz
	kball.velx = cos(rangle)*kvel
	kball.vely = sin(rangle)*kvel
End Sub

'******************************************************
'						FUNCTIONS
'******************************************************

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


'*******************************************************************
'*************************       Targets        ********************
'*******************************************************************

Sub sw47_Hit:STHit 47:End Sub
Sub sw48_Hit:STHit 48:End Sub
Sub sw64_Hit:STHit 64:End Sub
Sub sw65_Hit:STHit 65:End Sub
Sub sw65a_Hit:STHit 165:End Sub
Sub sw66_Hit:STHit 66:End Sub
Sub sw67_Hit:STHit 67:End Sub
Sub sw68_Hit:STHit 68:End Sub
Sub sw77_Hit:STHit 77:End Sub
Sub sw78_Hit:STHit 78:End Sub

'******************************************************
'***************   Mini PF Switches *******************
'******************************************************

Sub sw44_Hit:vpmTimer.PulseSw 44: End Sub
Sub sw45_Hit:vpmTimer.PulseSw 45 : End Sub
Sub sw45a_Hit:vpmTimer.PulseSw 45 : End Sub
Sub sw46_Hit:vpmTimer.PulseSw 46 : End Sub
Sub sw46a_Hit:vpmTimer.PulseSw 46 : End Sub
Sub sw75_Hit:vpmTimer.PulseSw 75 : End Sub
Sub sw75_UnHit
	if activeball.vely < 0 Then  PlaySoundat "fx_power", sw75
End Sub
Sub sw76_Hit:vpmTimer.PulseSw 76: End Sub


'******************************************************
'***************  Ramps Switches **********************
'******************************************************

Sub sw53_Hit:vpmTimer.PulseSw 53:SoundPlayfieldGate: End Sub
Sub sw54_Hit:vpmTimer.PulseSw 54 :SoundPlayfieldGate: LRampSw.rotatetoend : End Sub
Sub sw54_UnHit: LRampSw.rotatetostart : End Sub
Sub sw73_Hit ' Enter wire ramp to mini playfield
	vpmTimer.PulseSw 73
	WireRampOn False
	'Playsoundatball "fx_metalrolling"
End Sub

'**************************************************************
'***************  Rollover Switches   *************************
'**************************************************************

Sub sw11_Hit:  Update_Wires 11, True:vpmTimer.PulseSw 11:RandomSoundRollover: End Sub
Sub sw11_UnHit:Update_Wires 11, False: End Sub
Sub sw12_Hit:  Update_Wires 12, True:vpmTimer.PulseSw 12:RandomSoundRollover: End Sub
Sub sw12_UnHit:Update_Wires 12, False: End Sub
Sub sw36_Hit:  Update_Wires 36, True:vpmTimer.PulseSw 36:RandomSoundRollover: End Sub
Sub sw36_UnHit:Update_Wires 36, False: End Sub
Sub sw37_Hit:  Update_Wires 37, True:vpmTimer.PulseSw 37:RandomSoundRollover: End Sub
Sub sw37_UnHit:Update_Wires 37, False: End Sub
Sub sw38_Hit:  Update_Wires 38, True:vpmTimer.PulseSw 38:RandomSoundRollover: End Sub
Sub sw38_UnHit:Update_Wires 38, False: End Sub
Sub sw52_Hit:  Update_Wires 52, True:vpmTimer.PulseSw 52:RandomSoundRollover: End Sub
Sub sw52_UnHit:Update_Wires 52, False: End Sub
Sub sw56_Hit:  Update_Wires 56, True:vpmTimer.PulseSw 56:RandomSoundRollover: End Sub
Sub sw56_UnHit:Update_Wires 56, False: End Sub
Sub sw61_Hit:  Update_Wires 61, True:vpmTimer.PulseSw 61:RandomSoundRollover: End Sub
Sub sw61_UnHit:Update_Wires 61, False: End Sub
Sub sw62_Hit:  Update_Wires 62, True:vpmTimer.PulseSw 62:RandomSoundRollover: End Sub
Sub sw62_UnHit:Update_Wires 62, False: End Sub
Sub sw63_Hit:  Update_Wires 63, True:vpmTimer.PulseSw 63:RandomSoundRollover: End Sub
Sub sw63_UnHit:Update_Wires 63, False: End Sub

Sub Update_Wires(wire, pushed)
	Dim z : If pushed Then z = -14 Else z = 0
	Select Case wire
		Case 11
	sw11_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw11
	sw11_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw11
	sw11_LM_GI_Right.transz = z ' VLM.Props;LM;1;sw11
	sw11_LM_Flashers_f18.transz = z ' VLM.Props;LM;1;sw11
	sw11_LM_Flashers_f28.transz = z ' VLM.Props;LM;1;sw11
	sw11_LM_Inserts_l66.transz = z ' VLM.Props;LM;1;sw11
		Case 12
	sw12_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw12
	sw12_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw12
	sw12_LM_GI_Right.transz = z ' VLM.Props;LM;1;sw12
	sw12_LM_Flashers_f18.transz = z ' VLM.Props;LM;1;sw12
	sw12_LM_Flashers_f40.transz = z ' VLM.Props;LM;1;sw12
	sw12_LM_Inserts_l66.transz = z ' VLM.Props;LM;1;sw12
		Case 27
	sw27_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw27
	sw27_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw27
		Case 36
	sw36_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw36
	sw36_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw36
	sw36_LM_GI_Left.transz = z ' VLM.Props;LM;1;sw36
	sw36_LM_Flashers_f17.transz = z ' VLM.Props;LM;1;sw36
	sw36_LM_Inserts_l31.transz = z ' VLM.Props;LM;1;sw36
	sw36_LM_Inserts_l37.transz = z ' VLM.Props;LM;1;sw36
		Case 37
	sw37_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw37
	sw37_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw37
	sw37_LM_GI_Left.transz = z ' VLM.Props;LM;1;sw37
	sw37_LM_Flashers_f17.transz = z ' VLM.Props;LM;1;sw37
	sw37_LM_Inserts_l33.transz = z ' VLM.Props;LM;1;sw37
	sw37_LM_Inserts_l37.transz = z ' VLM.Props;LM;1;sw37
		Case 38
	sw38_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw38
	sw38_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw38
	sw38_LM_GI_Left.transz = z ' VLM.Props;LM;1;sw38
	sw38_LM_Flashers_f17.transz = z ' VLM.Props;LM;1;sw38
	sw38_LM_Inserts_l35.transz = z ' VLM.Props;LM;1;sw38
	sw38_LM_Inserts_l37.transz = z ' VLM.Props;LM;1;sw38
		Case 56
	sw56_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw56
		Case 61
	sw61_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw61
	sw61_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw61
	sw61_LM_Flashers_f18.transz = z ' VLM.Props;LM;1;sw61
		Case 62
	sw62_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw62
	sw62_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw62
	sw62_LM_Flashers_f18.transz = z ' VLM.Props;LM;1;sw62
	sw62_LM_Flashers_f37.transz = z ' VLM.Props;LM;1;sw62
	sw62_LM_Inserts_l71.transz = z ' VLM.Props;LM;1;sw62
		Case 63
	sw63_BM_Dark_Room.transz = z ' VLM.Props;BM;1;sw63
	sw63_LM_Lit_Room.transz = z ' VLM.Props;LM;1;sw63
	sw63_LM_Flashers_f18.transz = z ' VLM.Props;LM;1;sw63
	sw63_LM_Flashers_f37.transz = z ' VLM.Props;LM;1;sw63
	End Select
End Sub

'**************************************************************
'***************  Opto Switches   *********************
'**************************************************************

Sub sw81_Hit:Controller.Switch(81) = 1:End Sub
Sub sw81_UnHit
	Controller.Switch(81) = 0
	If mLowerRightMagnet.MagnetOn and activeball.id <> PowerBallID then
		activeball.vely = 0
		activeball.velx = 0
	End If
End Sub

Sub sw82_Hit:Controller.Switch(82) = 1:End Sub
Sub sw82_UnHit
	Controller.Switch(82) = 0
	If mUpperRightMagnet.MagnetOn and activeball.id <> PowerBallID then
		activeball.vely = 0
		activeball.velx = 0
	End If
End Sub

Sub sw83_Hit:Controller.Switch(83) = 1:End Sub
Sub sw83_UnHit
	Controller.Switch(83) = 0
	If mLeftMagnet.MagnetOn and activeball.id <> PowerBallID then
		activeball.vely = 0
		activeball.velx = 0
	End If
End Sub

'Clock Pass Opto (only in prototypes,supported by rom version 9.4)
Sub sw86_Hit:Controller.Switch(86) = 1:End Sub
Sub sw86_UnHit:Controller.Switch(86) = 0:End Sub
'Gumball entry opto
sub sw87_hit():controller.switch(87) = 1:end Sub
sub sw87_unhit():controller.switch(87) = 0:end Sub
''Autoplunger 2nd Opto (only in prototypes,supported by rom version 9.4)
'Sub sw71_Hit:Controller.Switch(71) = 1:End Sub
'Sub sw71_UnHit:Controller.Switch(71) = 0:End Sub

'*************************************************************
'***************   Shooting lane   ***************************
'*************************************************************

Sub sw27_Hit()
	controller.switch(27) = 1
	BIPL = True
	If TVMod = 1 Then
		TVTimer.enabled = 1
		Frame.imageA = "tv_1"
	End If
End Sub

Sub sw27_UnHit():controller.switch(27) = 0:BIPL = False:End Sub


'******************************************************
'					FLIPPERS
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

Sub SolULFlipper(Enabled)
	If Enabled Then
		LeftFlipper1.RotateToEnd
	
		If leftflipper1.currentangle < leftflipper1.endangle + ReflipAngle Then 
			If StagedFlipperMod = 1 Then RandomSoundReflipUpLeft LeftFlipper1
		Else 
			If StagedFlipperMod = 1 Then SoundFlipperUpAttackLeft LeftFlipper1
			If StagedFlipperMod = 1 Then RandomSoundFlipperUpLeft LeftFlipper1
		End If		
	Else
		LeftFlipper1.RotateToStart
		If LeftFlipper1.currentangle < LeftFlipper1.startAngle - 5 Then
			If StagedFlipperMod = 1 Then RandomSoundFlipperDownLeft LeftFlipper1
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolURFlipper(Enabled)
	If Enabled Then
		RightFlipper1.RotateToEnd
	
		If Rightflipper1.currentangle < Rightflipper1.endangle + ReflipAngle Then 
			If StagedFlipperMod = 1 Then RandomSoundReflipUpLeft RightFlipper1
		Else 
			If StagedFlipperMod = 1 Then SoundFlipperUpAttackLeft RightFlipper1
			If StagedFlipperMod = 1 Then RandomSoundFlipperUpLeft RightFlipper1
		End If		
	Else
		RightFlipper1.RotateToStart
		If RightFlipper1.currentangle < RightFlipper1.startAngle - 5 Then
			If StagedFlipperMod = 1 Then RandomSoundFlipperDownLeft RightFlipper1
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub


'******************************************************
'  FLIPPER TRICKS 
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState, 0
	FlipperTricks2 LeftFlipper1, LFPress1, LFCount1, LFEndAngle1, LFState1, 1
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState, 0
	FlipperTricks RightFlipper1, RFPress1, RFCount1, RFEndAngle1, RFState1, 1

	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b, BOT
	BOT = GetBalls

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
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then 
				EOSNudge1 = 0
		end if
	End If
End Sub

'*****************
' Maths
'*****************
'Dim PI: PI = 4*Atn(1)

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

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

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

'Function Distance(ax,ay,bx,by)
'	Distance = SQR((ax - bx)^2 + (ay - by)^2)
'End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI /180
End Function
'
'Function AnglePP(ax,ay,bx,by)
'	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
'End Function

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


dim LFPress, RFPress, LFPress1, RFPress1, LFCount, LFCount1, RFCount, RFCount1
dim LFState, LFState1, RFState, RFState1
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim EOST2, EOSA2,FReturn2
dim RFEndAngle, RFEndAngle1, LFEndAngle, LFEndAngle1

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 0.85 
Const EOSAnew = 1
Const EOSRampup = 0

Dim SOSRampup
SOSRampup = 2.5

LFEndAngle = Leftflipper.endangle
LFEndAngle1 = Leftflipper1.endangle
RFEndAngle = RightFlipper.endangle
RFEndAngle1 = RightFlipper1.endangle

EOST2 = leftflipper1.eostorque
EOSA2 = leftflipper1.eostorqueangle
FReturn2 = leftflipper1.return

Const EOSTnew2 = 1.2 'EM

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.715
Const EOSReturn2 = 0.055  'EM's
'Const EOSReturn = 0.045  'late 70's to mid 80's
'Const EOSReturn = 0.035  'mid 80's to early 90's
Const EOSReturn = 0.025  'mid 90's and later

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
		Dim b, BOT
		BOT = GetBalls

		For b = 0 to UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState, LiveCatchUpper) 
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

		If LiveCatchUpper = 1 Then
			if GameTime - FCount < LiveCatch/2 Then
				Flipper.Elasticity = LiveElasticity
			elseif GameTime - FCount < LiveCatch Then
				Flipper.Elasticity = 0.1
			Else
				Flipper.Elasticity = FElasticity
			end if
		End If

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

Sub FlipperActivate2(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity

	Flipper.eostorque = EOST2         
	Flipper.eostorqueangle = EOSA2         
End Sub

Sub FlipperDeactivate2(Flipper, FlipperPress)
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA2
	Flipper.eostorque = EOST2*EOSReturn2/FReturn2


	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT
		BOT = GetBalls

		For b = 0 to UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks2 (Flipper, FlipperPress, FCount, FEndAngle, FState, LiveCatchUpper) 
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

		If LiveCatchUpper = 1 Then
			if GameTime - FCount < LiveCatch/2 Then
				Flipper.Elasticity = LiveElasticity
			elseif GameTime - FCount < LiveCatch Then
				Flipper.Elasticity = 0.1
			Else
				Flipper.Elasticity = FElasticity
			end if
		End If

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup                        
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST2        
			Flipper.eostorqueangle = EOSA2
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
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm, 1
    End If
End Sub


'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************


Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub

Sub LeftFlipper1_Collide(parm)
	LeftFlipperCollide parm
End Sub

Sub RightFlipper1_Collide(parm)
	RightFlipperCollide parm
End Sub

' This subroutine updates the flipper shadows and visual primitives
Sub FlipperVisualUpdate
	Dim lfa, rfa, lf1a, rf1a
	lfa = LeftFlipper.CurrentAngle
	rfa = RightFlipper.CurrentAngle
	lf1a = LeftFlipper1.CurrentAngle
	rf1a = RightFlipper1.CurrentAngle
	FlipperLSh.RotZ = lfa
	FlipperRSh.RotZ = rfa
	FlipperL1Sh.RotZ = lf1a
	FlipperR1Sh.RotZ = rf1a

	lfa = lfa + 90
	rfa = rfa + 90
	lf1a = lf1a + 90
	rf1a = rf1a + 90
	FlipperL_BM_Dark_Room.RotZ = lfa ' VLM.Props;BM;1;FlipperL
	FlipperL_LM_GI_Clock.RotZ = lfa ' VLM.Props;LM;1;FlipperL
	FlipperL_LM_Lit_Room.RotZ = lfa ' VLM.Props;LM;1;FlipperL
	FlipperL_LM_GI_Left.RotZ = lfa ' VLM.Props;LM;1;FlipperL
	FlipperL_LM_GI_Right.RotZ = lfa ' VLM.Props;LM;1;FlipperL
	FlipperL_LM_Inserts_l11.RotZ = lfa ' VLM.Props;LM;1;FlipperL
	FlipperL_LM_Inserts_l13.RotZ = lfa ' VLM.Props;LM;1;FlipperL
	FlipperL_LM_Inserts_l14.RotZ = lfa ' VLM.Props;LM;1;FlipperL
	FlipperL_LM_Inserts_l41.RotZ = lfa ' VLM.Props;LM;1;FlipperL
	FlipperL_LM_Inserts_l42.RotZ = lfa ' VLM.Props;LM;1;FlipperL
	FlipperL_LM_Inserts_l43.RotZ = lfa ' VLM.Props;LM;1;FlipperL
	FlipperL_LM_Inserts_l44.RotZ = lfa ' VLM.Props;LM;1;FlipperL
	FlipperL_LM_Inserts_l45.RotZ = lfa ' VLM.Props;LM;1;FlipperL
	FlipperL_LM_Inserts_l46.RotZ = lfa ' VLM.Props;LM;1;FlipperL
	FlipperL_LM_Inserts_l47.RotZ = lfa ' VLM.Props;LM;1;FlipperL
	FlipperL1_BM_Dark_Room.RotZ = lf1a ' VLM.Props;BM;1;FlipperL1
	FlipperL1_LM_Mod_l110.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_GI_Clock.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_Lit_Room.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_GI_Left.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_GI_MiniPF.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_Flashers_f17.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_Flashers_f18.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_Flashers_f28.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_Flashers_f39.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_Flashers_f40.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_Inserts_l51.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_Inserts_l52.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_Inserts_l53.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_Inserts_l54.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_Inserts_l55.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_Inserts_l56.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_Inserts_l65.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_Inserts_l72.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_Inserts_l73.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_Flashers_l81.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperL1_LM_Inserts_l82.RotZ = lf1a ' VLM.Props;LM;1;FlipperL1
	FlipperR_BM_Dark_Room.RotZ = rfa ' VLM.Props;BM;1;FlipperR
	FlipperR_LM_GI_Clock.RotZ = rfa ' VLM.Props;LM;1;FlipperR
	FlipperR_LM_Lit_Room.RotZ = rfa ' VLM.Props;LM;1;FlipperR
	FlipperR_LM_GI_Left.RotZ = rfa ' VLM.Props;LM;1;FlipperR
	FlipperR_LM_GI_Right.RotZ = rfa ' VLM.Props;LM;1;FlipperR
	FlipperR_LM_Inserts_l11.RotZ = rfa ' VLM.Props;LM;1;FlipperR
	FlipperR_LM_Inserts_l23.RotZ = rfa ' VLM.Props;LM;1;FlipperR
	FlipperR_LM_Inserts_l24.RotZ = rfa ' VLM.Props;LM;1;FlipperR
	FlipperR_LM_Inserts_l41.RotZ = rfa ' VLM.Props;LM;1;FlipperR
	FlipperR_LM_Inserts_l42.RotZ = rfa ' VLM.Props;LM;1;FlipperR
	FlipperR_LM_Inserts_l43.RotZ = rfa ' VLM.Props;LM;1;FlipperR
	FlipperR_LM_Inserts_l44.RotZ = rfa ' VLM.Props;LM;1;FlipperR
	FlipperR_LM_Inserts_l45.RotZ = rfa ' VLM.Props;LM;1;FlipperR
	FlipperR_LM_Inserts_l46.RotZ = rfa ' VLM.Props;LM;1;FlipperR
	FlipperR_LM_Inserts_l47.RotZ = rfa ' VLM.Props;LM;1;FlipperR
	FlipperR_LM_Inserts_l48.RotZ = rfa ' VLM.Props;LM;1;FlipperR
	FlipperR_LM_Inserts_l66.RotZ = rfa ' VLM.Props;LM;1;FlipperR
	FlipperR1_BM_Dark_Room.RotZ = rf1a ' VLM.Props;BM;1;FlipperR1
	FlipperR1_LM_GI_Clock.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Lit_Room.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_GI_Right.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Mod_l106.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Mod_l107.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Mod_l111.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Flashers_f17.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Flashers_f18.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Flashers_f37.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Inserts_l18.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Inserts_l21.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Inserts_l22.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Inserts_l23.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Inserts_l24.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Inserts_l25.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Inserts_l26.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Inserts_l27.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Inserts_l28.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Inserts_l34.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Inserts_l36.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Inserts_l71.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Inserts_l72.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Flashers_l85.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperR1_LM_Flashers_l86.RotZ = rf1a ' VLM.Props;LM;1;FlipperR1
	FlipperSpL_BM_Dark_Room.RotZ = lfa ' VLM.Props;BM;1;FlipperSpL
	FlipperSpL_LM_GI_Clock.RotZ = lfa ' VLM.Props;LM;1;FlipperSpL
	FlipperSpL_LM_Lit_Room.RotZ = lfa ' VLM.Props;LM;1;FlipperSpL
	FlipperSpL_LM_GI_Left.RotZ = lfa ' VLM.Props;LM;1;FlipperSpL
	FlipperSpL_LM_GI_Right.RotZ = lfa ' VLM.Props;LM;1;FlipperSpL
	FlipperSpL_LM_Inserts_l11.RotZ = lfa ' VLM.Props;LM;1;FlipperSpL
	FlipperSpL_LM_Inserts_l13.RotZ = lfa ' VLM.Props;LM;1;FlipperSpL
	FlipperSpL_LM_Inserts_l14.RotZ = lfa ' VLM.Props;LM;1;FlipperSpL
	FlipperSpL_LM_Inserts_l41.RotZ = lfa ' VLM.Props;LM;1;FlipperSpL
	FlipperSpL_LM_Inserts_l42.RotZ = lfa ' VLM.Props;LM;1;FlipperSpL
	FlipperSpL_LM_Inserts_l43.RotZ = lfa ' VLM.Props;LM;1;FlipperSpL
	FlipperSpL_LM_Inserts_l44.RotZ = lfa ' VLM.Props;LM;1;FlipperSpL
	FlipperSpL_LM_Inserts_l45.RotZ = lfa ' VLM.Props;LM;1;FlipperSpL
	FlipperSpL_LM_Inserts_l46.RotZ = lfa ' VLM.Props;LM;1;FlipperSpL
	FlipperSpL_LM_Inserts_l47.RotZ = lfa ' VLM.Props;LM;1;FlipperSpL
	FlipperSpL1_BM_Dark_Room.RotZ = lf1a ' VLM.Props;BM;1;FlipperSpL1
	FlipperSpL1_LM_GI_Clock.RotZ = lf1a ' VLM.Props;LM;1;FlipperSpL1
	FlipperSpL1_LM_Lit_Room.RotZ = lf1a ' VLM.Props;LM;1;FlipperSpL1
	FlipperSpL1_LM_GI_Left.RotZ = lf1a ' VLM.Props;LM;1;FlipperSpL1
	FlipperSpL1_LM_GI_MiniPF.RotZ = lf1a ' VLM.Props;LM;1;FlipperSpL1
	FlipperSpL1_LM_Flashers_f17.RotZ = lf1a ' VLM.Props;LM;1;FlipperSpL1
	FlipperSpL1_LM_Flashers_f18.RotZ = lf1a ' VLM.Props;LM;1;FlipperSpL1
	FlipperSpL1_LM_Flashers_f28.RotZ = lf1a ' VLM.Props;LM;1;FlipperSpL1
	FlipperSpL1_LM_Flashers_f39.RotZ = lf1a ' VLM.Props;LM;1;FlipperSpL1
	FlipperSpL1_LM_Flashers_f40.RotZ = lf1a ' VLM.Props;LM;1;FlipperSpL1
	FlipperSpL1_LM_Inserts_l51.RotZ = lf1a ' VLM.Props;LM;1;FlipperSpL1
	FlipperSpL1_LM_Inserts_l52.RotZ = lf1a ' VLM.Props;LM;1;FlipperSpL1
	FlipperSpL1_LM_Inserts_l53.RotZ = lf1a ' VLM.Props;LM;1;FlipperSpL1
	FlipperSpL1_LM_Inserts_l54.RotZ = lf1a ' VLM.Props;LM;1;FlipperSpL1
	FlipperSpL1_LM_Inserts_l56.RotZ = lf1a ' VLM.Props;LM;1;FlipperSpL1
	FlipperSpL1_LM_Inserts_l72.RotZ = lf1a ' VLM.Props;LM;1;FlipperSpL1
	FlipperSpL1_LM_Inserts_l73.RotZ = lf1a ' VLM.Props;LM;1;FlipperSpL1
	FlipperSpL1_LM_Flashers_l81.RotZ = lf1a ' VLM.Props;LM;1;FlipperSpL1
	FlipperSpL1_LM_Inserts_l82.RotZ = lf1a ' VLM.Props;LM;1;FlipperSpL1
	FlipperSpR_BM_Dark_Room.RotZ = rfa ' VLM.Props;BM;1;FlipperSpR
	FlipperSpR_LM_GI_Clock.RotZ = rfa ' VLM.Props;LM;1;FlipperSpR
	FlipperSpR_LM_Lit_Room.RotZ = rfa ' VLM.Props;LM;1;FlipperSpR
	FlipperSpR_LM_GI_Left.RotZ = rfa ' VLM.Props;LM;1;FlipperSpR
	FlipperSpR_LM_GI_Right.RotZ = rfa ' VLM.Props;LM;1;FlipperSpR
	FlipperSpR_LM_Inserts_l11.RotZ = rfa ' VLM.Props;LM;1;FlipperSpR
	FlipperSpR_LM_Inserts_l23.RotZ = rfa ' VLM.Props;LM;1;FlipperSpR
	FlipperSpR_LM_Inserts_l24.RotZ = rfa ' VLM.Props;LM;1;FlipperSpR
	FlipperSpR_LM_Inserts_l41.RotZ = rfa ' VLM.Props;LM;1;FlipperSpR
	FlipperSpR_LM_Inserts_l42.RotZ = rfa ' VLM.Props;LM;1;FlipperSpR
	FlipperSpR_LM_Inserts_l43.RotZ = rfa ' VLM.Props;LM;1;FlipperSpR
	FlipperSpR_LM_Inserts_l44.RotZ = rfa ' VLM.Props;LM;1;FlipperSpR
	FlipperSpR_LM_Inserts_l45.RotZ = rfa ' VLM.Props;LM;1;FlipperSpR
	FlipperSpR_LM_Inserts_l46.RotZ = rfa ' VLM.Props;LM;1;FlipperSpR
	FlipperSpR_LM_Inserts_l47.RotZ = rfa ' VLM.Props;LM;1;FlipperSpR
	FlipperSpR_LM_Inserts_l48.RotZ = rfa ' VLM.Props;LM;1;FlipperSpR
	FlipperSpR_LM_Inserts_l66.RotZ = rfa ' VLM.Props;LM;1;FlipperSpR
	FlipperSpR1_BM_Dark_Room.RotZ = rf1a ' VLM.Props;BM;1;FlipperSpR1
	FlipperSpR1_LM_GI_Clock.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Lit_Room.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_GI_Right.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Mod_l106.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Mod_l107.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Mod_l111.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Flashers_f17.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Flashers_f18.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Flashers_f37.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Inserts_l18.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Inserts_l21.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Inserts_l23.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Inserts_l24.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Inserts_l25.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Inserts_l26.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Inserts_l27.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Inserts_l28.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Inserts_l36.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Inserts_l71.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Inserts_l72.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Flashers_l85.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
	FlipperSpR1_LM_Flashers_l86.RotZ = rf1a ' VLM.Props;LM;1;FlipperSpR1
End Sub


Public Sub MovableVisualUpdate
	UpdateClock
	dim a
	
	If BIP > 0 AND SpiralMod = 1 Then SpiralMove.enabled = Lampz.state(68)

	' Camera mod
	If CameraMod Then
		Lampz.state(110) = Lampz.state(55)
	Else
		Lampz.state(110) = 0
	End If

	' Rocket mod
	Lampz.state(111) = Lampz.state(137) / 255

	a = RampDiverter.CurrentAngle + 180
	DiverterP_BM_Dark_Room.RotZ = a ' VLM.Props;BM;1;DiverterP
	DiverterP_LM_Lit_Room.RotZ = a ' VLM.Props;LM;1;DiverterP
	DiverterP_LM_GI_Right.RotZ = a ' VLM.Props;LM;1;DiverterP
	DiverterP_LM_Flashers_f18.RotZ = a ' VLM.Props;LM;1;DiverterP
	DiverterP_LM_Flashers_f20.RotZ = a ' VLM.Props;LM;1;DiverterP
	DiverterP_LM_Flashers_f28.RotZ = a ' VLM.Props;LM;1;DiverterP
	DiverterP_LM_Flashers_f38.RotZ = a ' VLM.Props;LM;1;DiverterP
	DiverterP_LM_Flashers_f41.RotZ = a ' VLM.Props;LM;1;DiverterP
	DiverterP_LM_Inserts_l68.RotZ = a ' VLM.Props;LM;1;DiverterP
	
	a = GumballDiverter.CurrentAngle
	DiverterP1_BM_Dark_Room.RotZ = a ' VLM.Props;BM;1;DiverterP1
	DiverterP1_LM_Lit_Room.RotZ = a ' VLM.Props;LM;1;DiverterP1
	DiverterP1_LM_GI_Left.RotZ = a ' VLM.Props;LM;1;DiverterP1
	DiverterP1_LM_Flashers_f28.RotZ = a ' VLM.Props;LM;1;DiverterP1
	DiverterP1_LM_Flashers_f41.RotZ = a ' VLM.Props;LM;1;DiverterP1

	a = ShooterDiverter.CurrentAngle
	ShooterDiv_BM_Dark_Room.RotZ = a ' VLM.Props;BM;1;ShooterDiv
	ShooterDiv_LM_Lit_Room.RotZ = a ' VLM.Props;LM;1;ShooterDiv

	a = LRampG.CurrentAngle
	sw53p_BM_Dark_Room.RotX = a ' VLM.Props;BM;1;sw53p
	sw53p_LM_Lit_Room.RotX = a ' VLM.Props;LM;1;sw53p
	sw53p_LM_GI_Left.RotX = a ' VLM.Props;LM;1;sw53p
	sw53p_LM_Flashers_f18.RotX = a ' VLM.Props;LM;1;sw53p
	sw53p_LM_Flashers_f20.RotX = a ' VLM.Props;LM;1;sw53p

	a = -LRampSw.CurrentAngle
	sw54p_BM_Dark_Room.RotX = a ' VLM.Props;BM;1;sw54p
	sw54p_LM_Lit_Room.RotX = a ' VLM.Props;LM;1;sw54p
	sw54p_LM_GI_Right.RotX = a ' VLM.Props;LM;1;sw54p
	sw54p_LM_Flashers_f20.RotX = a ' VLM.Props;LM;1;sw54p
	sw54p_LM_Flashers_f41.RotX = a ' VLM.Props;LM;1;sw54p

	a = Gate1.CurrentAngle
	Gate1_BM_Dark_Room.RotX = a ' VLM.Props;BM;1;Gate1
	Gate1_LM_Lit_Room.RotX = a ' VLM.Props;LM;1;Gate1
	Gate1_LM_Flashers_f18.RotX = a ' VLM.Props;LM;1;Gate1

	a = Gate2.CurrentAngle
	Gate2_BM_Dark_Room.RotX = a ' VLM.Props;BM;1;Gate2
	Gate2_LM_Lit_Room.RotX = a ' VLM.Props;LM;1;Gate2
	Gate2_LM_Flashers_f18.RotX = a ' VLM.Props;LM;1;Gate2
	Gate2_LM_Flashers_f41.RotX = a ' VLM.Props;LM;1;Gate2
End Sub

Sub BallBrightnessUpdate
	Dim b, x, ballbrightness
	For b = 0 to UBound(TZBalls)
		If CheckGumball(TZBalls(b)) Or CheckGumball2(TZBalls(b)) Or CheckGumball3(TZBalls(b)) Then
			' Inside Gumball machine => only get the dimmed general light level
			ballbrightness = CInt(16 + (128 - 16) * (LightLevel / 100))
		Else
			' On playfield => global light level, with a blend of GI depending on position
			x = TZBalls(b).x / 1096
			If x < 0 Then x = 0
			If x > 1 Then x = 1
			ballbrightness = CInt(16 + 48*0.01*((1-x)*Playfield_LM_GI_Left.Opacity + x*Playfield_LM_GI_Right.Opacity) + (255 - 64) * (LightLevel / 100))
		End If
		If ballbrightness < 0 Then x = 0
		If ballbrightness > 255 Then x = 255
		ballbrightness = ballbrightness + (ballbrightness * 256) + (ballbrightness * 256 * 256)
		TZBalls(b).color = ballbrightness
	Next
End Sub

'********************************************************************************
'******************  NFOZZY'S GUMBALL MACHINE  2 ********************************
'********************** UPDATED BY ROTHBAUERW ***********************************
'********************************************************************************

Sub GumKickout()	'unfreeze balls in gumball machine trough
	Freezekicker2.enabled = false
	Freezekicker1.enabled = false
	Freezekicker0.enabled = false

	Freezekicker2.kick 0,0,0
	Freezekicker1.kick 0,0,0
	Freezekicker0.kickz 0,0,0,-25

	FreezeKicker0.timerenabled = true
End Sub

FreezeKicker0.TimerInterval= 80	'interval for kickout, how long the dropwall stays down. Adjust me if it kicks out 2, or none.

Sub FreezeKicker0_Timer()	'repop gumball floor after a short delay
	Freezekicker0.enabled = true
	If Not FreezeKicker1.enabled then 
		If CheckGumball(TZBall1) or CheckGumball(TZBall2) or CheckGumball(TZBall3) or CheckGumball(TZBall4) or CheckGumball(TZBall5) or CheckGumball(TZBall6) then 
			Freezekicker1.enabled = true
		End If
	Elseif Not FreezeKicker2.enabled Then
		If CheckGumball2(TZBall1) or CheckGumball2(TZBall2) or CheckGumball2(TZBall3) or CheckGumball2(TZBall4) or CheckGumball2(TZBall5) or CheckGumball2(TZBall6) then 
			Freezekicker2.enabled = true
			me.timerenabled = 0			
		End If
	End If
End Sub

Function CheckGumball(ball)
	If Int(ball.x) = 211 and Int(ball.y) = 276 and Int(ball.z) = 171 Then
		CheckGumball = True
	Else
		CheckGumball = False
	End If
End Function

Function CheckGumball2(ball)
	If Int(ball.x) = 199 and Int(ball.y) = 236 and Int(ball.z) = 198 Then
		CheckGumball2 = True
	Else
		CheckGumball2 = False
	End If
End Function

Function CheckGumball3(ball)
	If Int(ball.x) = 191 and Int(ball.y) = 199 and Int(ball.z) = 231 Then
		CheckGumball3 = True
	Else
		CheckGumball3 = False
	End If
End Function

Sub SolGumRelease(enabled)	'this is a pinmame hack, will not work with solmodcallbacks. Called from motor sol instead.
    If enabled Then
		GumKickout 					'new
		vpmtimer.PulseSw 55	'Geneva switch
    End If
End Sub

Sub SolGumballMotor(aOn)
	if aOn then PlaySoundat SoundFX("fx_GumMachine",DOFGear), FreezeKicker0 : vpmtimer.addtimer 1400, "GumKnobTimer.enabled = 1'" : vpmtimer.addtimer 1700, "SolGumRelease 1'" 				
End Sub

GumKnobTimer.Interval = -1
Sub GumKnobTimer_Timer()	'prior 20ms period
	GMKnob_BM_Dark_Room.RotY = GMKnob_BM_Dark_Room.RotY + 1 * frametime
	If GMKnob_BM_Dark_Room.RotY >  360 then GumKnobTimer.enabled = 0 : GMKnob_BM_Dark_Room.RotY = 0
	GMKnob_LM_Lit_Room.RotY = GMKnob_BM_Dark_Room.RotY  ' VLM.Props;LM;1;GMKnob
	GMKnob_LM_GI_MiniPF.RotY = GMKnob_BM_Dark_Room.RotY  ' VLM.Props;LM;1;GMKnob
	GMKnob_LM_Flashers_f18.RotY = GMKnob_BM_Dark_Room.RotY  ' VLM.Props;LM;1;GMKnob
	GMKnob_LM_Flashers_f20.RotY = GMKnob_BM_Dark_Room.RotY  ' VLM.Props;LM;1;GMKnob
	GMKnob_LM_Flashers_f28.RotY = GMKnob_BM_Dark_Room.RotY  ' VLM.Props;LM;1;GMKnob
	GMKnob_LM_Flashers_f38.RotY = GMKnob_BM_Dark_Room.RotY  ' VLM.Props;LM;1;GMKnob
	GMKnob_LM_Flashers_f40.RotY = GMKnob_BM_Dark_Room.RotY  ' VLM.Props;LM;1;GMKnob
End Sub

'********************************************************************
'*************************   CLOCK   ********************************
'********************************************************************

Dim LastTime : LastTime = 0
dim LastClockIndex

Sub UpdateClock()
    Dim Time, Min, Hour, temp
    'Time = CInt(Controller.GetMech(0) )
    Time = Controller.GetMech(0)
    If Time <> LastTime Then
		Min = (Time Mod 60)
		Hour = Int(Time / 2)

	ClockLarge_BM_Dark_Room.RotY = min * 6 ' VLM.Props;BM;1;ClockLarge
	ClockShort_BM_Dark_Room.RotY = Hour - 45 ' VLM.Props;BM;1;ClockShort

		LastTime = Time	'10.4 playsound args - name,loopcount,volume,pan,randompitch,pitch,UseExisting,Restart,Fade
		PlaySound SoundFXDOF("fx_motor",101,DOFPulse,DOFGear), -1, 1, 0.05, 0, 0, 1, 0
		LastClockIndex = 0
	Elseif LastClockIndex <=2 Then	'wait an update before stopping motor sound
		LastClockIndex = LastClockIndex + 1
	Elseif LastClockIndex > 2 then 
		Stopsound "fx_motor"
    End If
End Sub

'**********************************************************************
'**************   POWER FIELD MAGNETS *********************************
'**********************************************************************

Sub SolMiniMagnet(aMag, enabled)
	If enabled Then
		PlaySoundat SoundFX("fx_magnet",DOFShaker), sw76
		With aMag
			.removeball PowerBall
			.MagnetOn = True
			.Update
			.MagnetOn = False
		End With
	End If
End Sub

'**********************************************************************
' SPECIAL CODE BY NFOZZY TO HANDLE MAGNET TRIGGERS
' based on the code by KIEFERSKUNK/DORSOLA
' Method: on extra triggers unhit, kill the velocity of the
' ball if the magnet is on, helping the magnet catch the ball.
'**********************************************************************

Sub sw81_help_unhit
	If activeball.vely > 28 then  activeball.vely = RndNumO (26,27)			'-ninuzzu- Let's slow down the ball a bit so the magnets can
	If activeball.vely < - 28 then  activeball.vely = - RndNumO (26,27)		'catch the ball
	If mLowerRightMagnet.MagnetOn = 1 and activeball.id <> PowerBallID then
		activeball.vely = activeball.vely * -0.2
		activeball.velx = activeball.velx * -0.2
	End If
End Sub

Sub LowerRightMagnet_hit()
	If mLowerRightMagnet.MagnetOn = 1 and activeball.id <> PowerBallID then
		activeball.vely = activeball.vely/10
		activeball.velx = activeball.velx/10
	End If
End Sub

Sub sw82_help_unhit
	If mUpperRightMagnet.MagnetOn = 1 and activeball.id <> PowerBallID then
		activeball.vely = activeball.vely * -0.2
		activeball.velx = activeball.velx * -0.2
    End If
End Sub

Sub UpperRightMagnet_hit()
	If mUpperRightMagnet.MagnetOn = 1 and activeball.id <> PowerBallID then
		activeball.vely = activeball.vely/10
		activeball.velx = activeball.velx/10
	End If
End Sub

Sub sw83_help_unhit
	If mLeftMagnet.MagnetOn = 1 and activeball.id <> PowerBallID then
		activeball.vely = activeball.vely * -0.2
		activeball.velx = activeball.velx * -0.2
	End If
End Sub

Sub LeftMagnet_hit()
	If mLeftMagnet.MagnetOn = 1 and activeball.id <> PowerBallID then
		activeball.vely = activeball.vely/10
		activeball.velx = activeball.velx/10
	End If
End Sub

Sub SolLeftMagnet(enabled)
	If enabled Then
		mLeftMagnet.MagnetOn = 1
		mleftmagnet.removeball PowerBall
		PlaySoundat SoundFX("fx_magnet_catch",DOFShaker), sw83
	Else
		mLeftMagnet.MagnetOn = 0
	End If
End Sub

Sub SolUpperRightMagnet(enabled)
	If enabled Then
		mUpperRightMagnet.MagnetOn = 1
		mUpperRightMagnet.removeball PowerBall
		PlaySoundat SoundFX("fx_magnet_catch",DOFShaker), sw82
	Else
		mUpperRightMagnet.MagnetOn = 0
	End If
End Sub

Sub SolLowerRightMagnet(enabled)
	If enabled Then
		mLowerRightMagnet.MagnetOn = 1
		mLowerRightMagnet.removeball PowerBall
		PlaySoundat SoundFX("fx_magnet_catch",DOFShaker), sw81
	Else
		mLowerRightMagnet.MagnetOn = 0
	End If
End Sub


'******************************************************
'						SOUNDS
'******************************************************

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
Sub PlaySoundAtLoop(soundname, tableobj)
    PlaySound soundname, -1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, aVol, tableobj)
    PlaySound soundname, 1, aVol, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


Sub PlaySoundAtExisting(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBallVolME (Soundname, aVol)
	Playsound soundname, 1,aVol, AudioPan(ActiveBall), 0,0,1,0, AudioFade(ActiveBall)
End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************
Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / tablewidth-1
    If tmp> 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function RndNumO(min, max)
    RndNumO = Int(Rnd() * (max-min + 1) ) + min ' Sets a random number between min and max
End Function

'**********************
' Balldrop & Ramp Sound
'**********************


'*******************************************
'  Ramp Triggers
'*******************************************

Sub LREnter_Hit() ' Enter left plastic ramp
    If ActiveBall.VelY < 0 Then PlaySoundAtBallVol "fx_rlenter", Vol(Activeball)*VolumeDial*10
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub WirerampSound1_Hit() ' Enter wire part after plastic (to left side)
	WireRampOn False
    'PlaySoundat "fx_metalrolling", WirerampSound1
End Sub

Sub Balldrop1_Hit() ' Drop from ramp on left side
	WireRampOff
    'StopSound "fx_metalrolling"
End Sub

Sub WirerampSound2_Hit() ' Enter wire part after plastic (to right side)
	WireRampOn False
    'PlaySoundat "fx_metalrolling", WirerampSound2
End Sub

Sub Balldrop3_Hit() ' Drop from ramp on right side
	WireRampOff
    'StopSound "fx_metalrolling"
End Sub

Sub RREnter_Hit() ' Enter right metal ramp
    If ActiveBall.VelY < 0 Then PlaySoundAtBallVol "fx_metal_ramp_hit", Vol(Activeball)
	WireRampOn False
End Sub

Sub WirerampSoundStop_Hit() ' Enter mini playfield after wire ramp
	WireRampOff
    'StopSound "fx_metalrolling"
End Sub


'******************************************************
'		FLIPPER CORRECTION INITIALIZATION
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
	Next

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

	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41, 	1.05
	addpt "Velocity", 3, 0.53, 	1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03, 	0.945

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
'			FLIPPER CORRECTION FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

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
				'playsound "knocker"
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'		FLIPPER POLARITY AND RUBBER DAMPENER
'			SUPPORTING FUNCTIONS 
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

' Used for drop targets and flipper tricks
Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

'******************************************************
'****  PHYSICS DAMPENERS
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

'######################### Add Dampenf to Dampener Class 
'#########################    Only applies dampener when abs(velx) < 2 and vely < 0 and vely > -3.75  


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


'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

dim cor : set cor = New CoRTracker

Class CoRTracker
    public ballvel, ballvelx, ballvely, ballvelz, ballangmomx, ballangmomy, ballangmomz

    Private Sub Class_Initialize : redim ballvel(0) : redim ballvelx(0): redim ballvely(0) : redim ballvelz(0) : redim ballangmomx(0) : redim ballangmomy(0): redim ballangmomz(0): End Sub 

    Public Sub Update()    'tracks in-ball-velocity
        dim str, b, AllBalls, highestID : allBalls = getballs

        for each b in allballs
            if b.id >= HighestID then highestID = b.id
        Next

        if uBound(ballvel) < highestID then redim ballvel(highestID)    'set bounds
        if uBound(ballvelx) < highestID then redim ballvelx(highestID)    'set bounds
        if uBound(ballvely) < highestID then redim ballvely(highestID)    'set bounds
        if uBound(ballvelz) < highestID then redim ballvelz(highestID)    'set bounds
        if uBound(ballangmomx) < highestID then redim ballangmomx(highestID)    'set bounds
        if uBound(ballangmomy) < highestID then redim ballangmomy(highestID)    'set bounds
        if uBound(ballangmomz) < highestID then redim ballangmomz(highestID)    'set bounds

        for each b in allballs
            ballvel(b.id) = BallSpeed(b)
            ballvelx(b.id) = b.velx
            ballvely(b.id) = b.vely
            ballvelz(b.id) = b.velz
            ballangmomx(b.id) = b.angmomx
            ballangmomy(b.id) = b.angmomy
            ballangmomz(b.id) = b.angmomz
        Next
    End Sub
End Class


'******************************************************
'****  END PHYSICS DAMPENERS
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
Dim ST47, ST48, ST64, ST65, ST65a, ST66, ST67, ST68, ST77, ST78

'Set array with stand-up target objects
'
'StandupTargetvar = Array(primary, prim, swtich)
' 	primary: 			vp target to determine target hit
'	prim:				primitive target used for visuals and animation
'							IMPORTANT!!! 
'							transy must be used to offset the target animation
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instrucitons, set to 0

Set ST47 = (new StandupTarget)(sw47, sw47_BM_Dark_Room, 47, 0)
Set ST48 = (new StandupTarget)(sw48, sw48_BM_Dark_Room ,48, 0)
Set ST64 = (new StandupTarget)(sw64, sw64_BM_Dark_Room,64, 0)
Set ST65 = (new StandupTarget)(sw65, sw65_BM_Dark_Room,65, 0)
Set ST65a = (new StandupTarget)(sw65a, sw65a_BM_Dark_Room,165, 0)
Set ST66 = (new StandupTarget)(sw66, sw66_BM_Dark_Room,66, 0)
Set ST67 = (new StandupTarget)(sw67, sw67_BM_Dark_Room,67, 0)
Set ST68 = (new StandupTarget)(sw68, sw68_BM_Dark_Room,68, 0)
Set ST77 = (new StandupTarget)(sw77, sw77_BM_Dark_Room,77, 0)
Set ST78 = (new StandupTarget)(sw78, sw78_BM_Dark_Room,78, 0)



'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST47, ST48, ST64, ST65, ST65a, ST66, ST67, ST68, ST77, ST78)

'Configure the behavior of Stand-up Targets
Const STAnimStep =  -1.5 				'vpunits per animation step (control return to Start)
Const STMaxOffset = -9 			'max vp units target moves when hit
Const STHitSound = "targethit"	'Stand-up Target Hit sound

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
		STBallPhysics Activeball, STArray(i).primary.orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 to uBound(STArray) 
		If STArray(i).sw = switch Then STArrayID = i:Exit Function 
	Next
End Function

sub STBallPhysics(aBall, angle, mass)
	dim rangle,bangle,calc1, calc2, calc3
	rangle = (angle - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))

	calc1 = cor.BallVel(aball.id) * cos(bangle - rangle) * (aball.mass - mass) / (aball.mass + mass)
	calc2 = cor.BallVel(aball.id) * sin(bangle - rangle) * cos(rangle + 4*Atn(1)/2)
	calc3 = cor.BallVel(aball.id) * sin(bangle - rangle) * sin(rangle + 4*Atn(1)/2)

	aBall.velx = calc1 * cos(rangle) + calc2
	aBall.vely = calc1 * sin(rangle) + calc3
End Sub

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

	sw47_LM_Flashers_f18.transy = sw47_BM_Dark_Room.transy
	sw47_LM_Flashers_f28.transy = sw47_BM_Dark_Room.transy
	sw47_LM_Inserts_l51.transy = sw47_BM_Dark_Room.transy
	sw47_LM_Inserts_l82.transy = sw47_BM_Dark_Room.transy
	sw47_LM_Lit_Room.transy = sw47_BM_Dark_Room.transy
	sw47m_BM_Dark_Room.transy = sw47_BM_Dark_Room.transy
	sw47m_LM_Inserts_l82.transy = sw47_BM_Dark_Room.transy
	sw47m_LM_Lit_Room.transy = sw47_BM_Dark_Room.transy

	sw48_LM_Flashers_f17.transy = sw48_BM_Dark_Room.transy
	sw48_LM_GI_Left.transy = sw48_BM_Dark_Room.transy
	sw48_LM_GI_Right.transy = sw48_BM_Dark_Room.transy
	sw48_LM_Inserts_l13.transy = sw48_BM_Dark_Room.transy
	sw48_LM_Inserts_l35.transy = sw48_BM_Dark_Room.transy
	sw48_LM_Inserts_l37.transy = sw48_BM_Dark_Room.transy
	sw48_LM_Inserts_l43.transy = sw48_BM_Dark_Room.transy
	sw48_LM_Inserts_l44.transy = sw48_BM_Dark_Room.transy
	sw48_LM_Inserts_l45.transy = sw48_BM_Dark_Room.transy
	sw48_LM_Inserts_l64.transy = sw48_BM_Dark_Room.transy
	sw48_LM_Lit_Room.transy = sw48_BM_Dark_Room.transy
	sw48m_BM_Dark_Room.transy = sw48_BM_Dark_Room.transy
	sw48m_LM_GI_Left.transy = sw48_BM_Dark_Room.transy
	sw48m_LM_Inserts_l37.transy = sw48_BM_Dark_Room.transy
	sw48m_LM_Lit_Room.transy = sw48_BM_Dark_Room.transy

	sw64_LM_Flashers_f28.transy = sw64_BM_Dark_Room.transy
	sw64_LM_GI_MiniPF.transy = sw64_BM_Dark_Room.transy
	sw64_LM_Inserts_l57.transy = sw64_BM_Dark_Room.transy
	sw64_LM_Inserts_l75.transy = sw64_BM_Dark_Room.transy
	sw64_LM_Lit_Room.transy = sw64_BM_Dark_Room.transy
	sw64m_BM_Dark_Room.transy = sw64_BM_Dark_Room.transy
	sw64m_LM_Flashers_f18.transy = sw64_BM_Dark_Room.transy
	sw64m_LM_Flashers_l83.transy = sw64_BM_Dark_Room.transy
	sw64m_LM_Flashers_l84.transy = sw64_BM_Dark_Room.transy
	sw64m_LM_GI_Clock.transy = sw64_BM_Dark_Room.transy
	sw64m_LM_Inserts_l75.transy = sw64_BM_Dark_Room.transy
	sw64m_LM_Lit_Room.transy = sw64_BM_Dark_Room.transy

	sw65_LM_GI_Clock.transy = sw65_BM_Dark_Room.transy
	sw65_LM_GI_Right.transy = sw65_BM_Dark_Room.transy
	sw65_LM_Lit_Room.transy = sw65_BM_Dark_Room.transy
	sw65m_BM_Dark_Room.transy = sw65_BM_Dark_Room.transy
	sw65m_LM_Flashers_f18.transy = sw65_BM_Dark_Room.transy
	sw65m_LM_Flashers_f20.transy = sw65_BM_Dark_Room.transy
	sw65m_LM_Flashers_f28.transy = sw65_BM_Dark_Room.transy
	sw65m_LM_Flashers_f41.transy = sw65_BM_Dark_Room.transy
	sw65m_LM_GI_Clock.transy = sw65_BM_Dark_Room.transy
	sw65m_LM_GI_Right.transy = sw65_BM_Dark_Room.transy
	sw65m_LM_Lit_Room.transy = sw65_BM_Dark_Room.transy

	sw65a_LM_Flashers_f18.transy = sw65a_BM_Dark_Room.transy
	sw65a_LM_Flashers_f20.transy = sw65a_BM_Dark_Room.transy
	sw65a_LM_Flashers_l83.transy = sw65a_BM_Dark_Room.transy
	sw65a_LM_GI_Clock.transy = sw65a_BM_Dark_Room.transy
	sw65a_LM_GI_Right.transy = sw65a_BM_Dark_Room.transy
	sw65a_LM_Lit_Room.transy = sw65a_BM_Dark_Room.transy
	sw65am_BM_Dark_Room.transy = sw65a_BM_Dark_Room.transy
	sw65am_LM_Flashers_f18.transy = sw65a_BM_Dark_Room.transy
	sw65am_LM_Flashers_f20.transy = sw65a_BM_Dark_Room.transy
	sw65am_LM_Flashers_f28.transy = sw65a_BM_Dark_Room.transy
	sw65am_LM_GI_Clock.transy = sw65a_BM_Dark_Room.transy
	sw65am_LM_GI_Right.transy = sw65a_BM_Dark_Room.transy
	sw65am_LM_Lit_Room.transy = sw65a_BM_Dark_Room.transy


	sw66_LM_Flashers_f17.transy = sw66_BM_Dark_Room.transy
	sw66_LM_Flashers_f28.transy = sw66_BM_Dark_Room.transy
	sw66_LM_GI_Clock.transy = sw66_BM_Dark_Room.transy
	sw66_LM_GI_MiniPF.transy = sw66_BM_Dark_Room.transy
	sw66_LM_GI_Right.transy = sw66_BM_Dark_Room.transy
	sw66_LM_Inserts_l72.transy = sw66_BM_Dark_Room.transy
	sw66_LM_Inserts_l73.transy = sw66_BM_Dark_Room.transy
	sw66_LM_Lit_Room.transy = sw66_BM_Dark_Room.transy
	sw66m_BM_Dark_Room.transy = sw66_BM_Dark_Room.transy
	sw66m_LM_GI_Right.transy = sw66_BM_Dark_Room.transy
	sw66m_LM_Inserts_l72.transy = sw66_BM_Dark_Room.transy

	sw67_LM_GI_Right.transy = sw67_BM_Dark_Room.transy
	sw67_LM_Inserts_l72.transy = sw67_BM_Dark_Room.transy
	sw67_LM_Inserts_l73.transy = sw67_BM_Dark_Room.transy
	sw67_LM_Lit_Room.transy = sw67_BM_Dark_Room.transy
	sw67m_BM_Dark_Room.transy = sw67_BM_Dark_Room.transy
	sw67m_LM_GI_Right.transy = sw67_BM_Dark_Room.transy
	sw67m_LM_Lit_Room.transy = sw67_BM_Dark_Room.transy

	sw68_LM_Flashers_f17.transy = sw68_BM_Dark_Room.transy
	sw68_LM_Flashers_f18.transy = sw68_BM_Dark_Room.transy
	sw68_LM_Inserts_l71.transy = sw68_BM_Dark_Room.transy
	sw68_LM_Inserts_l73.transy = sw68_BM_Dark_Room.transy
	sw68_LM_Lit_Room.transy = sw68_BM_Dark_Room.transy
	sw68m_BM_Dark_Room.transy = sw68_BM_Dark_Room.transy
	sw68m_LM_Inserts_l71.transy = sw68_BM_Dark_Room.transy
	sw68m_LM_Lit_Room.transy = sw68_BM_Dark_Room.transy

	sw77_LM_Flashers_f17.transy = sw77_BM_Dark_Room.transy
	sw77_LM_Flashers_f18.transy = sw77_BM_Dark_Room.transy
	sw77_LM_GI_Left.transy = sw77_BM_Dark_Room.transy
	sw77_LM_Inserts_l16.transy = sw77_BM_Dark_Room.transy
	sw77_LM_Inserts_l17.transy = sw77_BM_Dark_Room.transy
	sw77_LM_Inserts_l37.transy = sw77_BM_Dark_Room.transy
	sw77_LM_Inserts_l38.transy = sw77_BM_Dark_Room.transy
	sw77_LM_Inserts_l64.transy = sw77_BM_Dark_Room.transy
	sw77_LM_Inserts_l65.transy = sw77_BM_Dark_Room.transy
	sw77_LM_Lit_Room.transy = sw77_BM_Dark_Room.transy
	sw77_LM_Mod_l109.transy = sw77_BM_Dark_Room.transy
	sw77m_BM_Dark_Room.transy = sw77_BM_Dark_Room.transy
	sw77m_LM_Flashers_f17.transy = sw77_BM_Dark_Room.transy
	sw77m_LM_Inserts_l64.transy = sw77_BM_Dark_Room.transy
	sw77m_LM_Lit_Room.transy = sw77_BM_Dark_Room.transy
	sw77m_LM_Mod_l109.transy = sw77_BM_Dark_Room.transy

	sw78_LM_Bumpers_l63.transy = sw78_BM_Dark_Room.transy
	sw78_LM_Flashers_f17.transy = sw78_BM_Dark_Room.transy
	sw78_LM_Flashers_f18.transy = sw78_BM_Dark_Room.transy
	sw78_LM_Flashers_f28.transy = sw78_BM_Dark_Room.transy
	sw78_LM_GI_Right.transy = sw78_BM_Dark_Room.transy
	sw78_LM_Inserts_l17.transy = sw78_BM_Dark_Room.transy
	sw78_LM_Inserts_l65.transy = sw78_BM_Dark_Room.transy
	sw78_LM_Lit_Room.transy = sw78_BM_Dark_Room.transy
	sw78_LM_Mod_l109.transy = sw78_BM_Dark_Room.transy
	sw78m_BM_Dark_Room.transy = sw78_BM_Dark_Room.transy
	sw78m_LM_Flashers_f17.transy = sw78_BM_Dark_Room.transy
	sw78m_LM_Inserts_l65.transy = sw78_BM_Dark_Room.transy
	sw78m_LM_Lit_Room.transy = sw78_BM_Dark_Room.transy

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
		vpmTimer.PulseSw switch	mod 100
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

''******************************************************
'' 						FSS
''******************************************************
'
'Dim xoff, yoff, zoff, xrot, xcen, ycen, zscale, topoff, BGArr
'BGArr=Array (FlSol19,FlL16a,FlL16b,FlL24,FlL44a,FlL44b,FlL53a,FlL53b,FlL53c,FlL53d,FlL53f, FlL54,FlL61,FlL62,FlL63, FlL74a,FlL74b,FlL85)
'
'Sub set_FSS()
'
'	xoff = 555
'	yoff = -10
'	zoff = 900set
'	xrot = -90
'
'	' the topper
'	topoff = 1835
'
'	TopDark.x = xoff
'	TopDark.y = yoff - 40
'	TopDark.height = Topoff
'	TopDark.rotx = xrot
'
'	TopHigh.x = xoff
'	TopHigh.y = yoff - 40
'	TopHigh.height = Topoff 
'	TopHigh.rotx = xrot
'
''	TopHigh1.x = xoff
''	TopHigh1.y = yoff - 40
''	TopHigh1.height = Topoff 
''	TopHigh1.rotx = xrot
'
'
'	TopHigh2.x = xoff
'	TopHigh2.y = yoff - 40
'	TopHigh2.height = topoff -100
'	TopHigh2.rotx = xrot
'
'	center_graphix()
'
'End Sub
'
'Sub center_graphix()
'	zscale = 0.0000001 ' screen z scale found in backglass currently set to 1.15 (should by default be set to 1)
'	xcen =(1167 /2) - (92 / 2)
'	ycen = (1167 /2 ) + (290 /2)
'
'	Dim xx
'	Dim yy
'	Dim yfact
'	Dim xfact
'	Dim obj
'	yfact =10 'y fudge factor (ycen was wrong so fix)
'	xfact =0
'
'	For Each obj In BGArr
'		xx =obj.x 
'			
'		obj.x = (xoff -xcen) + xx +xfact
'		yy = obj.y ' get the yoffset before it is changed
'		obj.y =yoff 
'
'			If(yy < 0.) then
'			yy = yy * -1
'			end if
'
'		
'		obj.height =( zoff - ycen) + yy - (yy * zscale) + yfact
'		
'		obj.rotx = xrot
'	Next
'end sub

'VR Room Animations
'**********************************************************************************************************

Sub TimerVRPlunger_Timer
  'debug.print plunger.position
  VR_Primary_plunger.Y = -8.430555 + (5* Plunger.Position)
End Sub







'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

'****** Part A:  Table Elements ******
'
' Import the "bsrtx7" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the BallShadowA flasher set and the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#
'	* with at least as many objects each as there can be balls, including locked balls
' Ensure you have a timer with a -1 interval that is always running

' Create a collection called DynamicSources that includes all light sources you want to cast ball shadows
'***These must be organized in order, so that lights that intersect on the table are adjacent in the collection***
'***If there are more than 3 lights that overlap in a playable area, exclude the less important lights***
' This is because the code will only project two shadows if they are coming from lights that are consecutive in the collection, and more than 3 will cause "jumping" between which shadows are drawn
' The easiest way to keep track of this is to start with the group on the right slingshot and move anticlockwise around the table
'	For example, if you use 6 lights: A & B on the left slingshot and C & D on the right, with E near A&B and F next to C&D, your collection would look like EBACDF
'
'G				H											^	E
'															^	B
'	A		 C												^	A
'	 B		D			your collection should look like	^	G		because E&B, B&A, etc. intersect; but B&D or E&F do not
'  E		  F												^	H
'															^	C
'															^	D
'															^	F
'		When selecting them, you'd shift+click in this order^^^^^

'****** End Part A:  Table Elements ******


'****** Part B:  Code and Functions ******

' *** Timer sub
' The "DynamicBSUpdate" sub should be called by a timer with an interval of -1 (framerate)
'Sub FrameTimer_Timer()
'	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
'End Sub

' *** These are usually defined elsewhere (ballrolling), but activate here if necessary
'Const tnob = 10 ' total number of balls
'Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
'Dim tablewidth: tablewidth = Table1.width
'Dim tableheight: tableheight = Table1.height

' *** User Options - Uncomment here or move to top
'----- Shadow Options -----
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
'Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
'									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
'									'2 = flasher image shadow, but it moves like ninuzzu's

Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source


' *** This segment goes within the RollingUpdate sub, so that if Ambient...=0 and Dynamic...=0 the entire DynamicBSUpdate sub can be skipped for max performance
'	' stop the sound of deleted balls
'	For b = UBound(BOT) + 1 to tnob
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
'		...rolling(b) = False
'		...StopSound("BallRoll_" & b)
'	Next
'
'...rolling and drop sounds...

'		If DropCount(b) < 5 Then
'			DropCount(b) = DropCount(b) + 1
'		End If
'
'		' "Static" Ball Shadows
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

' *** Required Functions, enable these if they are not already present elswhere in your table
Function DistanceFast(x, y)
	dim ratio, ax, ay
	ax = abs(x)					'Get absolute value of each vector
	ay = abs(y)
	ratio = 1 / max(ax, ay)		'Create a ratio
	ratio = ratio * (1.29289 - (ax + ay) * ratio * 0.29289)
	if ratio > 0 then			'Quickly determine if it's worth using
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

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

'****** End Part B:  Code and Functions ******


'****** Part C:  The Magic ******
Dim sourcenames, DSSources(30), DSGISide(30), numberofsources, numberofsources_hold
sourcenames = Array ("","","","","","","","","","","","")

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!
dim objrtx1(12), objrtx2(12)
dim objBallShadow(12)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5,BallShadowA6,BallShadowA7,BallShadowA8,BallShadowA9,BallShadowA10,BallShadowA11)

DynamicBSInit

sub DynamicBSInit()
	Dim iii, source

	for iii = 0 to tnob									'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = 1 + iii/1000 + 0.01
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
		If Instr(Source.name , "Left") > 0 Then DSGISide(iii) = 0 Else DSGISide(iii) = 1
		iii = iii + 1
	Next
	numberofsources = iii
	numberofsources_hold = iii
end sub


Sub DynamicBSUpdate
	Dim falloff: falloff = 150 'Max distance to light sources, can be changed if you have a reason
	Dim BOT: BOT = TZBalls ' All balls, use GetBalls() if not available through an array
	Dim ShadowOpacity1, ShadowOpacity2 
	Dim s, LSd, iii
	Dim dist1, dist2, src1, src2

	'Hide shadow of deleted balls
	For s = UBound(BOT) + 1 to tnob
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next

	If UBound(BOT) < lob Then Exit Sub		'No balls in play, exit

'The Magic happens now
	For s = lob to UBound(BOT)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		If AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If BOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If BOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = BOT(s).Y + BallSize/10 + fovY
				objBallShadow(s).visible = 1

				BallShadowA(s).X = BOT(s).X
				BallShadowA(s).Y = BOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif BOT(s).Z <= 30 And BOT(s).Z > 20 Then	'On pf, primitive only
				objBallShadow(s).visible = 1
				If BOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					objBallShadow(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				objBallShadow(s).Y = BOT(s).Y + fovY
				BallShadowA(s).visible = 0
			Else											'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If BOT(s).Z > 30 Then							'In a ramp
				BallShadowA(s).X = BOT(s).X
				BallShadowA(s).Y = BOT(s).Y + BallSize/5 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
				BallShadowA(s).visible = 1
			Elseif BOT(s).Z <= 30 And BOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If BOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((BOT(s).X) - (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + 5
				Else
					BallShadowA(s).X = ((BOT(s).X) + (Ballsize/10) + ((BOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) - 5
				End If
				BallShadowA(s).Y = BOT(s).Y + Ballsize/10 + fovY
				BallShadowA(s).height=BOT(s).z - BallSize/2 + 5
			Else											'Under pf
				BallShadowA(s).visible = 0
			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If BOT(s).Z < 30 Then 'And BOT(s).Y < (TableHeight - 200) Then 'Or BOT(s).Z > 105 Then		'Defining when and where (on the table) you can have dynamic shadows
				dist1 = falloff
				dist2 = falloff
				For iii = 0 to numberofsources - 1 ' Search the 2 nearest influencing lights
					'LSd = DistanceFast((BOT(s).x - DSSources(iii)(0)), (BOT(s).y - DSSources(iii)(1))) 'Calculating the Linear distance to the Source
					LSd = Distance(BOT(s).x, BOT(s).y, DSSources(iii)(0), DSSources(iii)(1)) 'Calculating the Linear distance to the Source
					'If LSd < dist1 And gilvl > 0 Then 'If the ball is within the falloff range of a light and light is on (we will set numberofsources to 0 when GI is off)
					If LSd < dist2 And ((DSGISide(iii) = 0 And Lampz.State(100)>0) Or (DSGISide(iii) = 1 And Lampz.State(104)>0)) Then ' Adapted for TZ with GI left / GI right
						dist2 = dist1
						dist1 = LSd
						src2 = src1
						src1 = iii
					End If
				Next
				ShadowOpacity1 = 0
				If dist1 < falloff Then
					objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
					'objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx1(s).rotz = AnglePP(DSSources(src1)(0), DSSources(src1)(1), BOT(s).X, BOT(s).Y) + 90
					ShadowOpacity1 = 1 - dist1 / falloff
					objrtx1(s).size_y = Wideness * ShadowOpacity1 + Thinness
					UpdateMaterial objrtx1(s).material,1,0,0,0,0,0,ShadowOpacity1*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx1(s).visible = 0
				End If
				ShadowOpacity2 = 0
				If dist2 < falloff Then
					objrtx2(s).visible = 1 : objrtx2(s).X = BOT(s).X : objrtx2(s).Y = BOT(s).Y + fovY
					'objrtx2(s).Z = BOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx2(s).rotz = AnglePP(DSSources(src2)(0), DSSources(src2)(1), BOT(s).X, BOT(s).Y) + 90
					ShadowOpacity2 = 1 - dist2 / falloff
					objrtx2(s).size_y = Wideness * ShadowOpacity2 + Thinness
					UpdateMaterial objrtx2(s).material,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx2(s).visible = 0
				End If
				If AmbientBallShadowOn = 1 Then
					'Brightens the ambient primitive when it's close to a light
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(1 - max(ShadowOpacity1, ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					BallShadowA(s).Opacity = 100 * AmbientBSFactor * (1 - max(ShadowOpacity1, ShadowOpacity2))
				End If
			Else 'Hide dynamic shadows everywhere else
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub
'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************


'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
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

' Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit
	TargetBouncer activeball, 1
End Sub


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
	Dim BOT, b
	BOT = TZBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
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
		If AmbientBallShadowOn = 0 Then
			If BOT(b).Z > 30 Then
				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping
			Else
				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
			End If
			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + fovY
			BallShadowA(b).X = BOT(b).X
			BallShadowA(b).visible = 1
		End If
	Next
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
dim RampBalls(11,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(11)	

Sub WireRampOn(input) : Waddball ActiveBall, input : RampRollUpdate: End Sub
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

'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for this table.

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
'                      Supporting Ball & Sound Functions
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
	PlaySoundAtLevelStatic ("Start_Button"), StartButtonSoundLevel, Drain
End Sub

Sub SoundNudgeLeft()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound ("Nudge_1"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
		Case 2 : PlaySound ("Nudge_2"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
		Case 3 : PlaySound ("Nudge_3"), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
	End Select
End Sub

Sub SoundNudgeRight()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound ("Nudge_1"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
		Case 2 : PlaySound ("Nudge_2"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
		Case 3 : PlaySound ("Nudge_3"), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
	End Select
End Sub

Sub SoundNudgeCenter()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelStatic ("Nudge_1"), NudgeCenterSoundLevel * VolumeDial, sw18
		Case 2 : PlaySoundAtLevelStatic ("Nudge_2"), NudgeCenterSoundLevel * VolumeDial, sw18
		Case 3 : PlaySoundAtLevelStatic ("Nudge_3"), NudgeCenterSoundLevel * VolumeDial, sw18
	End Select
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
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic ("Drain_1"), DrainSoundLevel, drainswitch
		Case 2 : PlaySoundAtLevelStatic ("Drain_2"), DrainSoundLevel, drainswitch
		Case 3 : PlaySoundAtLevelStatic ("Drain_3"), DrainSoundLevel, drainswitch
		Case 4 : PlaySoundAtLevelStatic ("Drain_4"), DrainSoundLevel, drainswitch
		Case 5 : PlaySoundAtLevelStatic ("Drain_5"), DrainSoundLevel, drainswitch
		Case 6 : PlaySoundAtLevelStatic ("Drain_6"), DrainSoundLevel, drainswitch
		Case 7 : PlaySoundAtLevelStatic ("Drain_7"), DrainSoundLevel, drainswitch
		Case 8 : PlaySoundAtLevelStatic ("Drain_8"), DrainSoundLevel, drainswitch
		Case 9 : PlaySoundAtLevelStatic ("Drain_9"), DrainSoundLevel, drainswitch
		Case 10 : PlaySoundAtLevelStatic ("Drain_10"), DrainSoundLevel, drainswitch
		Case 11 : PlaySoundAtLevelStatic ("Drain_11"), DrainSoundLevel, drainswitch
	End Select
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBallRelease(drainswitch)
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("BallRelease1",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 2 : PlaySoundAtLevelStatic SoundFX("BallRelease2",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 3 : PlaySoundAtLevelStatic SoundFX("BallRelease3",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 4 : PlaySoundAtLevelStatic SoundFX("BallRelease4",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 5 : PlaySoundAtLevelStatic SoundFX("BallRelease5",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 6 : PlaySoundAtLevelStatic SoundFX("BallRelease6",DOFContactors), BallReleaseSoundLevel, drainswitch
		Case 7 : PlaySoundAtLevelStatic SoundFX("BallRelease7",DOFContactors), BallReleaseSoundLevel, drainswitch
	End Select
End Sub



'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundSlingshotLeft(sling)
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Sling_L1",DOFContactors), SlingshotSoundLevel, Sling
		Case 2 : PlaySoundAtLevelStatic SoundFX("Sling_L2",DOFContactors), SlingshotSoundLevel, Sling
		Case 3 : PlaySoundAtLevelStatic SoundFX("Sling_L3",DOFContactors), SlingshotSoundLevel, Sling
		Case 4 : PlaySoundAtLevelStatic SoundFX("Sling_L4",DOFContactors), SlingshotSoundLevel, Sling
		Case 5 : PlaySoundAtLevelStatic SoundFX("Sling_L5",DOFContactors), SlingshotSoundLevel, Sling
		Case 6 : PlaySoundAtLevelStatic SoundFX("Sling_L6",DOFContactors), SlingshotSoundLevel, Sling
		Case 7 : PlaySoundAtLevelStatic SoundFX("Sling_L7",DOFContactors), SlingshotSoundLevel, Sling
		Case 8 : PlaySoundAtLevelStatic SoundFX("Sling_L8",DOFContactors), SlingshotSoundLevel, Sling
		Case 9 : PlaySoundAtLevelStatic SoundFX("Sling_L9",DOFContactors), SlingshotSoundLevel, Sling
		Case 10 : PlaySoundAtLevelStatic SoundFX("Sling_L10",DOFContactors), SlingshotSoundLevel, Sling
	End Select
End Sub

Sub RandomSoundSlingshotRight(sling)
	Select Case Int(Rnd*8)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Sling_R1",DOFContactors), SlingshotSoundLevel, Sling
		Case 2 : PlaySoundAtLevelStatic SoundFX("Sling_R2",DOFContactors), SlingshotSoundLevel, Sling
		Case 3 : PlaySoundAtLevelStatic SoundFX("Sling_R3",DOFContactors), SlingshotSoundLevel, Sling
		Case 4 : PlaySoundAtLevelStatic SoundFX("Sling_R4",DOFContactors), SlingshotSoundLevel, Sling
		Case 5 : PlaySoundAtLevelStatic SoundFX("Sling_R5",DOFContactors), SlingshotSoundLevel, Sling
		Case 6 : PlaySoundAtLevelStatic SoundFX("Sling_R6",DOFContactors), SlingshotSoundLevel, Sling
		Case 7 : PlaySoundAtLevelStatic SoundFX("Sling_R7",DOFContactors), SlingshotSoundLevel, Sling
		Case 8 : PlaySoundAtLevelStatic SoundFX("Sling_R8",DOFContactors), SlingshotSoundLevel, Sling
	End Select
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////
Sub RandomSoundBumper(Bump)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
	End Select
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
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_L01",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_L02",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_L07",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_L08",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_L09",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_L10",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_L12",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_L14",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 9 : PlaySoundAtLevelStatic SoundFX("Flipper_L18",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 10 : PlaySoundAtLevelStatic SoundFX("Flipper_L20",DOFFlippers), FlipperLeftHitParm, Flipper
		Case 11 : PlaySoundAtLevelStatic SoundFX("Flipper_L26",DOFFlippers), FlipperLeftHitParm, Flipper
	End Select
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	Select Case Int(Rnd*11)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_R01",DOFFlippers), FlipperRightHitParm, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_R02",DOFFlippers), FlipperRightHitParm, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_R03",DOFFlippers), FlipperRightHitParm, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_R04",DOFFlippers), FlipperRightHitParm, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_R05",DOFFlippers), FlipperRightHitParm, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_R06",DOFFlippers), FlipperRightHitParm, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_R07",DOFFlippers), FlipperRightHitParm, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_R08",DOFFlippers), FlipperRightHitParm, Flipper
		Case 9 : PlaySoundAtLevelStatic SoundFX("Flipper_R09",DOFFlippers), FlipperRightHitParm, Flipper
		Case 10 : PlaySoundAtLevelStatic SoundFX("Flipper_R10",DOFFlippers), FlipperRightHitParm, Flipper
		Case 11 : PlaySoundAtLevelStatic SoundFX("Flipper_R11",DOFFlippers), FlipperRightHitParm, Flipper
	End Select
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L01",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L02",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L03",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundReflipUpRight(flipper)
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R01",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R02",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R03",DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_1",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_2",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_3",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_4",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_5",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_6",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_7",DOFFlippers), FlipperDownSoundLevel, Flipper
	End Select
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	Select Case Int(Rnd*8)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_1",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 2 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_2",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 3 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_3",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 4 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_4",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 5 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_5",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 6 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_6",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 7 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_7",DOFFlippers), FlipperDownSoundLevel, Flipper
		Case 8 : PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_8",DOFFlippers), FlipperDownSoundLevel, Flipper
	End Select
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
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_1"), parm  * RubberFlipperSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_2"), parm  * RubberFlipperSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_3"), parm  * RubberFlipperSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_4"), parm  * RubberFlipperSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_5"), parm  * RubberFlipperSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_6"), parm  * RubberFlipperSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Flipper_Rubber_7"), parm  * RubberFlipperSoundFactor
	End Select
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////
Sub RandomSoundRollover()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rollover_1"), RolloverSoundLevel
		Case 2 : PlaySoundAtLevelActiveBall ("Rollover_2"), RolloverSoundLevel
		Case 3 : PlaySoundAtLevelActiveBall ("Rollover_3"), RolloverSoundLevel
		Case 4 : PlaySoundAtLevelActiveBall ("Rollover_4"), RolloverSoundLevel
	End Select
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

Sub PostRubbers_Hit(idx)
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
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_1"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_2"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_3"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_3"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_5"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_6"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_7"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_8"), Vol(ActiveBall) * RubberWeakSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_9"), Vol(ActiveBall) * RubberWeakSoundFactor
	End Select
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
	Select Case Int(Rnd*13)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Metal_Touch_1"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Metal_Touch_2"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Metal_Touch_3"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Metal_Touch_4"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 5 : PlaySoundAtLevelActiveBall ("Metal_Touch_5"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 6 : PlaySoundAtLevelActiveBall ("Metal_Touch_6"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 7 : PlaySoundAtLevelActiveBall ("Metal_Touch_7"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 8 : PlaySoundAtLevelActiveBall ("Metal_Touch_8"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 9 : PlaySoundAtLevelActiveBall ("Metal_Touch_9"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 10 : PlaySoundAtLevelActiveBall ("Metal_Touch_10"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 11 : PlaySoundAtLevelActiveBall ("Metal_Touch_11"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 12 : PlaySoundAtLevelActiveBall ("Metal_Touch_12"), Vol(ActiveBall) * MetalImpactSoundFactor
		Case 13 : PlaySoundAtLevelActiveBall ("Metal_Touch_13"), Vol(ActiveBall) * MetalImpactSoundFactor
	End Select
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
 		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_2"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
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
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_1"), BottomArchBallGuideSoundFactor * 0.25
		Case 2 : PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_2"), BottomArchBallGuideSoundFactor * 0.25
		Case 3 : PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_3"), BottomArchBallGuideSoundFactor * 0.25
	End Select
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
 		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Medium_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_2"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 3 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
		End Select
 	End If
	If finalspeed < 6 Then
 		Select Case Int(Rnd*7)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Soft_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Soft_2"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 3 : PlaySoundAtLevelActiveBall ("Apron_Soft_3"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 4 : PlaySoundAtLevelActiveBall ("Apron_Soft_4"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 5 : PlaySoundAtLevelActiveBall ("Apron_Soft_5"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 6 : PlaySoundAtLevelActiveBall ("Apron_Soft_6"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 7 : PlaySoundAtLevelActiveBall ("Apron_Soft_7"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////
Sub RandomSoundTargetHitStrong()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_5",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_6",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_7",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_8",DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor		
	End Select
End Sub

Sub RandomSoundTargetHitWeak()
	Select Case Int(Rnd*4)+1		
		Case 1 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_1",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_2",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_3",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall SoundFX("Target_Hit_4",DOFTargets), Vol(ActiveBall) * TargetSoundFactor
	End Select
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
	Select Case Int(Rnd*7)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_3"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_4"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_6"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
	End Select
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
	Select Case Int(Rnd*2)+1				
		Case 1 : PlaySoundAtLevelStatic ("Gate_FastTrigger_1"), GateSoundLevel, Activeball
		Case 2 : PlaySoundAtLevelStatic ("Gate_FastTrigger_2"), GateSoundLevel, Activeball
	End Select
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, Activeball
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

'/////////////////////////////  JP'S VP10 BALL COLLISION SOUND  ////////////////////////////
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











Sub UpdateGI(no, step)
	Lampz.state(100 + no) = step
End Sub

'******************************************************
'****  LAMPZ by nFozzy
'******************************************************
' 
' Lampz is a utility designed to manage and fade the lights and light-related objects on a table that is being driven by a ROM.
' To set up Lampz, one must populate the Lampz.MassAssign array with VPX Light objects, where the index of the MassAssign array
' corrisponds to the ROM index of the associated light. More that one Light object can be associated with a single MassAssign index (not shown in this example)
' Optionally, callbacks can be assigned for each index using the Lampz.Callback array. This is very useful for allowing 3D Insert primitives
' to be controlled by the ROM. Note, the aLvl parameter (i.e. the fading level that ranges between 0 and 1) is appended to the callback call.

Sub SetModLamp(id, val)
	Lampz.state(id) = val
End Sub

Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
InitLampsNF              	' Setup lamp assignments
LampTimer.Interval = 16		' Using fixed value so the fading speed is same for every fps
LampTimer.Enabled = 1

Sub LampTimer_Timer()
	dim x, chglamp
	chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	Lampz.Update1	'update (fading logic only)
End Sub

dim FrameTime, InitFrameTime : InitFrameTime = 0
LampTimer2.Interval = -1
LampTimer2.Enabled = True
Sub LampTimer2_Timer()
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
	Lampz.Update 'updates on frametime (Object updates only)
End Sub

Function FlashLevelToIndex(Input, MaxSize)
	FlashLevelToIndex = cInt(MaxSize * Input)
End Function

'***Material Swap***
'Fade material for green, red, yellow colored Bulb prims
Sub FadeMaterialColoredBulb(pri, group, ByVal aLvl)	'cp's script
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
		Case 3:pri.Material = group(3) 'Full
	End Select
	'if tb.text <> pri.image then tb.text = pri.image : 'debug.print pri.image end If	'debug
	pri.blenddisablelighting = aLvl * 1 'Intensity Adjustment
End Sub

'Fade material for red, yellow colored bulb Filiment prims
Sub FadeMaterialColoredFiliment(pri, group, ByVal aLvl)	'cp's script
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
		Case 3:pri.Material = group(3) 'Full
	End Select
	'if tb.text <> pri.image then tb.text = pri.image : 'debug.print pri.image end If	'debug
	pri.blenddisablelighting = aLvl * 50  'Intensity Adjustment
End Sub

Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub

Sub UpdateLightMap(lightmap, intensity, ByVal aLvl)
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	lightmap.Opacity = aLvl * intensity
End Sub

Sub InitLampsNF()
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (max level / full MS fading time)
	Dim x
	for x = 0 to 150 : Lampz.FadeSpeedUp(x) = 1/5 : Lampz.FadeSpeedDown(x) = 1/20 : next
	' GI
	for x = 100 to 104 : Lampz.FadeSpeedUp(x) = 8/10 : Lampz.FadeSpeedDown(x) = 8/40 : Lampz.Modulate(x) = 1/8 : next
	' FLashers
	for x = 117 to 141 : Lampz.FadeSpeedUp(x) = 255/2 : Lampz.FadeSpeedDown(x) = 255/40 : Lampz.Modulate(x) = 1.5/255 : next
	' Mods (fast LEDs)
	for x = 105 to 111 : Lampz.FadeSpeedUp(x) = 1/2 : Lampz.FadeSpeedDown(x) = 1/8 : next
	' Hide all lights used for ball reflection only (only hide the direct halo and the transmission part, not the reflection on balls)
	For each x in BallReflections : x.visible = False : Next
	Lampz.FadeSpeedUp(150) = 100/1 : Lampz.FadeSpeedDown(150) = 100/1 : Lampz.Modulate(150) = 1/100


	' Lampz.MassAssign(150) = L150 ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Playfield_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Parts_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Over1_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Over2_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Over3_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Over4_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Over5_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap BR1_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap BR2_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap BR3_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap DiverterP_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap DiverterP1_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap FlipperL_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap FlipperL1_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap FlipperR_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap FlipperR1_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap GMKnob_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Gate1_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Gate2_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap LSling1_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap LSling2_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap RDiv_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap RSling1_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap RSling2_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap SLING1_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap SLING2_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap ShooterDiv_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw11_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw12_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw27_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw36_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw37_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw38_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw53p_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw54p_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw56_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw61_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw62_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw63_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap BumperPegs_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Camera_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Clock_Color_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Clock_White_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap ClockToy_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Gumballs_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap InvaderToy_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap MysticSeerToy_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Piano_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Pyramid_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Robot_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap RocketToy_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Sign_Spiral_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap SlotMachineToy_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap SpiralToy_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap TVtoy_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap TownSquarePost_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap URMagnet_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw47_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw48_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw64_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw65_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw65a_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw66_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw67_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw68_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw77_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw78_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw47m_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw48m_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw64m_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw65am_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw65m_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw67m_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw68m_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw77m_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap sw78m_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap FlipperSpL_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap FlipperSpL1_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap FlipperSpR_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap FlipperSpR1_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap SideMod_LM_Lit_Room, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap SideMod_LM_Lit_Room_001, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Parts_LM_Lit_Room_001, 100.0, " ' VLM.Lampz;Lit Room
	Lampz.Callback(150) = "UpdateLightMap Parts_LM_Lit_Room_002, 100.0, " ' VLM.Lampz;Lit Room

	' GI strings
	Lampz.MassAssign(100) = GiLeft001
	Lampz.MassAssign(100) = GiLeft002
	Lampz.MassAssign(100) = GiLeft003
	Lampz.MassAssign(100) = GiLeft004
	Lampz.MassAssign(100) = GiLeft005
	Lampz.MassAssign(100) = GiLeft006
	Lampz.MassAssign(100) = GiLeft007
	Lampz.MassAssign(100) = GiLeft008
	Lampz.MassAssign(100) = GiLeft009
	Lampz.MassAssign(100) = GiLeft010
	Lampz.MassAssign(101) = GIMiniPF001
	Lampz.MassAssign(104) = GiRight001
	Lampz.MassAssign(104) = GiRight002
	Lampz.MassAssign(104) = GiRight003
	Lampz.MassAssign(104) = GiRight004
	Lampz.MassAssign(104) = GiRight005
	Lampz.MassAssign(104) = GiRight006
	Lampz.MassAssign(104) = GiRight007
	Lampz.MassAssign(104) = GiRight008
	Lampz.MassAssign(104) = GiRight009
	' Lampz.MassAssign(100) = l100 ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap Playfield_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap Parts_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap Over1_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap Over2_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap Over3_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap Over4_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap Over5_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap BR1_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap BR2_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap BR3_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap DiverterP1_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap FlipperL_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap FlipperL1_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap FlipperR_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap LSling1_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap LSling2_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap RDiv_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap RSling1_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap RSling2_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap SLING1_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap SLING2_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap sw36_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap sw37_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap sw38_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap sw53p_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap BumperPegs_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap ClockToy_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap Gumballs_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap InvaderToy_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap MysticSeerToy_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap Piano_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap Robot_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap RocketToy_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap Sign_Spiral_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap SlotMachineToy_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap SpiralToy_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap TownSquarePost_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap sw48_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap sw77_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap sw48m_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap FlipperSpL_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap FlipperSpL1_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap FlipperSpR_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap SideMod_LM_GI_Left, 100.0, " ' VLM.Lampz;GI Left
	Lampz.Callback(100) = "UpdateLightMap Parts_LM_GI_Left_001, 100.0, " ' VLM.Lampz;GI Left
	' Lampz.MassAssign(101) = l101 ' VLM.Lampz;GI MiniPF
	Lampz.Callback(101) = "UpdateLightMap Playfield_LM_GI_MiniPF, 100.0, " ' VLM.Lampz;GI MiniPF
	Lampz.Callback(101) = "UpdateLightMap Parts_LM_GI_MiniPF, 100.0, " ' VLM.Lampz;GI MiniPF
	Lampz.Callback(101) = "UpdateLightMap Over1_LM_GI_MiniPF, 100.0, " ' VLM.Lampz;GI MiniPF
	Lampz.Callback(101) = "UpdateLightMap FlipperL1_LM_GI_MiniPF, 100.0, " ' VLM.Lampz;GI MiniPF
	Lampz.Callback(101) = "UpdateLightMap GMKnob_LM_GI_MiniPF, 100.0, " ' VLM.Lampz;GI MiniPF
	Lampz.Callback(101) = "UpdateLightMap ClockToy_LM_GI_MiniPF, 100.0, " ' VLM.Lampz;GI MiniPF
	Lampz.Callback(101) = "UpdateLightMap Piano_LM_GI_MiniPF, 100.0, " ' VLM.Lampz;GI MiniPF
	Lampz.Callback(101) = "UpdateLightMap Pyramid_LM_GI_MiniPF, 100.0, " ' VLM.Lampz;GI MiniPF
	Lampz.Callback(101) = "UpdateLightMap Sign_Spiral_LM_GI_MiniPF, 100.0, " ' VLM.Lampz;GI MiniPF
	Lampz.Callback(101) = "UpdateLightMap sw64_LM_GI_MiniPF, 100.0, " ' VLM.Lampz;GI MiniPF
	Lampz.Callback(101) = "UpdateLightMap sw66_LM_GI_MiniPF, 100.0, " ' VLM.Lampz;GI MiniPF
	Lampz.Callback(101) = "UpdateLightMap FlipperSpL1_LM_GI_MiniPF, 100.0, " ' VLM.Lampz;GI MiniPF
	Lampz.Callback(101) = "UpdateLightMap SideMod_LM_GI_MiniPF, 100.0, " ' VLM.Lampz;GI MiniPF
	' BG MiniPF (101)
	Lampz.Callback(101) = "UpdateLightMap VRBGGI007, 100.0, " ' VLM.Lampz;GI BG MiniPF
	Lampz.Callback(101) = "UpdateLightMap VRBGGI009, 100.0, " ' VLM.Lampz;GI BG MiniPF
	Lampz.Callback(101) = "UpdateLightMap VRBGGI011, 50.0, " ' VLM.Lampz;GI BG MiniPF
	Lampz.Callback(101) = "UpdateLightMap VRBGGI012, 50.0, " ' VLM.Lampz;GI BG MiniPF
	Lampz.Callback(101) = "UpdateLightMap VRBGGI013, 50.0, " ' VLM.Lampz;GI BG MiniPF
	Lampz.Callback(101) = "UpdateLightMap VRBGGI014, 35.0, " ' VLM.Lampz;GI BG MiniPF
	Lampz.Callback(101) = "UpdateLightMap VRBGGI015, 35.0, " ' VLM.Lampz;GI BG MiniPF
	Lampz.Callback(101) = "UpdateLightMap VRBGGI016, 50.0, " ' VLM.Lampz;GI BG MiniPF
	Lampz.Callback(101) = "UpdateLightMap VRBGGI019, 50.0, " ' VLM.Lampz;GI BG MiniPF
	Lampz.Callback(101) = "UpdateLightMap VRBGGI020, 50.0, " ' VLM.Lampz;GI BG MiniPF
	Lampz.Callback(101) = "UpdateLightMap VRBGGI021, 50.0, " ' VLM.Lampz;GI BG MiniPF
	Lampz.Callback(101) = "UpdateLightMap VRBGGI022, 50.0, " ' VLM.Lampz;GI BG MiniPF
	Lampz.Callback(101) = "UpdateLightMap VRBGGI038, 75.0, " ' VLM.Lampz;GI BG MiniPF
	Lampz.Callback(101) = "UpdateLightMap VRBGGI039, 75.0, " ' VLM.Lampz;GI BG MiniPF
	Lampz.Callback(101) = "UpdateLightMap VRBGGI040, 75.0, " ' VLM.Lampz;GI BG MiniPF
	Lampz.Callback(101) = "UpdateLightMap BGBright, 100.0, " ' VLM.Lampz;GI BG Main
	' Lampz.MassAssign(102) = l102 ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap Playfield_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap Parts_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap Over1_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap Over2_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap Over3_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap FlipperL_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap FlipperL1_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap FlipperR_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap FlipperR1_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap Clock_Color_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap Clock_White_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap ClockToy_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap Piano_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap SlotMachineToy_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap sw65_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap sw65a_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap sw66_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap sw64m_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap sw65am_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap sw65m_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap FlipperSpL_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap FlipperSpL1_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap FlipperSpR_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	Lampz.Callback(102) = "UpdateLightMap FlipperSpR1_LM_GI_Clock, 100.0, " ' VLM.Lampz;GI Clock
	' GI BG Clock(102)
	Lampz.Callback(102) = "UpdateLightMap VRBGGI028, 40.0, " ' VLM.Lampz;GI BG Clock
	Lampz.Callback(102) = "UpdateLightMap VRBGGI029, 40.0, " ' VLM.Lampz;GI BG Clock
	Lampz.Callback(102) = "UpdateLightMap VRBGGI030, 40.0, " ' VLM.Lampz;GI BG Clock
	Lampz.Callback(102) = "UpdateLightMap VRBGGI031, 40.0, " ' VLM.Lampz;GI BG Clock
	Lampz.Callback(102) = "UpdateLightMap VRBGGI032, 40.0, " ' VLM.Lampz;GI BG Clock
	Lampz.Callback(102) = "UpdateLightMap VRBGGI033, 40.0, " ' VLM.Lampz;GI BG Clock
	Lampz.Callback(102) = "UpdateLightMap VRBGGI034, 40.0, " ' VLM.Lampz;GI BG Clock
	Lampz.Callback(102) = "UpdateLightMap VRBGGI036, 130.0, " ' VLM.Lampz;GI BG Clock
	Lampz.Callback(102) = "UpdateLightMap VRBGGI035, 100.0, " ' VLM.Lampz;GI BG Clock
	Lampz.Callback(102) = "UpdateLightMap VRBGGI002, 100.0, " ' VLM.Lampz;GI BG Clock 
	Lampz.Callback(102) = "UpdateLightMap VRBGGI037, 100.0, " ' VLM.Lampz;GI BG Clock
'	' GI BG Main (103)
	Lampz.Callback(103) = "UpdateLightMap VRBGGI003, 100.0, " ' VLM.Lampz;GI BG Main
	Lampz.Callback(103) = "UpdateLightMap VRBGGI004, 100.0, " ' VLM.Lampz;GI BG Main
	Lampz.Callback(103) = "UpdateLightMap VRBGGI005, 100.0, " ' VLM.Lampz;GI BG Main
	Lampz.Callback(103) = "UpdateLightMap VRBGGI006, 100.0, " ' VLM.Lampz;GI BG Main
	Lampz.Callback(103) = "UpdateLightMap VRBGGI008, 100.0, " ' VLM.Lampz;GI BG Main
	Lampz.Callback(103) = "UpdateLightMap VRBGGI010, 100.0, " ' VLM.Lampz;GI BG Main
	Lampz.Callback(103) = "UpdateLightMap VRBGGI017, 100.0, " ' VLM.Lampz;GI BG Main
	Lampz.Callback(103) = "UpdateLightMap VRBGGI018, 100.0, " ' VLM.Lampz;GI BG Main
	Lampz.Callback(103) = "UpdateLightMap VRBGGI023, 100.0, " ' VLM.Lampz;GI BG Main
	Lampz.Callback(103) = "UpdateLightMap VRBGGI024, 125.0, " ' VLM.Lampz;GI BG Main
	Lampz.Callback(103) = "UpdateLightMap VRBGGI025, 100.0, " ' VLM.Lampz;GI BG Main
	Lampz.Callback(103) = "UpdateLightMap VRBGGI026, 125.0, " ' VLM.Lampz;GI BG Main
	Lampz.Callback(103) = "UpdateLightMap VRBGGI027, 100.0, " ' VLM.Lampz;GI BG Main
	Lampz.Callback(103) = "UpdateLightMap BGBright2, 1000.0, " ' VLM.Lampz;GI BG Main
	' Lampz.MassAssign(104) = l104 ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap Playfield_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap Parts_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap Over1_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap Over2_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap Over3_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap Over4_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap BR1_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap BR2_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap BR3_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap DiverterP_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap FlipperL_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap FlipperR_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap FlipperR1_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap LSling1_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap LSling2_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap RSling1_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap RSling2_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap SLING1_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap sw11_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap sw12_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap sw54p_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap BumperPegs_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap Clock_Color_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap Clock_White_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap ClockToy_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap InvaderToy_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap Piano_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap RocketToy_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap SlotMachineToy_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap TVtoy_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap TownSquarePost_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap URMagnet_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap sw48_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap sw65_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap sw65a_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap sw66_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap sw67_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap sw78_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap sw65am_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap sw65m_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap sw66m_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap sw67m_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap FlipperSpL_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap FlipperSpR_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap FlipperSpR1_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right
	Lampz.Callback(104) = "UpdateLightMap SideMod_LM_GI_Right, 100.0, " ' VLM.Lampz;GI Right

	' Mods
	' Lampz.MassAssign(105) = l105 ' VLM.Lampz;Mod-l105
	Lampz.Callback(105) = "UpdateLightMap InvaderToy_LM_Mod_l105, 100.0, " ' VLM.Lampz;Mod-l105
	' Lampz.MassAssign(106) = l106 ' VLM.Lampz;Mod-l106
	Lampz.Callback(106) = "UpdateLightMap Playfield_LM_Mod_l106, 100.0, " ' VLM.Lampz;Mod-l106
	Lampz.Callback(106) = "UpdateLightMap Parts_LM_Mod_l106, 100.0, " ' VLM.Lampz;Mod-l106
	Lampz.Callback(106) = "UpdateLightMap FlipperR1_LM_Mod_l106, 100.0, " ' VLM.Lampz;Mod-l106
	Lampz.Callback(106) = "UpdateLightMap InvaderToy_LM_Mod_l106, 100.0, " ' VLM.Lampz;Mod-l106
	Lampz.Callback(106) = "UpdateLightMap SlotMachineToy_LM_Mod_l106, 100.0, " ' VLM.Lampz;Mod-l106
	Lampz.Callback(106) = "UpdateLightMap FlipperSpR1_LM_Mod_l106, 100.0, " ' VLM.Lampz;Mod-l106
	' Lampz.MassAssign(107) = l107 ' VLM.Lampz;Mod-l107
	Lampz.Callback(107) = "UpdateLightMap Playfield_LM_Mod_l107, 100.0, " ' VLM.Lampz;Mod-l107
	Lampz.Callback(107) = "UpdateLightMap Parts_LM_Mod_l107, 100.0, " ' VLM.Lampz;Mod-l107
	Lampz.Callback(107) = "UpdateLightMap FlipperR1_LM_Mod_l107, 100.0, " ' VLM.Lampz;Mod-l107
	Lampz.Callback(107) = "UpdateLightMap InvaderToy_LM_Mod_l107, 100.0, " ' VLM.Lampz;Mod-l107
	Lampz.Callback(107) = "UpdateLightMap FlipperSpR1_LM_Mod_l107, 100.0, " ' VLM.Lampz;Mod-l107
	' Lampz.MassAssign(108) = l108 ' VLM.Lampz;Mod-l108
	Lampz.Callback(108) = "UpdateLightMap Parts_LM_Mod_l108, 100.0, " ' VLM.Lampz;Mod-l108
	' Lampz.MassAssign(109) = l109 ' VLM.Lampz;Mod-l109
	Lampz.Callback(109) = "UpdateLightMap Playfield_LM_Mod_l109, 100.0, " ' VLM.Lampz;Mod-l109
	Lampz.Callback(109) = "UpdateLightMap Parts_LM_Mod_l109, 100.0, " ' VLM.Lampz;Mod-l109
	Lampz.Callback(109) = "UpdateLightMap Over1_LM_Mod_l109, 100.0, " ' VLM.Lampz;Mod-l109
	Lampz.Callback(109) = "UpdateLightMap BR2_LM_Mod_l109, 100.0, " ' VLM.Lampz;Mod-l109
	Lampz.Callback(109) = "UpdateLightMap BR3_LM_Mod_l109, 100.0, " ' VLM.Lampz;Mod-l109
	Lampz.Callback(109) = "UpdateLightMap BumperPegs_LM_Mod_l109, 100.0, " ' VLM.Lampz;Mod-l109
	Lampz.Callback(109) = "UpdateLightMap TownSquarePost_LM_Mod_l109, 100.0, " ' VLM.Lampz;Mod-l109
	Lampz.Callback(109) = "UpdateLightMap sw77_LM_Mod_l109, 100.0, " ' VLM.Lampz;Mod-l109
	Lampz.Callback(109) = "UpdateLightMap sw78_LM_Mod_l109, 100.0, " ' VLM.Lampz;Mod-l109
	Lampz.Callback(109) = "UpdateLightMap sw77m_LM_Mod_l109, 100.0, " ' VLM.Lampz;Mod-l109
	' Lampz.MassAssign(110) = l110 ' VLM.Lampz;Mod-l110
	Lampz.Callback(110) = "UpdateLightMap Playfield_LM_Mod_l110, 100.0, " ' VLM.Lampz;Mod-l110
	Lampz.Callback(110) = "UpdateLightMap Parts_LM_Mod_l110, 100.0, " ' VLM.Lampz;Mod-l110
	Lampz.Callback(110) = "UpdateLightMap BR2_LM_Mod_l110, 100.0, " ' VLM.Lampz;Mod-l110
	Lampz.Callback(110) = "UpdateLightMap BR3_LM_Mod_l110, 100.0, " ' VLM.Lampz;Mod-l110
	Lampz.Callback(110) = "UpdateLightMap FlipperL1_LM_Mod_l110, 100.0, " ' VLM.Lampz;Mod-l110
	Lampz.Callback(110) = "UpdateLightMap Camera_LM_Mod_l110, 100.0, " ' VLM.Lampz;Mod-l110
	' Lampz.MassAssign(111) = l111 ' VLM.Lampz;Mod-l111
	Lampz.Callback(111) = "UpdateLightMap Playfield_LM_Mod_l111, 100.0, " ' VLM.Lampz;Mod-l111
	Lampz.Callback(111) = "UpdateLightMap Parts_LM_Mod_l111, 100.0, " ' VLM.Lampz;Mod-l111
	Lampz.Callback(111) = "UpdateLightMap Over1_LM_Mod_l111, 100.0, " ' VLM.Lampz;Mod-l111
	Lampz.Callback(111) = "UpdateLightMap FlipperR1_LM_Mod_l111, 100.0, " ' VLM.Lampz;Mod-l111
	Lampz.Callback(111) = "UpdateLightMap RSling1_LM_Mod_l111, 100.0, " ' VLM.Lampz;Mod-l111
	Lampz.Callback(111) = "UpdateLightMap RSling2_LM_Mod_l111, 100.0, " ' VLM.Lampz;Mod-l111
	Lampz.Callback(111) = "UpdateLightMap SLING1_LM_Mod_l111, 100.0, " ' VLM.Lampz;Mod-l111
	Lampz.Callback(111) = "UpdateLightMap InvaderToy_LM_Mod_l111, 100.0, " ' VLM.Lampz;Mod-l111
	Lampz.Callback(111) = "UpdateLightMap RocketToy_LM_Mod_l111, 100.0, " ' VLM.Lampz;Mod-l111
	Lampz.Callback(111) = "UpdateLightMap SlotMachineToy_LM_Mod_l111, 100.0, " ' VLM.Lampz;Mod-l111
	Lampz.Callback(111) = "UpdateLightMap FlipperSpR1_LM_Mod_l111, 100.0, " ' VLM.Lampz;Mod-l111

	' Flashers
	Lampz.MassAssign(117) = f17a
	Lampz.MassAssign(117) = f17b
	Lampz.MassAssign(118) = f18halo
	Lampz.MassAssign(120) = f20halo
	Lampz.MassAssign(128) = f28halo
	Lampz.MassAssign(141) = f41halo
	' Lampz.MassAssign(117) = f17 ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap Playfield_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap Parts_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap Over1_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap BR1_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap BR2_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap BR3_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap FlipperL1_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap FlipperR1_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap LSling1_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap RSling1_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap RSling2_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap sw36_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap sw37_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap sw38_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap BumperPegs_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap Camera_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap ClockToy_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap InvaderToy_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap MysticSeerToy_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap Piano_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap Pyramid_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap RocketToy_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap Sign_Spiral_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap SlotMachineToy_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap TownSquarePost_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap sw48_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap sw66_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap sw68_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap sw77_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap sw78_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap sw77m_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap sw78m_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap FlipperSpL1_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap FlipperSpR1_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.Callback(117) = "UpdateLightMap SideMod_LM_Flashers_f17, 100.0, " ' VLM.Lampz;Flashers-f17
	Lampz.MassAssign(118) = f18 ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Playfield_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Parts_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Over1_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Over2_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Over3_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Over4_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap BR2_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap BR3_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap DiverterP_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap FlipperL1_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap FlipperR1_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap GMKnob_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Gate1_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Gate2_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap LSling1_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap LSling2_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap sw11_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap sw12_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap sw53p_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap sw61_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap sw62_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap sw63_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap BumperPegs_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Camera_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Clock_Color_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Clock_White_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap ClockToy_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Gumballs_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap InvaderToy_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap MysticSeerToy_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Piano_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Pyramid_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Robot_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap RocketToy_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap Sign_Spiral_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap SlotMachineToy_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap SpiralToy_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap TVtoy_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap TownSquarePost_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap sw47_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap sw65a_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap sw68_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap sw77_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap sw78_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap sw64m_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap sw65am_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap sw65m_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap FlipperSpL1_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap FlipperSpR1_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.Callback(118) = "UpdateLightMap SideMod_LM_Flashers_f18, 100.0, " ' VLM.Lampz;Flashers-f18
	Lampz.MassAssign(119) = f19 ' VLM.Lampz;Flashers-f19
	Lampz.Callback(119) = "UpdateLightMap Parts_LM_Flashers_f19, 100.0, " ' VLM.Lampz;Flashers-f19
	Lampz.Callback(119) = "UpdateLightMap Over1_LM_Flashers_f19, 100.0, " ' VLM.Lampz;Flashers-f19
	Lampz.Callback(119) = "UpdateLightMap Pyramid_LM_Flashers_f19, 100.0, " ' VLM.Lampz;Flashers-f19
	Lampz.MassAssign(120) = f20 ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap Playfield_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap Parts_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap Over1_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap Over2_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap Over3_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap Over4_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap Over5_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap DiverterP_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap GMKnob_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap RDiv_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap sw53p_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap sw54p_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap Clock_Color_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap Clock_White_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap ClockToy_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap Gumballs_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap Piano_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap Pyramid_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap Robot_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap RocketToy_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap Sign_Spiral_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap SlotMachineToy_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap SpiralToy_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap TVtoy_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap URMagnet_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap sw65a_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap sw65am_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap sw65m_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.Callback(120) = "UpdateLightMap SideMod_LM_Flashers_f20, 100.0, " ' VLM.Lampz;Flashers-f20
	Lampz.MassAssign(128) = f28 ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Playfield_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Parts_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Over1_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Over2_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Over3_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Over4_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Over5_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap DiverterP_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap DiverterP1_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap FlipperL1_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap GMKnob_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap RDiv_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap sw11_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Camera_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Clock_Color_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Clock_White_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap ClockToy_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Gumballs_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Piano_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Pyramid_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Robot_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap RocketToy_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap Sign_Spiral_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap SlotMachineToy_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap SpiralToy_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap TVtoy_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap URMagnet_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap sw47_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap sw64_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap sw66_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap sw78_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap sw65am_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap sw65m_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap FlipperSpL1_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.Callback(128) = "UpdateLightMap SideMod_LM_Flashers_f28, 100.0, " ' VLM.Lampz;Flashers-f28
	Lampz.MassAssign(137) = f37 ' VLM.Lampz;Flashers-f37
	Lampz.Callback(137) = "UpdateLightMap Playfield_LM_Flashers_f37, 100.0, " ' VLM.Lampz;Flashers-f37
	Lampz.Callback(137) = "UpdateLightMap Parts_LM_Flashers_f37, 100.0, " ' VLM.Lampz;Flashers-f37
	Lampz.Callback(137) = "UpdateLightMap Over1_LM_Flashers_f37, 100.0, " ' VLM.Lampz;Flashers-f37
	Lampz.Callback(137) = "UpdateLightMap FlipperR1_LM_Flashers_f37, 100.0, " ' VLM.Lampz;Flashers-f37
	Lampz.Callback(137) = "UpdateLightMap sw62_LM_Flashers_f37, 100.0, " ' VLM.Lampz;Flashers-f37
	Lampz.Callback(137) = "UpdateLightMap sw63_LM_Flashers_f37, 100.0, " ' VLM.Lampz;Flashers-f37
	Lampz.Callback(137) = "UpdateLightMap FlipperSpR1_LM_Flashers_f37, 100.0, " ' VLM.Lampz;Flashers-f37
	Lampz.MassAssign(138) = f38 ' VLM.Lampz;Flashers-f38
	Lampz.Callback(138) = "UpdateLightMap Playfield_LM_Flashers_f38, 100.0, " ' VLM.Lampz;Flashers-f38
	Lampz.Callback(138) = "UpdateLightMap Parts_LM_Flashers_f38, 100.0, " ' VLM.Lampz;Flashers-f38
	Lampz.Callback(138) = "UpdateLightMap Over1_LM_Flashers_f38, 100.0, " ' VLM.Lampz;Flashers-f38
	Lampz.Callback(138) = "UpdateLightMap Over2_LM_Flashers_f38, 100.0, " ' VLM.Lampz;Flashers-f38
	Lampz.Callback(138) = "UpdateLightMap Over3_LM_Flashers_f38, 100.0, " ' VLM.Lampz;Flashers-f38
	Lampz.Callback(138) = "UpdateLightMap Over4_LM_Flashers_f38, 100.0, " ' VLM.Lampz;Flashers-f38
	Lampz.Callback(138) = "UpdateLightMap DiverterP_LM_Flashers_f38, 100.0, " ' VLM.Lampz;Flashers-f38
	Lampz.Callback(138) = "UpdateLightMap GMKnob_LM_Flashers_f38, 100.0, " ' VLM.Lampz;Flashers-f38
	Lampz.Callback(138) = "UpdateLightMap ClockToy_LM_Flashers_f38, 100.0, " ' VLM.Lampz;Flashers-f38
	Lampz.Callback(138) = "UpdateLightMap Gumballs_LM_Flashers_f38, 100.0, " ' VLM.Lampz;Flashers-f38
	Lampz.Callback(138) = "UpdateLightMap Piano_LM_Flashers_f38, 100.0, " ' VLM.Lampz;Flashers-f38
	Lampz.Callback(138) = "UpdateLightMap Pyramid_LM_Flashers_f38, 100.0, " ' VLM.Lampz;Flashers-f38
	Lampz.Callback(138) = "UpdateLightMap Robot_LM_Flashers_f38, 100.0, " ' VLM.Lampz;Flashers-f38
	Lampz.Callback(138) = "UpdateLightMap Sign_Spiral_LM_Flashers_f38, 100.0, " ' VLM.Lampz;Flashers-f38
	Lampz.Callback(138) = "UpdateLightMap SlotMachineToy_LM_Flashers_f38, 100.0, " ' VLM.Lampz;Flashers-f38
	Lampz.Callback(138) = "UpdateLightMap SpiralToy_LM_Flashers_f38, 100.0, " ' VLM.Lampz;Flashers-f38
	Lampz.Callback(138) = "UpdateLightMap SideMod_LM_Flashers_f38, 100.0, " ' VLM.Lampz;Flashers-f38
	Lampz.MassAssign(139) = f39 ' VLM.Lampz;Flashers-f39
	Lampz.Callback(139) = "UpdateLightMap Playfield_LM_Flashers_f39, 100.0, " ' VLM.Lampz;Flashers-f39
	Lampz.Callback(139) = "UpdateLightMap Parts_LM_Flashers_f39, 100.0, " ' VLM.Lampz;Flashers-f39
	Lampz.Callback(139) = "UpdateLightMap Over1_LM_Flashers_f39, 100.0, " ' VLM.Lampz;Flashers-f39
	Lampz.Callback(139) = "UpdateLightMap Over3_LM_Flashers_f39, 100.0, " ' VLM.Lampz;Flashers-f39
	Lampz.Callback(139) = "UpdateLightMap FlipperL1_LM_Flashers_f39, 100.0, " ' VLM.Lampz;Flashers-f39
	Lampz.Callback(139) = "UpdateLightMap ClockToy_LM_Flashers_f39, 100.0, " ' VLM.Lampz;Flashers-f39
	Lampz.Callback(139) = "UpdateLightMap Gumballs_LM_Flashers_f39, 100.0, " ' VLM.Lampz;Flashers-f39
	Lampz.Callback(139) = "UpdateLightMap Pyramid_LM_Flashers_f39, 100.0, " ' VLM.Lampz;Flashers-f39
	Lampz.Callback(139) = "UpdateLightMap Sign_Spiral_LM_Flashers_f39, 100.0, " ' VLM.Lampz;Flashers-f39
	Lampz.Callback(139) = "UpdateLightMap FlipperSpL1_LM_Flashers_f39, 100.0, " ' VLM.Lampz;Flashers-f39
	Lampz.Callback(139) = "UpdateLightMap SideMod_LM_Flashers_f39, 100.0, " ' VLM.Lampz;Flashers-f39
	Lampz.MassAssign(140) = f40 ' VLM.Lampz;Flashers-f40
	Lampz.Callback(140) = "UpdateLightMap Playfield_LM_Flashers_f40, 100.0, " ' VLM.Lampz;Flashers-f40
	Lampz.Callback(140) = "UpdateLightMap Parts_LM_Flashers_f40, 100.0, " ' VLM.Lampz;Flashers-f40
	Lampz.Callback(140) = "UpdateLightMap Over1_LM_Flashers_f40, 100.0, " ' VLM.Lampz;Flashers-f40
	Lampz.Callback(140) = "UpdateLightMap Over3_LM_Flashers_f40, 100.0, " ' VLM.Lampz;Flashers-f40
	Lampz.Callback(140) = "UpdateLightMap FlipperL1_LM_Flashers_f40, 100.0, " ' VLM.Lampz;Flashers-f40
	Lampz.Callback(140) = "UpdateLightMap GMKnob_LM_Flashers_f40, 100.0, " ' VLM.Lampz;Flashers-f40
	Lampz.Callback(140) = "UpdateLightMap RDiv_LM_Flashers_f40, 100.0, " ' VLM.Lampz;Flashers-f40
	Lampz.Callback(140) = "UpdateLightMap sw12_LM_Flashers_f40, 100.0, " ' VLM.Lampz;Flashers-f40
	Lampz.Callback(140) = "UpdateLightMap ClockToy_LM_Flashers_f40, 100.0, " ' VLM.Lampz;Flashers-f40
	Lampz.Callback(140) = "UpdateLightMap Gumballs_LM_Flashers_f40, 100.0, " ' VLM.Lampz;Flashers-f40
	Lampz.Callback(140) = "UpdateLightMap Piano_LM_Flashers_f40, 100.0, " ' VLM.Lampz;Flashers-f40
	Lampz.Callback(140) = "UpdateLightMap Pyramid_LM_Flashers_f40, 100.0, " ' VLM.Lampz;Flashers-f40
	Lampz.Callback(140) = "UpdateLightMap Robot_LM_Flashers_f40, 100.0, " ' VLM.Lampz;Flashers-f40
	Lampz.Callback(140) = "UpdateLightMap Sign_Spiral_LM_Flashers_f40, 100.0, " ' VLM.Lampz;Flashers-f40
	Lampz.Callback(140) = "UpdateLightMap SlotMachineToy_LM_Flashers_f40, 100.0, " ' VLM.Lampz;Flashers-f40
	Lampz.Callback(140) = "UpdateLightMap SpiralToy_LM_Flashers_f40, 100.0, " ' VLM.Lampz;Flashers-f40
	Lampz.Callback(140) = "UpdateLightMap FlipperSpL1_LM_Flashers_f40, 100.0, " ' VLM.Lampz;Flashers-f40
	Lampz.Callback(140) = "UpdateLightMap SideMod_LM_Flashers_f40, 100.0, " ' VLM.Lampz;Flashers-f40
	Lampz.MassAssign(141) = f41 ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap Playfield_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap Parts_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap Over1_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap Over2_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap Over3_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap Over4_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap DiverterP_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap DiverterP1_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap Gate2_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap RDiv_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap sw54p_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap Clock_Color_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap Clock_White_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap ClockToy_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap Piano_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap Pyramid_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap Robot_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap SlotMachineToy_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap TVtoy_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap URMagnet_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap sw65m_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41
	Lampz.Callback(141) = "UpdateLightMap SideMod_LM_Flashers_f41, 100.0, " ' VLM.Lampz;Flashers-f41

	' Inserts
	Lampz.MassAssign(11) = l11 ' VLM.Lampz;Inserts-l11
	Lampz.Callback(11) = "UpdateLightMap Playfield_LM_Inserts_l11, 100.0, " ' VLM.Lampz;Inserts-l11
	Lampz.Callback(11) = "UpdateLightMap FlipperL_LM_Inserts_l11, 100.0, " ' VLM.Lampz;Inserts-l11
	Lampz.Callback(11) = "UpdateLightMap FlipperR_LM_Inserts_l11, 100.0, " ' VLM.Lampz;Inserts-l11
	Lampz.Callback(11) = "UpdateLightMap FlipperSpL_LM_Inserts_l11, 100.0, " ' VLM.Lampz;Inserts-l11
	Lampz.Callback(11) = "UpdateLightMap FlipperSpR_LM_Inserts_l11, 100.0, " ' VLM.Lampz;Inserts-l11
	Lampz.MassAssign(12) = l12 ' VLM.Lampz;Inserts-l12
	Lampz.Callback(12) = "UpdateLightMap Playfield_LM_Inserts_l12, 100.0, " ' VLM.Lampz;Inserts-l12
	Lampz.MassAssign(13) = l13 ' VLM.Lampz;Inserts-l13
	Lampz.Callback(13) = "UpdateLightMap Playfield_LM_Inserts_l13, 100.0, " ' VLM.Lampz;Inserts-l13
	Lampz.Callback(13) = "UpdateLightMap Parts_LM_Inserts_l13, 100.0, " ' VLM.Lampz;Inserts-l13
	Lampz.Callback(13) = "UpdateLightMap Over1_LM_Inserts_l13, 100.0, " ' VLM.Lampz;Inserts-l13
	Lampz.Callback(13) = "UpdateLightMap FlipperL_LM_Inserts_l13, 100.0, " ' VLM.Lampz;Inserts-l13
	Lampz.Callback(13) = "UpdateLightMap LSling1_LM_Inserts_l13, 100.0, " ' VLM.Lampz;Inserts-l13
	Lampz.Callback(13) = "UpdateLightMap sw48_LM_Inserts_l13, 100.0, " ' VLM.Lampz;Inserts-l13
	Lampz.Callback(13) = "UpdateLightMap FlipperSpL_LM_Inserts_l13, 100.0, " ' VLM.Lampz;Inserts-l13
	Lampz.MassAssign(14) = l14 ' VLM.Lampz;Inserts-l14
	Lampz.Callback(14) = "UpdateLightMap Playfield_LM_Inserts_l14, 100.0, " ' VLM.Lampz;Inserts-l14
	Lampz.Callback(14) = "UpdateLightMap Parts_LM_Inserts_l14, 100.0, " ' VLM.Lampz;Inserts-l14
	Lampz.Callback(14) = "UpdateLightMap Over1_LM_Inserts_l14, 100.0, " ' VLM.Lampz;Inserts-l14
	Lampz.Callback(14) = "UpdateLightMap FlipperL_LM_Inserts_l14, 100.0, " ' VLM.Lampz;Inserts-l14
	Lampz.Callback(14) = "UpdateLightMap RocketToy_LM_Inserts_l14, 100.0, " ' VLM.Lampz;Inserts-l14
	Lampz.Callback(14) = "UpdateLightMap FlipperSpL_LM_Inserts_l14, 100.0, " ' VLM.Lampz;Inserts-l14
	Lampz.MassAssign(15) = l15 ' VLM.Lampz;Inserts-l15
	Lampz.Callback(15) = "UpdateLightMap Playfield_LM_Inserts_l15, 100.0, " ' VLM.Lampz;Inserts-l15
	Lampz.Callback(15) = "UpdateLightMap Parts_LM_Inserts_l15, 100.0, " ' VLM.Lampz;Inserts-l15
	Lampz.Callback(15) = "UpdateLightMap Over1_LM_Inserts_l15, 100.0, " ' VLM.Lampz;Inserts-l15
	Lampz.Callback(15) = "UpdateLightMap BR2_LM_Inserts_l15, 100.0, " ' VLM.Lampz;Inserts-l15
	Lampz.MassAssign(16) = l16 ' VLM.Lampz;Inserts-l16
	Lampz.Callback(16) = "UpdateLightMap Playfield_LM_Inserts_l16, 100.0, " ' VLM.Lampz;Inserts-l16
	Lampz.Callback(16) = "UpdateLightMap Parts_LM_Inserts_l16, 100.0, " ' VLM.Lampz;Inserts-l16
	Lampz.Callback(16) = "UpdateLightMap Over1_LM_Inserts_l16, 100.0, " ' VLM.Lampz;Inserts-l16
	Lampz.Callback(16) = "UpdateLightMap BR2_LM_Inserts_l16, 100.0, " ' VLM.Lampz;Inserts-l16
	Lampz.Callback(16) = "UpdateLightMap TownSquarePost_LM_Inserts_l16, 100.0, " ' VLM.Lampz;Inserts-l16
	Lampz.Callback(16) = "UpdateLightMap sw77_LM_Inserts_l16, 100.0, " ' VLM.Lampz;Inserts-l16
	Lampz.MassAssign(17) = l17 ' VLM.Lampz;Inserts-l17
	Lampz.Callback(17) = "UpdateLightMap Playfield_LM_Inserts_l17, 100.0, " ' VLM.Lampz;Inserts-l17
	Lampz.Callback(17) = "UpdateLightMap Parts_LM_Inserts_l17, 100.0, " ' VLM.Lampz;Inserts-l17
	Lampz.Callback(17) = "UpdateLightMap Over1_LM_Inserts_l17, 100.0, " ' VLM.Lampz;Inserts-l17
	Lampz.Callback(17) = "UpdateLightMap BR2_LM_Inserts_l17, 100.0, " ' VLM.Lampz;Inserts-l17
	Lampz.Callback(17) = "UpdateLightMap sw77_LM_Inserts_l17, 100.0, " ' VLM.Lampz;Inserts-l17
	Lampz.Callback(17) = "UpdateLightMap sw78_LM_Inserts_l17, 100.0, " ' VLM.Lampz;Inserts-l17
	Lampz.MassAssign(18) = l18 ' VLM.Lampz;Inserts-l18
	Lampz.Callback(18) = "UpdateLightMap Playfield_LM_Inserts_l18, 100.0, " ' VLM.Lampz;Inserts-l18
	Lampz.Callback(18) = "UpdateLightMap Parts_LM_Inserts_l18, 100.0, " ' VLM.Lampz;Inserts-l18
	Lampz.Callback(18) = "UpdateLightMap Over1_LM_Inserts_l18, 100.0, " ' VLM.Lampz;Inserts-l18
	Lampz.Callback(18) = "UpdateLightMap BR2_LM_Inserts_l18, 100.0, " ' VLM.Lampz;Inserts-l18
	Lampz.Callback(18) = "UpdateLightMap FlipperR1_LM_Inserts_l18, 100.0, " ' VLM.Lampz;Inserts-l18
	Lampz.Callback(18) = "UpdateLightMap FlipperSpR1_LM_Inserts_l18, 100.0, " ' VLM.Lampz;Inserts-l18
	Lampz.MassAssign(21) = l21 ' VLM.Lampz;Inserts-l21
	Lampz.Callback(21) = "UpdateLightMap Playfield_LM_Inserts_l21, 100.0, " ' VLM.Lampz;Inserts-l21
	Lampz.Callback(21) = "UpdateLightMap FlipperR1_LM_Inserts_l21, 100.0, " ' VLM.Lampz;Inserts-l21
	Lampz.Callback(21) = "UpdateLightMap FlipperSpR1_LM_Inserts_l21, 100.0, " ' VLM.Lampz;Inserts-l21
	Lampz.MassAssign(22) = l22 ' VLM.Lampz;Inserts-l22
	Lampz.Callback(22) = "UpdateLightMap Playfield_LM_Inserts_l22, 100.0, " ' VLM.Lampz;Inserts-l22
	Lampz.Callback(22) = "UpdateLightMap FlipperR1_LM_Inserts_l22, 100.0, " ' VLM.Lampz;Inserts-l22
	Lampz.Callback(22) = "UpdateLightMap RocketToy_LM_Inserts_l22, 100.0, " ' VLM.Lampz;Inserts-l22
	Lampz.MassAssign(23) = l23 ' VLM.Lampz;Inserts-l23
	Lampz.Callback(23) = "UpdateLightMap Playfield_LM_Inserts_l23, 100.0, " ' VLM.Lampz;Inserts-l23
	Lampz.Callback(23) = "UpdateLightMap FlipperR_LM_Inserts_l23, 100.0, " ' VLM.Lampz;Inserts-l23
	Lampz.Callback(23) = "UpdateLightMap FlipperR1_LM_Inserts_l23, 100.0, " ' VLM.Lampz;Inserts-l23
	Lampz.Callback(23) = "UpdateLightMap RSling1_LM_Inserts_l23, 100.0, " ' VLM.Lampz;Inserts-l23
	Lampz.Callback(23) = "UpdateLightMap RocketToy_LM_Inserts_l23, 100.0, " ' VLM.Lampz;Inserts-l23
	Lampz.Callback(23) = "UpdateLightMap FlipperSpR_LM_Inserts_l23, 100.0, " ' VLM.Lampz;Inserts-l23
	Lampz.Callback(23) = "UpdateLightMap FlipperSpR1_LM_Inserts_l23, 100.0, " ' VLM.Lampz;Inserts-l23
	Lampz.MassAssign(24) = l24 ' VLM.Lampz;Inserts-l24
	Lampz.Callback(24) = "UpdateLightMap Playfield_LM_Inserts_l24, 100.0, " ' VLM.Lampz;Inserts-l24
	Lampz.Callback(24) = "UpdateLightMap FlipperR_LM_Inserts_l24, 100.0, " ' VLM.Lampz;Inserts-l24
	Lampz.Callback(24) = "UpdateLightMap FlipperR1_LM_Inserts_l24, 100.0, " ' VLM.Lampz;Inserts-l24
	Lampz.Callback(24) = "UpdateLightMap RocketToy_LM_Inserts_l24, 100.0, " ' VLM.Lampz;Inserts-l24
	Lampz.Callback(24) = "UpdateLightMap FlipperSpR_LM_Inserts_l24, 100.0, " ' VLM.Lampz;Inserts-l24
	Lampz.Callback(24) = "UpdateLightMap FlipperSpR1_LM_Inserts_l24, 100.0, " ' VLM.Lampz;Inserts-l24
	Lampz.MassAssign(25) = l25 ' VLM.Lampz;Inserts-l25
	Lampz.Callback(25) = "UpdateLightMap Playfield_LM_Inserts_l25, 100.0, " ' VLM.Lampz;Inserts-l25
	Lampz.Callback(25) = "UpdateLightMap Parts_LM_Inserts_l25, 100.0, " ' VLM.Lampz;Inserts-l25
	Lampz.Callback(25) = "UpdateLightMap FlipperR1_LM_Inserts_l25, 100.0, " ' VLM.Lampz;Inserts-l25
	Lampz.Callback(25) = "UpdateLightMap FlipperSpR1_LM_Inserts_l25, 100.0, " ' VLM.Lampz;Inserts-l25
	Lampz.MassAssign(26) = l26 ' VLM.Lampz;Inserts-l26
	Lampz.Callback(26) = "UpdateLightMap Playfield_LM_Inserts_l26, 100.0, " ' VLM.Lampz;Inserts-l26
	Lampz.Callback(26) = "UpdateLightMap Parts_LM_Inserts_l26, 100.0, " ' VLM.Lampz;Inserts-l26
	Lampz.Callback(26) = "UpdateLightMap Over1_LM_Inserts_l26, 100.0, " ' VLM.Lampz;Inserts-l26
	Lampz.Callback(26) = "UpdateLightMap FlipperR1_LM_Inserts_l26, 100.0, " ' VLM.Lampz;Inserts-l26
	Lampz.Callback(26) = "UpdateLightMap FlipperSpR1_LM_Inserts_l26, 100.0, " ' VLM.Lampz;Inserts-l26
	Lampz.MassAssign(27) = l27 ' VLM.Lampz;Inserts-l27
	Lampz.Callback(27) = "UpdateLightMap Playfield_LM_Inserts_l27, 100.0, " ' VLM.Lampz;Inserts-l27
	Lampz.Callback(27) = "UpdateLightMap Parts_LM_Inserts_l27, 100.0, " ' VLM.Lampz;Inserts-l27
	Lampz.Callback(27) = "UpdateLightMap Over1_LM_Inserts_l27, 100.0, " ' VLM.Lampz;Inserts-l27
	Lampz.Callback(27) = "UpdateLightMap BR2_LM_Inserts_l27, 100.0, " ' VLM.Lampz;Inserts-l27
	Lampz.Callback(27) = "UpdateLightMap FlipperR1_LM_Inserts_l27, 100.0, " ' VLM.Lampz;Inserts-l27
	Lampz.Callback(27) = "UpdateLightMap FlipperSpR1_LM_Inserts_l27, 100.0, " ' VLM.Lampz;Inserts-l27
	Lampz.MassAssign(28) = l28 ' VLM.Lampz;Inserts-l28
	Lampz.Callback(28) = "UpdateLightMap Playfield_LM_Inserts_l28, 100.0, " ' VLM.Lampz;Inserts-l28
	Lampz.Callback(28) = "UpdateLightMap Parts_LM_Inserts_l28, 100.0, " ' VLM.Lampz;Inserts-l28
	Lampz.Callback(28) = "UpdateLightMap FlipperR1_LM_Inserts_l28, 100.0, " ' VLM.Lampz;Inserts-l28
	Lampz.Callback(28) = "UpdateLightMap FlipperSpR1_LM_Inserts_l28, 100.0, " ' VLM.Lampz;Inserts-l28
	Lampz.MassAssign(31) = l31 ' VLM.Lampz;Inserts-l31
	Lampz.Callback(31) = "UpdateLightMap Playfield_LM_Inserts_l31, 100.0, " ' VLM.Lampz;Inserts-l31
	Lampz.Callback(31) = "UpdateLightMap Parts_LM_Inserts_l31, 100.0, " ' VLM.Lampz;Inserts-l31
	Lampz.Callback(31) = "UpdateLightMap Over1_LM_Inserts_l31, 100.0, " ' VLM.Lampz;Inserts-l31
	Lampz.Callback(31) = "UpdateLightMap sw36_LM_Inserts_l31, 100.0, " ' VLM.Lampz;Inserts-l31
	Lampz.Callback(31) = "UpdateLightMap SideMod_LM_Inserts_l31, 100.0, " ' VLM.Lampz;Inserts-l31
	Lampz.MassAssign(32) = l32 ' VLM.Lampz;Inserts-l32
	Lampz.Callback(32) = "UpdateLightMap Playfield_LM_Inserts_l32, 100.0, " ' VLM.Lampz;Inserts-l32
	Lampz.Callback(32) = "UpdateLightMap Parts_LM_Inserts_l32, 100.0, " ' VLM.Lampz;Inserts-l32
	Lampz.Callback(32) = "UpdateLightMap Over1_LM_Inserts_l32, 100.0, " ' VLM.Lampz;Inserts-l32
	Lampz.MassAssign(33) = l33 ' VLM.Lampz;Inserts-l33
	Lampz.Callback(33) = "UpdateLightMap Playfield_LM_Inserts_l33, 100.0, " ' VLM.Lampz;Inserts-l33
	Lampz.Callback(33) = "UpdateLightMap Parts_LM_Inserts_l33, 100.0, " ' VLM.Lampz;Inserts-l33
	Lampz.Callback(33) = "UpdateLightMap Over1_LM_Inserts_l33, 100.0, " ' VLM.Lampz;Inserts-l33
	Lampz.Callback(33) = "UpdateLightMap BR3_LM_Inserts_l33, 100.0, " ' VLM.Lampz;Inserts-l33
	Lampz.Callback(33) = "UpdateLightMap sw37_LM_Inserts_l33, 100.0, " ' VLM.Lampz;Inserts-l33
	Lampz.Callback(33) = "UpdateLightMap MysticSeerToy_LM_Inserts_l33, 100.0, " ' VLM.Lampz;Inserts-l33
	Lampz.Callback(33) = "UpdateLightMap SideMod_LM_Inserts_l33, 100.0, " ' VLM.Lampz;Inserts-l33
	Lampz.MassAssign(34) = l34 ' VLM.Lampz;Inserts-l34
	Lampz.Callback(34) = "UpdateLightMap Playfield_LM_Inserts_l34, 100.0, " ' VLM.Lampz;Inserts-l34
	Lampz.Callback(34) = "UpdateLightMap Parts_LM_Inserts_l34, 100.0, " ' VLM.Lampz;Inserts-l34
	Lampz.Callback(34) = "UpdateLightMap Over1_LM_Inserts_l34, 100.0, " ' VLM.Lampz;Inserts-l34
	Lampz.Callback(34) = "UpdateLightMap FlipperR1_LM_Inserts_l34, 100.0, " ' VLM.Lampz;Inserts-l34
	Lampz.MassAssign(35) = l35 ' VLM.Lampz;Inserts-l35
	Lampz.Callback(35) = "UpdateLightMap Playfield_LM_Inserts_l35, 100.0, " ' VLM.Lampz;Inserts-l35
	Lampz.Callback(35) = "UpdateLightMap Parts_LM_Inserts_l35, 100.0, " ' VLM.Lampz;Inserts-l35
	Lampz.Callback(35) = "UpdateLightMap Over1_LM_Inserts_l35, 100.0, " ' VLM.Lampz;Inserts-l35
	Lampz.Callback(35) = "UpdateLightMap BR3_LM_Inserts_l35, 100.0, " ' VLM.Lampz;Inserts-l35
	Lampz.Callback(35) = "UpdateLightMap sw38_LM_Inserts_l35, 100.0, " ' VLM.Lampz;Inserts-l35
	Lampz.Callback(35) = "UpdateLightMap sw48_LM_Inserts_l35, 100.0, " ' VLM.Lampz;Inserts-l35
	Lampz.MassAssign(36) = l36 ' VLM.Lampz;Inserts-l36
	Lampz.Callback(36) = "UpdateLightMap Playfield_LM_Inserts_l36, 100.0, " ' VLM.Lampz;Inserts-l36
	Lampz.Callback(36) = "UpdateLightMap Parts_LM_Inserts_l36, 100.0, " ' VLM.Lampz;Inserts-l36
	Lampz.Callback(36) = "UpdateLightMap Over1_LM_Inserts_l36, 100.0, " ' VLM.Lampz;Inserts-l36
	Lampz.Callback(36) = "UpdateLightMap FlipperR1_LM_Inserts_l36, 100.0, " ' VLM.Lampz;Inserts-l36
	Lampz.Callback(36) = "UpdateLightMap FlipperSpR1_LM_Inserts_l36, 100.0, " ' VLM.Lampz;Inserts-l36
	Lampz.MassAssign(37) = l37 ' VLM.Lampz;Inserts-l37
	Lampz.Callback(37) = "UpdateLightMap Playfield_LM_Inserts_l37, 100.0, " ' VLM.Lampz;Inserts-l37
	Lampz.Callback(37) = "UpdateLightMap Parts_LM_Inserts_l37, 100.0, " ' VLM.Lampz;Inserts-l37
	Lampz.Callback(37) = "UpdateLightMap Over1_LM_Inserts_l37, 100.0, " ' VLM.Lampz;Inserts-l37
	Lampz.Callback(37) = "UpdateLightMap BR2_LM_Inserts_l37, 100.0, " ' VLM.Lampz;Inserts-l37
	Lampz.Callback(37) = "UpdateLightMap BR3_LM_Inserts_l37, 100.0, " ' VLM.Lampz;Inserts-l37
	Lampz.Callback(37) = "UpdateLightMap sw36_LM_Inserts_l37, 100.0, " ' VLM.Lampz;Inserts-l37
	Lampz.Callback(37) = "UpdateLightMap sw37_LM_Inserts_l37, 100.0, " ' VLM.Lampz;Inserts-l37
	Lampz.Callback(37) = "UpdateLightMap sw38_LM_Inserts_l37, 100.0, " ' VLM.Lampz;Inserts-l37
	Lampz.Callback(37) = "UpdateLightMap TownSquarePost_LM_Inserts_l37, 100.0, " ' VLM.Lampz;Inserts-l37
	Lampz.Callback(37) = "UpdateLightMap sw48_LM_Inserts_l37, 100.0, " ' VLM.Lampz;Inserts-l37
	Lampz.Callback(37) = "UpdateLightMap sw77_LM_Inserts_l37, 100.0, " ' VLM.Lampz;Inserts-l37
	Lampz.Callback(37) = "UpdateLightMap sw48m_LM_Inserts_l37, 100.0, " ' VLM.Lampz;Inserts-l37
	Lampz.MassAssign(38) = l38 ' VLM.Lampz;Inserts-l38
	Lampz.Callback(38) = "UpdateLightMap Playfield_LM_Inserts_l38, 100.0, " ' VLM.Lampz;Inserts-l38
	Lampz.Callback(38) = "UpdateLightMap Parts_LM_Inserts_l38, 100.0, " ' VLM.Lampz;Inserts-l38
	Lampz.Callback(38) = "UpdateLightMap Over1_LM_Inserts_l38, 100.0, " ' VLM.Lampz;Inserts-l38
	Lampz.Callback(38) = "UpdateLightMap BR2_LM_Inserts_l38, 100.0, " ' VLM.Lampz;Inserts-l38
	Lampz.Callback(38) = "UpdateLightMap BR3_LM_Inserts_l38, 100.0, " ' VLM.Lampz;Inserts-l38
	Lampz.Callback(38) = "UpdateLightMap BumperPegs_LM_Inserts_l38, 100.0, " ' VLM.Lampz;Inserts-l38
	Lampz.Callback(38) = "UpdateLightMap TownSquarePost_LM_Inserts_l38, 100.0, " ' VLM.Lampz;Inserts-l38
	Lampz.Callback(38) = "UpdateLightMap sw77_LM_Inserts_l38, 100.0, " ' VLM.Lampz;Inserts-l38
	Lampz.MassAssign(41) = l41 ' VLM.Lampz;Inserts-l41
	Lampz.Callback(41) = "UpdateLightMap Playfield_LM_Inserts_l41, 100.0, " ' VLM.Lampz;Inserts-l41
	Lampz.Callback(41) = "UpdateLightMap Parts_LM_Inserts_l41, 100.0, " ' VLM.Lampz;Inserts-l41
	Lampz.Callback(41) = "UpdateLightMap Over1_LM_Inserts_l41, 100.0, " ' VLM.Lampz;Inserts-l41
	Lampz.Callback(41) = "UpdateLightMap BR2_LM_Inserts_l41, 100.0, " ' VLM.Lampz;Inserts-l41
	Lampz.Callback(41) = "UpdateLightMap FlipperL_LM_Inserts_l41, 100.0, " ' VLM.Lampz;Inserts-l41
	Lampz.Callback(41) = "UpdateLightMap FlipperR_LM_Inserts_l41, 100.0, " ' VLM.Lampz;Inserts-l41
	Lampz.Callback(41) = "UpdateLightMap LSling1_LM_Inserts_l41, 100.0, " ' VLM.Lampz;Inserts-l41
	Lampz.Callback(41) = "UpdateLightMap RocketToy_LM_Inserts_l41, 100.0, " ' VLM.Lampz;Inserts-l41
	Lampz.Callback(41) = "UpdateLightMap FlipperSpL_LM_Inserts_l41, 100.0, " ' VLM.Lampz;Inserts-l41
	Lampz.Callback(41) = "UpdateLightMap FlipperSpR_LM_Inserts_l41, 100.0, " ' VLM.Lampz;Inserts-l41
	Lampz.MassAssign(42) = l42 ' VLM.Lampz;Inserts-l42
	Lampz.Callback(42) = "UpdateLightMap Playfield_LM_Inserts_l42, 100.0, " ' VLM.Lampz;Inserts-l42
	Lampz.Callback(42) = "UpdateLightMap Parts_LM_Inserts_l42, 100.0, " ' VLM.Lampz;Inserts-l42
	Lampz.Callback(42) = "UpdateLightMap Over1_LM_Inserts_l42, 100.0, " ' VLM.Lampz;Inserts-l42
	Lampz.Callback(42) = "UpdateLightMap BR2_LM_Inserts_l42, 100.0, " ' VLM.Lampz;Inserts-l42
	Lampz.Callback(42) = "UpdateLightMap FlipperL_LM_Inserts_l42, 100.0, " ' VLM.Lampz;Inserts-l42
	Lampz.Callback(42) = "UpdateLightMap FlipperR_LM_Inserts_l42, 100.0, " ' VLM.Lampz;Inserts-l42
	Lampz.Callback(42) = "UpdateLightMap LSling1_LM_Inserts_l42, 100.0, " ' VLM.Lampz;Inserts-l42
	Lampz.Callback(42) = "UpdateLightMap LSling2_LM_Inserts_l42, 100.0, " ' VLM.Lampz;Inserts-l42
	Lampz.Callback(42) = "UpdateLightMap RocketToy_LM_Inserts_l42, 100.0, " ' VLM.Lampz;Inserts-l42
	Lampz.Callback(42) = "UpdateLightMap FlipperSpL_LM_Inserts_l42, 100.0, " ' VLM.Lampz;Inserts-l42
	Lampz.Callback(42) = "UpdateLightMap FlipperSpR_LM_Inserts_l42, 100.0, " ' VLM.Lampz;Inserts-l42
	Lampz.MassAssign(43) = l43 ' VLM.Lampz;Inserts-l43
	Lampz.Callback(43) = "UpdateLightMap Playfield_LM_Inserts_l43, 100.0, " ' VLM.Lampz;Inserts-l43
	Lampz.Callback(43) = "UpdateLightMap Parts_LM_Inserts_l43, 100.0, " ' VLM.Lampz;Inserts-l43
	Lampz.Callback(43) = "UpdateLightMap Over1_LM_Inserts_l43, 100.0, " ' VLM.Lampz;Inserts-l43
	Lampz.Callback(43) = "UpdateLightMap BR2_LM_Inserts_l43, 100.0, " ' VLM.Lampz;Inserts-l43
	Lampz.Callback(43) = "UpdateLightMap FlipperL_LM_Inserts_l43, 100.0, " ' VLM.Lampz;Inserts-l43
	Lampz.Callback(43) = "UpdateLightMap FlipperR_LM_Inserts_l43, 100.0, " ' VLM.Lampz;Inserts-l43
	Lampz.Callback(43) = "UpdateLightMap LSling1_LM_Inserts_l43, 100.0, " ' VLM.Lampz;Inserts-l43
	Lampz.Callback(43) = "UpdateLightMap LSling2_LM_Inserts_l43, 100.0, " ' VLM.Lampz;Inserts-l43
	Lampz.Callback(43) = "UpdateLightMap RSling1_LM_Inserts_l43, 100.0, " ' VLM.Lampz;Inserts-l43
	Lampz.Callback(43) = "UpdateLightMap RSling2_LM_Inserts_l43, 100.0, " ' VLM.Lampz;Inserts-l43
	Lampz.Callback(43) = "UpdateLightMap RocketToy_LM_Inserts_l43, 100.0, " ' VLM.Lampz;Inserts-l43
	Lampz.Callback(43) = "UpdateLightMap sw48_LM_Inserts_l43, 100.0, " ' VLM.Lampz;Inserts-l43
	Lampz.Callback(43) = "UpdateLightMap FlipperSpL_LM_Inserts_l43, 100.0, " ' VLM.Lampz;Inserts-l43
	Lampz.Callback(43) = "UpdateLightMap FlipperSpR_LM_Inserts_l43, 100.0, " ' VLM.Lampz;Inserts-l43
	Lampz.MassAssign(44) = l44 ' VLM.Lampz;Inserts-l44
	Lampz.Callback(44) = "UpdateLightMap Playfield_LM_Inserts_l44, 100.0, " ' VLM.Lampz;Inserts-l44
	Lampz.Callback(44) = "UpdateLightMap Parts_LM_Inserts_l44, 100.0, " ' VLM.Lampz;Inserts-l44
	Lampz.Callback(44) = "UpdateLightMap Over1_LM_Inserts_l44, 100.0, " ' VLM.Lampz;Inserts-l44
	Lampz.Callback(44) = "UpdateLightMap BR2_LM_Inserts_l44, 100.0, " ' VLM.Lampz;Inserts-l44
	Lampz.Callback(44) = "UpdateLightMap FlipperL_LM_Inserts_l44, 100.0, " ' VLM.Lampz;Inserts-l44
	Lampz.Callback(44) = "UpdateLightMap FlipperR_LM_Inserts_l44, 100.0, " ' VLM.Lampz;Inserts-l44
	Lampz.Callback(44) = "UpdateLightMap LSling1_LM_Inserts_l44, 100.0, " ' VLM.Lampz;Inserts-l44
	Lampz.Callback(44) = "UpdateLightMap LSling2_LM_Inserts_l44, 100.0, " ' VLM.Lampz;Inserts-l44
	Lampz.Callback(44) = "UpdateLightMap RSling1_LM_Inserts_l44, 100.0, " ' VLM.Lampz;Inserts-l44
	Lampz.Callback(44) = "UpdateLightMap RSling2_LM_Inserts_l44, 100.0, " ' VLM.Lampz;Inserts-l44
	Lampz.Callback(44) = "UpdateLightMap RocketToy_LM_Inserts_l44, 100.0, " ' VLM.Lampz;Inserts-l44
	Lampz.Callback(44) = "UpdateLightMap sw48_LM_Inserts_l44, 100.0, " ' VLM.Lampz;Inserts-l44
	Lampz.Callback(44) = "UpdateLightMap FlipperSpL_LM_Inserts_l44, 100.0, " ' VLM.Lampz;Inserts-l44
	Lampz.Callback(44) = "UpdateLightMap FlipperSpR_LM_Inserts_l44, 100.0, " ' VLM.Lampz;Inserts-l44
	Lampz.MassAssign(45) = l45 ' VLM.Lampz;Inserts-l45
	Lampz.Callback(45) = "UpdateLightMap Playfield_LM_Inserts_l45, 100.0, " ' VLM.Lampz;Inserts-l45
	Lampz.Callback(45) = "UpdateLightMap Parts_LM_Inserts_l45, 100.0, " ' VLM.Lampz;Inserts-l45
	Lampz.Callback(45) = "UpdateLightMap Over1_LM_Inserts_l45, 100.0, " ' VLM.Lampz;Inserts-l45
	Lampz.Callback(45) = "UpdateLightMap FlipperL_LM_Inserts_l45, 100.0, " ' VLM.Lampz;Inserts-l45
	Lampz.Callback(45) = "UpdateLightMap FlipperR_LM_Inserts_l45, 100.0, " ' VLM.Lampz;Inserts-l45
	Lampz.Callback(45) = "UpdateLightMap LSling1_LM_Inserts_l45, 100.0, " ' VLM.Lampz;Inserts-l45
	Lampz.Callback(45) = "UpdateLightMap RSling1_LM_Inserts_l45, 100.0, " ' VLM.Lampz;Inserts-l45
	Lampz.Callback(45) = "UpdateLightMap RSling2_LM_Inserts_l45, 100.0, " ' VLM.Lampz;Inserts-l45
	Lampz.Callback(45) = "UpdateLightMap RocketToy_LM_Inserts_l45, 100.0, " ' VLM.Lampz;Inserts-l45
	Lampz.Callback(45) = "UpdateLightMap sw48_LM_Inserts_l45, 100.0, " ' VLM.Lampz;Inserts-l45
	Lampz.Callback(45) = "UpdateLightMap FlipperSpL_LM_Inserts_l45, 100.0, " ' VLM.Lampz;Inserts-l45
	Lampz.Callback(45) = "UpdateLightMap FlipperSpR_LM_Inserts_l45, 100.0, " ' VLM.Lampz;Inserts-l45
	Lampz.MassAssign(46) = l46 ' VLM.Lampz;Inserts-l46
	Lampz.Callback(46) = "UpdateLightMap Playfield_LM_Inserts_l46, 100.0, " ' VLM.Lampz;Inserts-l46
	Lampz.Callback(46) = "UpdateLightMap Parts_LM_Inserts_l46, 100.0, " ' VLM.Lampz;Inserts-l46
	Lampz.Callback(46) = "UpdateLightMap FlipperL_LM_Inserts_l46, 100.0, " ' VLM.Lampz;Inserts-l46
	Lampz.Callback(46) = "UpdateLightMap FlipperR_LM_Inserts_l46, 100.0, " ' VLM.Lampz;Inserts-l46
	Lampz.Callback(46) = "UpdateLightMap FlipperSpL_LM_Inserts_l46, 100.0, " ' VLM.Lampz;Inserts-l46
	Lampz.Callback(46) = "UpdateLightMap FlipperSpR_LM_Inserts_l46, 100.0, " ' VLM.Lampz;Inserts-l46
	Lampz.MassAssign(47) = l47 ' VLM.Lampz;Inserts-l47
	Lampz.Callback(47) = "UpdateLightMap Playfield_LM_Inserts_l47, 100.0, " ' VLM.Lampz;Inserts-l47
	Lampz.Callback(47) = "UpdateLightMap Parts_LM_Inserts_l47, 100.0, " ' VLM.Lampz;Inserts-l47
	Lampz.Callback(47) = "UpdateLightMap FlipperL_LM_Inserts_l47, 100.0, " ' VLM.Lampz;Inserts-l47
	Lampz.Callback(47) = "UpdateLightMap FlipperR_LM_Inserts_l47, 100.0, " ' VLM.Lampz;Inserts-l47
	Lampz.Callback(47) = "UpdateLightMap FlipperSpL_LM_Inserts_l47, 100.0, " ' VLM.Lampz;Inserts-l47
	Lampz.Callback(47) = "UpdateLightMap FlipperSpR_LM_Inserts_l47, 100.0, " ' VLM.Lampz;Inserts-l47
	Lampz.MassAssign(48) = l48 ' VLM.Lampz;Inserts-l48
	Lampz.Callback(48) = "UpdateLightMap Playfield_LM_Inserts_l48, 100.0, " ' VLM.Lampz;Inserts-l48
	Lampz.Callback(48) = "UpdateLightMap Parts_LM_Inserts_l48, 100.0, " ' VLM.Lampz;Inserts-l48
	Lampz.Callback(48) = "UpdateLightMap Over1_LM_Inserts_l48, 100.0, " ' VLM.Lampz;Inserts-l48
	Lampz.Callback(48) = "UpdateLightMap FlipperR_LM_Inserts_l48, 100.0, " ' VLM.Lampz;Inserts-l48
	Lampz.Callback(48) = "UpdateLightMap RocketToy_LM_Inserts_l48, 100.0, " ' VLM.Lampz;Inserts-l48
	Lampz.Callback(48) = "UpdateLightMap FlipperSpR_LM_Inserts_l48, 100.0, " ' VLM.Lampz;Inserts-l48
	Lampz.MassAssign(51) = l51 ' VLM.Lampz;Inserts-l51
	Lampz.Callback(51) = "UpdateLightMap Playfield_LM_Inserts_l51, 100.0, " ' VLM.Lampz;Inserts-l51
	Lampz.Callback(51) = "UpdateLightMap Parts_LM_Inserts_l51, 100.0, " ' VLM.Lampz;Inserts-l51
	Lampz.Callback(51) = "UpdateLightMap Over2_LM_Inserts_l51, 100.0, " ' VLM.Lampz;Inserts-l51
	Lampz.Callback(51) = "UpdateLightMap FlipperL1_LM_Inserts_l51, 100.0, " ' VLM.Lampz;Inserts-l51
	Lampz.Callback(51) = "UpdateLightMap ClockToy_LM_Inserts_l51, 100.0, " ' VLM.Lampz;Inserts-l51
	Lampz.Callback(51) = "UpdateLightMap sw47_LM_Inserts_l51, 100.0, " ' VLM.Lampz;Inserts-l51
	Lampz.Callback(51) = "UpdateLightMap FlipperSpL1_LM_Inserts_l51, 100.0, " ' VLM.Lampz;Inserts-l51
	Lampz.MassAssign(52) = l52 ' VLM.Lampz;Inserts-l52
	Lampz.Callback(52) = "UpdateLightMap Playfield_LM_Inserts_l52, 100.0, " ' VLM.Lampz;Inserts-l52
	Lampz.Callback(52) = "UpdateLightMap Parts_LM_Inserts_l52, 100.0, " ' VLM.Lampz;Inserts-l52
	Lampz.Callback(52) = "UpdateLightMap Over2_LM_Inserts_l52, 100.0, " ' VLM.Lampz;Inserts-l52
	Lampz.Callback(52) = "UpdateLightMap FlipperL1_LM_Inserts_l52, 100.0, " ' VLM.Lampz;Inserts-l52
	Lampz.Callback(52) = "UpdateLightMap ClockToy_LM_Inserts_l52, 100.0, " ' VLM.Lampz;Inserts-l52
	Lampz.Callback(52) = "UpdateLightMap FlipperSpL1_LM_Inserts_l52, 100.0, " ' VLM.Lampz;Inserts-l52
	Lampz.MassAssign(53) = l53 ' VLM.Lampz;Inserts-l53
	Lampz.Callback(53) = "UpdateLightMap Playfield_LM_Inserts_l53, 100.0, " ' VLM.Lampz;Inserts-l53
	Lampz.Callback(53) = "UpdateLightMap Parts_LM_Inserts_l53, 100.0, " ' VLM.Lampz;Inserts-l53
	Lampz.Callback(53) = "UpdateLightMap FlipperL1_LM_Inserts_l53, 100.0, " ' VLM.Lampz;Inserts-l53
	Lampz.Callback(53) = "UpdateLightMap ClockToy_LM_Inserts_l53, 100.0, " ' VLM.Lampz;Inserts-l53
	Lampz.Callback(53) = "UpdateLightMap FlipperSpL1_LM_Inserts_l53, 100.0, " ' VLM.Lampz;Inserts-l53
	Lampz.MassAssign(54) = l54 ' VLM.Lampz;Inserts-l54
	Lampz.Callback(54) = "UpdateLightMap Playfield_LM_Inserts_l54, 100.0, " ' VLM.Lampz;Inserts-l54
	Lampz.Callback(54) = "UpdateLightMap Parts_LM_Inserts_l54, 100.0, " ' VLM.Lampz;Inserts-l54
	Lampz.Callback(54) = "UpdateLightMap FlipperL1_LM_Inserts_l54, 100.0, " ' VLM.Lampz;Inserts-l54
	Lampz.Callback(54) = "UpdateLightMap RDiv_LM_Inserts_l54, 100.0, " ' VLM.Lampz;Inserts-l54
	Lampz.Callback(54) = "UpdateLightMap ClockToy_LM_Inserts_l54, 100.0, " ' VLM.Lampz;Inserts-l54
	Lampz.Callback(54) = "UpdateLightMap Sign_Spiral_LM_Inserts_l54, 100.0, " ' VLM.Lampz;Inserts-l54
	Lampz.Callback(54) = "UpdateLightMap FlipperSpL1_LM_Inserts_l54, 100.0, " ' VLM.Lampz;Inserts-l54
	Lampz.MassAssign(55) = l55 ' VLM.Lampz;Inserts-l55
	Lampz.Callback(55) = "UpdateLightMap Playfield_LM_Inserts_l55, 100.0, " ' VLM.Lampz;Inserts-l55
	Lampz.Callback(55) = "UpdateLightMap Parts_LM_Inserts_l55, 100.0, " ' VLM.Lampz;Inserts-l55
	Lampz.Callback(55) = "UpdateLightMap Over1_LM_Inserts_l55, 100.0, " ' VLM.Lampz;Inserts-l55
	Lampz.Callback(55) = "UpdateLightMap FlipperL1_LM_Inserts_l55, 100.0, " ' VLM.Lampz;Inserts-l55
	Lampz.Callback(55) = "UpdateLightMap Camera_LM_Inserts_l55, 100.0, " ' VLM.Lampz;Inserts-l55
	Lampz.MassAssign(56) = l56 ' VLM.Lampz;Inserts-l56
	Lampz.Callback(56) = "UpdateLightMap Playfield_LM_Inserts_l56, 100.0, " ' VLM.Lampz;Inserts-l56
	Lampz.Callback(56) = "UpdateLightMap Parts_LM_Inserts_l56, 100.0, " ' VLM.Lampz;Inserts-l56
	Lampz.Callback(56) = "UpdateLightMap Over2_LM_Inserts_l56, 100.0, " ' VLM.Lampz;Inserts-l56
	Lampz.Callback(56) = "UpdateLightMap FlipperL1_LM_Inserts_l56, 100.0, " ' VLM.Lampz;Inserts-l56
	Lampz.Callback(56) = "UpdateLightMap ClockToy_LM_Inserts_l56, 100.0, " ' VLM.Lampz;Inserts-l56
	Lampz.Callback(56) = "UpdateLightMap FlipperSpL1_LM_Inserts_l56, 100.0, " ' VLM.Lampz;Inserts-l56
	Lampz.MassAssign(57) = l57 ' VLM.Lampz;Inserts-l57
	Lampz.Callback(57) = "UpdateLightMap Playfield_LM_Inserts_l57, 100.0, " ' VLM.Lampz;Inserts-l57
	Lampz.Callback(57) = "UpdateLightMap Parts_LM_Inserts_l57, 100.0, " ' VLM.Lampz;Inserts-l57
	Lampz.Callback(57) = "UpdateLightMap Over1_LM_Inserts_l57, 100.0, " ' VLM.Lampz;Inserts-l57
	Lampz.Callback(57) = "UpdateLightMap Over2_LM_Inserts_l57, 100.0, " ' VLM.Lampz;Inserts-l57
	Lampz.Callback(57) = "UpdateLightMap Over3_LM_Inserts_l57, 100.0, " ' VLM.Lampz;Inserts-l57
	Lampz.Callback(57) = "UpdateLightMap Clock_White_LM_Inserts_l57, 100.0, " ' VLM.Lampz;Inserts-l57
	Lampz.Callback(57) = "UpdateLightMap ClockToy_LM_Inserts_l57, 100.0, " ' VLM.Lampz;Inserts-l57
	Lampz.Callback(57) = "UpdateLightMap sw64_LM_Inserts_l57, 100.0, " ' VLM.Lampz;Inserts-l57
	Lampz.MassAssign(58) = l58 ' VLM.Lampz;Inserts-l58
	Lampz.Callback(58) = "UpdateLightMap Playfield_LM_Inserts_l58, 100.0, " ' VLM.Lampz;Inserts-l58
	Lampz.Callback(58) = "UpdateLightMap Parts_LM_Inserts_l58, 100.0, " ' VLM.Lampz;Inserts-l58
	Lampz.Callback(58) = "UpdateLightMap Over1_LM_Inserts_l58, 100.0, " ' VLM.Lampz;Inserts-l58
	Lampz.Callback(58) = "UpdateLightMap Over2_LM_Inserts_l58, 100.0, " ' VLM.Lampz;Inserts-l58
	Lampz.Callback(58) = "UpdateLightMap Over3_LM_Inserts_l58, 100.0, " ' VLM.Lampz;Inserts-l58
	Lampz.Callback(58) = "UpdateLightMap Clock_White_LM_Inserts_l58, 100.0, " ' VLM.Lampz;Inserts-l58
	Lampz.Callback(58) = "UpdateLightMap ClockToy_LM_Inserts_l58, 100.0, " ' VLM.Lampz;Inserts-l58
	Lampz.MassAssign(61) = l61 ' VLM.Lampz;Bumpers-l61
	Lampz.Callback(61) = "UpdateLightMap Playfield_LM_Bumpers_l61, 100.0, " ' VLM.Lampz;Bumpers-l61
	Lampz.Callback(61) = "UpdateLightMap Parts_LM_Bumpers_l61, 100.0, " ' VLM.Lampz;Bumpers-l61
	Lampz.Callback(61) = "UpdateLightMap Over1_LM_Bumpers_l61, 100.0, " ' VLM.Lampz;Bumpers-l61
	Lampz.Callback(61) = "UpdateLightMap BR1_LM_Bumpers_l61, 100.0, " ' VLM.Lampz;Bumpers-l61
	Lampz.Callback(61) = "UpdateLightMap BumperPegs_LM_Bumpers_l61, 100.0, " ' VLM.Lampz;Bumpers-l61
	Lampz.Callback(61) = "UpdateLightMap TownSquarePost_LM_Bumpers_l61, 100.0, " ' VLM.Lampz;Bumpers-l61
	Lampz.Callback(61) = "UpdateLightMap SideMod_LM_Bumpers_l61, 100.0, " ' VLM.Lampz;Bumpers-l61
	Lampz.MassAssign(62) = l62 ' VLM.Lampz;Bumpers-l62
	Lampz.Callback(62) = "UpdateLightMap Playfield_LM_Bumpers_l62, 100.0, " ' VLM.Lampz;Bumpers-l62
	Lampz.Callback(62) = "UpdateLightMap Parts_LM_Bumpers_l62, 100.0, " ' VLM.Lampz;Bumpers-l62
	Lampz.Callback(62) = "UpdateLightMap Over1_LM_Bumpers_l62, 100.0, " ' VLM.Lampz;Bumpers-l62
	Lampz.Callback(62) = "UpdateLightMap BR3_LM_Bumpers_l62, 100.0, " ' VLM.Lampz;Bumpers-l62
	Lampz.Callback(62) = "UpdateLightMap BumperPegs_LM_Bumpers_l62, 100.0, " ' VLM.Lampz;Bumpers-l62
	Lampz.Callback(62) = "UpdateLightMap MysticSeerToy_LM_Bumpers_l62, 100.0, " ' VLM.Lampz;Bumpers-l62
	Lampz.MassAssign(63) = l63 ' VLM.Lampz;Bumpers-l63
	Lampz.Callback(63) = "UpdateLightMap Playfield_LM_Bumpers_l63, 100.0, " ' VLM.Lampz;Bumpers-l63
	Lampz.Callback(63) = "UpdateLightMap Parts_LM_Bumpers_l63, 100.0, " ' VLM.Lampz;Bumpers-l63
	Lampz.Callback(63) = "UpdateLightMap Over1_LM_Bumpers_l63, 100.0, " ' VLM.Lampz;Bumpers-l63
	Lampz.Callback(63) = "UpdateLightMap BR2_LM_Bumpers_l63, 100.0, " ' VLM.Lampz;Bumpers-l63
	Lampz.Callback(63) = "UpdateLightMap TownSquarePost_LM_Bumpers_l63, 100.0, " ' VLM.Lampz;Bumpers-l63
	Lampz.Callback(63) = "UpdateLightMap sw78_LM_Bumpers_l63, 100.0, " ' VLM.Lampz;Bumpers-l63
	Lampz.MassAssign(64) = l64 ' VLM.Lampz;Inserts-l64
	Lampz.Callback(64) = "UpdateLightMap Playfield_LM_Inserts_l64, 100.0, " ' VLM.Lampz;Inserts-l64
	Lampz.Callback(64) = "UpdateLightMap Parts_LM_Inserts_l64, 100.0, " ' VLM.Lampz;Inserts-l64
	Lampz.Callback(64) = "UpdateLightMap Over1_LM_Inserts_l64, 100.0, " ' VLM.Lampz;Inserts-l64
	Lampz.Callback(64) = "UpdateLightMap BR2_LM_Inserts_l64, 100.0, " ' VLM.Lampz;Inserts-l64
	Lampz.Callback(64) = "UpdateLightMap BR3_LM_Inserts_l64, 100.0, " ' VLM.Lampz;Inserts-l64
	Lampz.Callback(64) = "UpdateLightMap BumperPegs_LM_Inserts_l64, 100.0, " ' VLM.Lampz;Inserts-l64
	Lampz.Callback(64) = "UpdateLightMap SlotMachineToy_LM_Inserts_l64, 100.0, " ' VLM.Lampz;Inserts-l64
	Lampz.Callback(64) = "UpdateLightMap TownSquarePost_LM_Inserts_l64, 100.0, " ' VLM.Lampz;Inserts-l64
	Lampz.Callback(64) = "UpdateLightMap sw48_LM_Inserts_l64, 100.0, " ' VLM.Lampz;Inserts-l64
	Lampz.Callback(64) = "UpdateLightMap sw77_LM_Inserts_l64, 100.0, " ' VLM.Lampz;Inserts-l64
	Lampz.Callback(64) = "UpdateLightMap sw77m_LM_Inserts_l64, 100.0, " ' VLM.Lampz;Inserts-l64
	Lampz.MassAssign(65) = l65 ' VLM.Lampz;Inserts-l65
	Lampz.Callback(65) = "UpdateLightMap Playfield_LM_Inserts_l65, 100.0, " ' VLM.Lampz;Inserts-l65
	Lampz.Callback(65) = "UpdateLightMap Parts_LM_Inserts_l65, 100.0, " ' VLM.Lampz;Inserts-l65
	Lampz.Callback(65) = "UpdateLightMap Over1_LM_Inserts_l65, 100.0, " ' VLM.Lampz;Inserts-l65
	Lampz.Callback(65) = "UpdateLightMap BR2_LM_Inserts_l65, 100.0, " ' VLM.Lampz;Inserts-l65
	Lampz.Callback(65) = "UpdateLightMap FlipperL1_LM_Inserts_l65, 100.0, " ' VLM.Lampz;Inserts-l65
	Lampz.Callback(65) = "UpdateLightMap Camera_LM_Inserts_l65, 100.0, " ' VLM.Lampz;Inserts-l65
	Lampz.Callback(65) = "UpdateLightMap ClockToy_LM_Inserts_l65, 100.0, " ' VLM.Lampz;Inserts-l65
	Lampz.Callback(65) = "UpdateLightMap SlotMachineToy_LM_Inserts_l65, 100.0, " ' VLM.Lampz;Inserts-l65
	Lampz.Callback(65) = "UpdateLightMap sw77_LM_Inserts_l65, 100.0, " ' VLM.Lampz;Inserts-l65
	Lampz.Callback(65) = "UpdateLightMap sw78_LM_Inserts_l65, 100.0, " ' VLM.Lampz;Inserts-l65
	Lampz.Callback(65) = "UpdateLightMap sw78m_LM_Inserts_l65, 100.0, " ' VLM.Lampz;Inserts-l65
	Lampz.MassAssign(66) = l66 ' VLM.Lampz;Inserts-l66
	Lampz.Callback(66) = "UpdateLightMap Playfield_LM_Inserts_l66, 100.0, " ' VLM.Lampz;Inserts-l66
	Lampz.Callback(66) = "UpdateLightMap Parts_LM_Inserts_l66, 100.0, " ' VLM.Lampz;Inserts-l66
	Lampz.Callback(66) = "UpdateLightMap Over1_LM_Inserts_l66, 100.0, " ' VLM.Lampz;Inserts-l66
	Lampz.Callback(66) = "UpdateLightMap FlipperR_LM_Inserts_l66, 100.0, " ' VLM.Lampz;Inserts-l66
	Lampz.Callback(66) = "UpdateLightMap sw11_LM_Inserts_l66, 100.0, " ' VLM.Lampz;Inserts-l66
	Lampz.Callback(66) = "UpdateLightMap sw12_LM_Inserts_l66, 100.0, " ' VLM.Lampz;Inserts-l66
	Lampz.Callback(66) = "UpdateLightMap RocketToy_LM_Inserts_l66, 100.0, " ' VLM.Lampz;Inserts-l66
	Lampz.Callback(66) = "UpdateLightMap FlipperSpR_LM_Inserts_l66, 100.0, " ' VLM.Lampz;Inserts-l66
	Lampz.MassAssign(67) = l67 ' VLM.Lampz;Inserts-l67
	Lampz.Callback(67) = "UpdateLightMap Playfield_LM_Inserts_l67, 100.0, " ' VLM.Lampz;Inserts-l67
	Lampz.Callback(67) = "UpdateLightMap Parts_LM_Inserts_l67, 100.0, " ' VLM.Lampz;Inserts-l67
	Lampz.Callback(67) = "UpdateLightMap Over1_LM_Inserts_l67, 100.0, " ' VLM.Lampz;Inserts-l67
	Lampz.Callback(67) = "UpdateLightMap Over3_LM_Inserts_l67, 100.0, " ' VLM.Lampz;Inserts-l67
	Lampz.Callback(67) = "UpdateLightMap SlotMachineToy_LM_Inserts_l67, 100.0, " ' VLM.Lampz;Inserts-l67
	Lampz.MassAssign(68) = l68 ' VLM.Lampz;Inserts-l68
	Lampz.Callback(68) = "UpdateLightMap Playfield_LM_Inserts_l68, 100.0, " ' VLM.Lampz;Inserts-l68
	Lampz.Callback(68) = "UpdateLightMap Parts_LM_Inserts_l68, 100.0, " ' VLM.Lampz;Inserts-l68
	Lampz.Callback(68) = "UpdateLightMap Over2_LM_Inserts_l68, 100.0, " ' VLM.Lampz;Inserts-l68
	Lampz.Callback(68) = "UpdateLightMap Over3_LM_Inserts_l68, 100.0, " ' VLM.Lampz;Inserts-l68
	Lampz.Callback(68) = "UpdateLightMap DiverterP_LM_Inserts_l68, 100.0, " ' VLM.Lampz;Inserts-l68
	Lampz.MassAssign(71) = l71 ' VLM.Lampz;Inserts-l71
	Lampz.Callback(71) = "UpdateLightMap Playfield_LM_Inserts_l71, 100.0, " ' VLM.Lampz;Inserts-l71
	Lampz.Callback(71) = "UpdateLightMap Parts_LM_Inserts_l71, 100.0, " ' VLM.Lampz;Inserts-l71
	Lampz.Callback(71) = "UpdateLightMap Over1_LM_Inserts_l71, 100.0, " ' VLM.Lampz;Inserts-l71
	Lampz.Callback(71) = "UpdateLightMap Over3_LM_Inserts_l71, 100.0, " ' VLM.Lampz;Inserts-l71
	Lampz.Callback(71) = "UpdateLightMap BR2_LM_Inserts_l71, 100.0, " ' VLM.Lampz;Inserts-l71
	Lampz.Callback(71) = "UpdateLightMap FlipperR1_LM_Inserts_l71, 100.0, " ' VLM.Lampz;Inserts-l71
	Lampz.Callback(71) = "UpdateLightMap sw62_LM_Inserts_l71, 100.0, " ' VLM.Lampz;Inserts-l71
	Lampz.Callback(71) = "UpdateLightMap ClockToy_LM_Inserts_l71, 100.0, " ' VLM.Lampz;Inserts-l71
	Lampz.Callback(71) = "UpdateLightMap SlotMachineToy_LM_Inserts_l71, 100.0, " ' VLM.Lampz;Inserts-l71
	Lampz.Callback(71) = "UpdateLightMap sw68_LM_Inserts_l71, 100.0, " ' VLM.Lampz;Inserts-l71
	Lampz.Callback(71) = "UpdateLightMap sw68m_LM_Inserts_l71, 100.0, " ' VLM.Lampz;Inserts-l71
	Lampz.Callback(71) = "UpdateLightMap FlipperSpR1_LM_Inserts_l71, 100.0, " ' VLM.Lampz;Inserts-l71
	Lampz.MassAssign(72) = l72 ' VLM.Lampz;Inserts-l72
	Lampz.Callback(72) = "UpdateLightMap Playfield_LM_Inserts_l72, 100.0, " ' VLM.Lampz;Inserts-l72
	Lampz.Callback(72) = "UpdateLightMap Parts_LM_Inserts_l72, 100.0, " ' VLM.Lampz;Inserts-l72
	Lampz.Callback(72) = "UpdateLightMap Over1_LM_Inserts_l72, 100.0, " ' VLM.Lampz;Inserts-l72
	Lampz.Callback(72) = "UpdateLightMap Over2_LM_Inserts_l72, 100.0, " ' VLM.Lampz;Inserts-l72
	Lampz.Callback(72) = "UpdateLightMap Over3_LM_Inserts_l72, 100.0, " ' VLM.Lampz;Inserts-l72
	Lampz.Callback(72) = "UpdateLightMap FlipperL1_LM_Inserts_l72, 100.0, " ' VLM.Lampz;Inserts-l72
	Lampz.Callback(72) = "UpdateLightMap FlipperR1_LM_Inserts_l72, 100.0, " ' VLM.Lampz;Inserts-l72
	Lampz.Callback(72) = "UpdateLightMap Clock_Color_LM_Inserts_l72, 100.0, " ' VLM.Lampz;Inserts-l72
	Lampz.Callback(72) = "UpdateLightMap Clock_White_LM_Inserts_l72, 100.0, " ' VLM.Lampz;Inserts-l72
	Lampz.Callback(72) = "UpdateLightMap ClockToy_LM_Inserts_l72, 100.0, " ' VLM.Lampz;Inserts-l72
	Lampz.Callback(72) = "UpdateLightMap SlotMachineToy_LM_Inserts_l72, 100.0, " ' VLM.Lampz;Inserts-l72
	Lampz.Callback(72) = "UpdateLightMap sw66_LM_Inserts_l72, 100.0, " ' VLM.Lampz;Inserts-l72
	Lampz.Callback(72) = "UpdateLightMap sw67_LM_Inserts_l72, 100.0, " ' VLM.Lampz;Inserts-l72
	Lampz.Callback(72) = "UpdateLightMap sw66m_LM_Inserts_l72, 100.0, " ' VLM.Lampz;Inserts-l72
	Lampz.Callback(72) = "UpdateLightMap FlipperSpL1_LM_Inserts_l72, 100.0, " ' VLM.Lampz;Inserts-l72
	Lampz.Callback(72) = "UpdateLightMap FlipperSpR1_LM_Inserts_l72, 100.0, " ' VLM.Lampz;Inserts-l72
	Lampz.MassAssign(73) = l73 ' VLM.Lampz;Inserts-l73
	Lampz.Callback(73) = "UpdateLightMap Playfield_LM_Inserts_l73, 100.0, " ' VLM.Lampz;Inserts-l73
	Lampz.Callback(73) = "UpdateLightMap Parts_LM_Inserts_l73, 100.0, " ' VLM.Lampz;Inserts-l73
	Lampz.Callback(73) = "UpdateLightMap Over1_LM_Inserts_l73, 100.0, " ' VLM.Lampz;Inserts-l73
	Lampz.Callback(73) = "UpdateLightMap Over2_LM_Inserts_l73, 100.0, " ' VLM.Lampz;Inserts-l73
	Lampz.Callback(73) = "UpdateLightMap Over3_LM_Inserts_l73, 100.0, " ' VLM.Lampz;Inserts-l73
	Lampz.Callback(73) = "UpdateLightMap FlipperL1_LM_Inserts_l73, 100.0, " ' VLM.Lampz;Inserts-l73
	Lampz.Callback(73) = "UpdateLightMap Clock_Color_LM_Inserts_l73, 100.0, " ' VLM.Lampz;Inserts-l73
	Lampz.Callback(73) = "UpdateLightMap Clock_White_LM_Inserts_l73, 100.0, " ' VLM.Lampz;Inserts-l73
	Lampz.Callback(73) = "UpdateLightMap ClockToy_LM_Inserts_l73, 100.0, " ' VLM.Lampz;Inserts-l73
	Lampz.Callback(73) = "UpdateLightMap Piano_LM_Inserts_l73, 100.0, " ' VLM.Lampz;Inserts-l73
	Lampz.Callback(73) = "UpdateLightMap SlotMachineToy_LM_Inserts_l73, 100.0, " ' VLM.Lampz;Inserts-l73
	Lampz.Callback(73) = "UpdateLightMap sw66_LM_Inserts_l73, 100.0, " ' VLM.Lampz;Inserts-l73
	Lampz.Callback(73) = "UpdateLightMap sw67_LM_Inserts_l73, 100.0, " ' VLM.Lampz;Inserts-l73
	Lampz.Callback(73) = "UpdateLightMap sw68_LM_Inserts_l73, 100.0, " ' VLM.Lampz;Inserts-l73
	Lampz.Callback(73) = "UpdateLightMap FlipperSpL1_LM_Inserts_l73, 100.0, " ' VLM.Lampz;Inserts-l73
	Lampz.MassAssign(74) = l74 ' VLM.Lampz;Inserts-l74
	Lampz.Callback(74) = "UpdateLightMap Playfield_LM_Inserts_l74, 100.0, " ' VLM.Lampz;Inserts-l74
	Lampz.Callback(74) = "UpdateLightMap Parts_LM_Inserts_l74, 100.0, " ' VLM.Lampz;Inserts-l74
	Lampz.Callback(74) = "UpdateLightMap Over2_LM_Inserts_l74, 100.0, " ' VLM.Lampz;Inserts-l74
	Lampz.Callback(74) = "UpdateLightMap Over3_LM_Inserts_l74, 100.0, " ' VLM.Lampz;Inserts-l74
	Lampz.Callback(74) = "UpdateLightMap Clock_Color_LM_Inserts_l74, 100.0, " ' VLM.Lampz;Inserts-l74
	Lampz.Callback(74) = "UpdateLightMap Clock_White_LM_Inserts_l74, 100.0, " ' VLM.Lampz;Inserts-l74
	Lampz.Callback(74) = "UpdateLightMap ClockToy_LM_Inserts_l74, 100.0, " ' VLM.Lampz;Inserts-l74
	Lampz.MassAssign(75) = l75 ' VLM.Lampz;Inserts-l75
	Lampz.Callback(75) = "UpdateLightMap Playfield_LM_Inserts_l75, 100.0, " ' VLM.Lampz;Inserts-l75
	Lampz.Callback(75) = "UpdateLightMap Parts_LM_Inserts_l75, 100.0, " ' VLM.Lampz;Inserts-l75
	Lampz.Callback(75) = "UpdateLightMap Over1_LM_Inserts_l75, 100.0, " ' VLM.Lampz;Inserts-l75
	Lampz.Callback(75) = "UpdateLightMap Over2_LM_Inserts_l75, 100.0, " ' VLM.Lampz;Inserts-l75
	Lampz.Callback(75) = "UpdateLightMap Over3_LM_Inserts_l75, 100.0, " ' VLM.Lampz;Inserts-l75
	Lampz.Callback(75) = "UpdateLightMap Clock_Color_LM_Inserts_l75, 100.0, " ' VLM.Lampz;Inserts-l75
	Lampz.Callback(75) = "UpdateLightMap Clock_White_LM_Inserts_l75, 100.0, " ' VLM.Lampz;Inserts-l75
	Lampz.Callback(75) = "UpdateLightMap ClockToy_LM_Inserts_l75, 100.0, " ' VLM.Lampz;Inserts-l75
	Lampz.Callback(75) = "UpdateLightMap Piano_LM_Inserts_l75, 100.0, " ' VLM.Lampz;Inserts-l75
	Lampz.Callback(75) = "UpdateLightMap Robot_LM_Inserts_l75, 100.0, " ' VLM.Lampz;Inserts-l75
	Lampz.Callback(75) = "UpdateLightMap SlotMachineToy_LM_Inserts_l75, 100.0, " ' VLM.Lampz;Inserts-l75
	Lampz.Callback(75) = "UpdateLightMap sw64_LM_Inserts_l75, 100.0, " ' VLM.Lampz;Inserts-l75
	Lampz.Callback(75) = "UpdateLightMap sw64m_LM_Inserts_l75, 100.0, " ' VLM.Lampz;Inserts-l75
	Lampz.MassAssign(76) = l76 ' VLM.Lampz;Inserts-l76
	Lampz.Callback(76) = "UpdateLightMap Parts_LM_Inserts_l76, 100.0, " ' VLM.Lampz;Inserts-l76
	Lampz.Callback(76) = "UpdateLightMap Over1_LM_Inserts_l76, 100.0, " ' VLM.Lampz;Inserts-l76
	Lampz.Callback(76) = "UpdateLightMap Pyramid_LM_Inserts_l76, 100.0, " ' VLM.Lampz;Inserts-l76
	Lampz.MassAssign(77) = l77 ' VLM.Lampz;Inserts-l77
	Lampz.Callback(77) = "UpdateLightMap Parts_LM_Inserts_l77, 100.0, " ' VLM.Lampz;Inserts-l77
	Lampz.Callback(77) = "UpdateLightMap Over1_LM_Inserts_l77, 100.0, " ' VLM.Lampz;Inserts-l77
	Lampz.MassAssign(78) = l78 ' VLM.Lampz;Inserts-l78
	Lampz.Callback(78) = "UpdateLightMap Parts_LM_Inserts_l78, 100.0, " ' VLM.Lampz;Inserts-l78
	Lampz.Callback(78) = "UpdateLightMap Over1_LM_Inserts_l78, 100.0, " ' VLM.Lampz;Inserts-l78
	Lampz.MassAssign(81) = l81 ' VLM.Lampz;Flashers-l81
	Lampz.Callback(81) = "UpdateLightMap Playfield_LM_Flashers_l81, 100.0, " ' VLM.Lampz;Flashers-l81
	Lampz.Callback(81) = "UpdateLightMap Parts_LM_Flashers_l81, 100.0, " ' VLM.Lampz;Flashers-l81
	Lampz.Callback(81) = "UpdateLightMap Over1_LM_Flashers_l81, 100.0, " ' VLM.Lampz;Flashers-l81
	Lampz.Callback(81) = "UpdateLightMap FlipperL1_LM_Flashers_l81, 100.0, " ' VLM.Lampz;Flashers-l81
	Lampz.Callback(81) = "UpdateLightMap ClockToy_LM_Flashers_l81, 100.0, " ' VLM.Lampz;Flashers-l81
	Lampz.Callback(81) = "UpdateLightMap Sign_Spiral_LM_Flashers_l81, 100.0, " ' VLM.Lampz;Flashers-l81
	Lampz.Callback(81) = "UpdateLightMap FlipperSpL1_LM_Flashers_l81, 100.0, " ' VLM.Lampz;Flashers-l81
	Lampz.MassAssign(82) = l82 ' VLM.Lampz;Inserts-l82
	Lampz.Callback(82) = "UpdateLightMap Playfield_LM_Inserts_l82, 100.0, " ' VLM.Lampz;Inserts-l82
	Lampz.Callback(82) = "UpdateLightMap Parts_LM_Inserts_l82, 100.0, " ' VLM.Lampz;Inserts-l82
	Lampz.Callback(82) = "UpdateLightMap Over1_LM_Inserts_l82, 100.0, " ' VLM.Lampz;Inserts-l82
	Lampz.Callback(82) = "UpdateLightMap Over2_LM_Inserts_l82, 100.0, " ' VLM.Lampz;Inserts-l82
	Lampz.Callback(82) = "UpdateLightMap FlipperL1_LM_Inserts_l82, 100.0, " ' VLM.Lampz;Inserts-l82
	Lampz.Callback(82) = "UpdateLightMap ClockToy_LM_Inserts_l82, 100.0, " ' VLM.Lampz;Inserts-l82
	Lampz.Callback(82) = "UpdateLightMap sw47_LM_Inserts_l82, 100.0, " ' VLM.Lampz;Inserts-l82
	Lampz.Callback(82) = "UpdateLightMap sw47m_LM_Inserts_l82, 100.0, " ' VLM.Lampz;Inserts-l82
	Lampz.Callback(82) = "UpdateLightMap FlipperSpL1_LM_Inserts_l82, 100.0, " ' VLM.Lampz;Inserts-l82
	Lampz.MassAssign(83) = l83 ' VLM.Lampz;Flashers-l83
	Lampz.Callback(83) = "UpdateLightMap Playfield_LM_Flashers_l83, 100.0, " ' VLM.Lampz;Flashers-l83
	Lampz.Callback(83) = "UpdateLightMap Parts_LM_Flashers_l83, 100.0, " ' VLM.Lampz;Flashers-l83
	Lampz.Callback(83) = "UpdateLightMap Over1_LM_Flashers_l83, 100.0, " ' VLM.Lampz;Flashers-l83
	Lampz.Callback(83) = "UpdateLightMap Over2_LM_Flashers_l83, 100.0, " ' VLM.Lampz;Flashers-l83
	Lampz.Callback(83) = "UpdateLightMap Over3_LM_Flashers_l83, 100.0, " ' VLM.Lampz;Flashers-l83
	Lampz.Callback(83) = "UpdateLightMap Clock_Color_LM_Flashers_l83, 100.0, " ' VLM.Lampz;Flashers-l83
	Lampz.Callback(83) = "UpdateLightMap Clock_White_LM_Flashers_l83, 100.0, " ' VLM.Lampz;Flashers-l83
	Lampz.Callback(83) = "UpdateLightMap ClockToy_LM_Flashers_l83, 100.0, " ' VLM.Lampz;Flashers-l83
	Lampz.Callback(83) = "UpdateLightMap Piano_LM_Flashers_l83, 100.0, " ' VLM.Lampz;Flashers-l83
	Lampz.Callback(83) = "UpdateLightMap sw65a_LM_Flashers_l83, 100.0, " ' VLM.Lampz;Flashers-l83
	Lampz.Callback(83) = "UpdateLightMap sw64m_LM_Flashers_l83, 100.0, " ' VLM.Lampz;Flashers-l83
	Lampz.MassAssign(84) = l84 ' VLM.Lampz;Flashers-l84
	Lampz.Callback(84) = "UpdateLightMap Playfield_LM_Flashers_l84, 100.0, " ' VLM.Lampz;Flashers-l84
	Lampz.Callback(84) = "UpdateLightMap Parts_LM_Flashers_l84, 100.0, " ' VLM.Lampz;Flashers-l84
	Lampz.Callback(84) = "UpdateLightMap Over1_LM_Flashers_l84, 100.0, " ' VLM.Lampz;Flashers-l84
	Lampz.Callback(84) = "UpdateLightMap Over2_LM_Flashers_l84, 100.0, " ' VLM.Lampz;Flashers-l84
	Lampz.Callback(84) = "UpdateLightMap Over3_LM_Flashers_l84, 100.0, " ' VLM.Lampz;Flashers-l84
	Lampz.Callback(84) = "UpdateLightMap Clock_Color_LM_Flashers_l84, 100.0, " ' VLM.Lampz;Flashers-l84
	Lampz.Callback(84) = "UpdateLightMap Clock_White_LM_Flashers_l84, 100.0, " ' VLM.Lampz;Flashers-l84
	Lampz.Callback(84) = "UpdateLightMap Piano_LM_Flashers_l84, 100.0, " ' VLM.Lampz;Flashers-l84
	Lampz.Callback(84) = "UpdateLightMap sw64m_LM_Flashers_l84, 100.0, " ' VLM.Lampz;Flashers-l84
	Lampz.MassAssign(85) = l85 ' VLM.Lampz;Flashers-l85
	Lampz.Callback(85) = "UpdateLightMap Playfield_LM_Flashers_l85, 100.0, " ' VLM.Lampz;Flashers-l85
	Lampz.Callback(85) = "UpdateLightMap Parts_LM_Flashers_l85, 100.0, " ' VLM.Lampz;Flashers-l85
	Lampz.Callback(85) = "UpdateLightMap FlipperR1_LM_Flashers_l85, 100.0, " ' VLM.Lampz;Flashers-l85
	Lampz.Callback(85) = "UpdateLightMap SlotMachineToy_LM_Flashers_l85, 100.0, " ' VLM.Lampz;Flashers-l85
	Lampz.Callback(85) = "UpdateLightMap TownSquarePost_LM_Flashers_l85, 100.0, " ' VLM.Lampz;Flashers-l85
	Lampz.Callback(85) = "UpdateLightMap FlipperSpR1_LM_Flashers_l85, 100.0, " ' VLM.Lampz;Flashers-l85
	Lampz.MassAssign(86) = l86 ' VLM.Lampz;Flashers-l86
	Lampz.Callback(86) = "UpdateLightMap Playfield_LM_Flashers_l86, 100.0, " ' VLM.Lampz;Flashers-l86
	Lampz.Callback(86) = "UpdateLightMap Parts_LM_Flashers_l86, 100.0, " ' VLM.Lampz;Flashers-l86
	Lampz.Callback(86) = "UpdateLightMap Over1_LM_Flashers_l86, 100.0, " ' VLM.Lampz;Flashers-l86
	Lampz.Callback(86) = "UpdateLightMap FlipperR1_LM_Flashers_l86, 100.0, " ' VLM.Lampz;Flashers-l86
	Lampz.Callback(86) = "UpdateLightMap InvaderToy_LM_Flashers_l86, 100.0, " ' VLM.Lampz;Flashers-l86
	Lampz.Callback(86) = "UpdateLightMap SlotMachineToy_LM_Flashers_l86, 100.0, " ' VLM.Lampz;Flashers-l86
	Lampz.Callback(86) = "UpdateLightMap FlipperSpR1_LM_Flashers_l86, 100.0, " ' VLM.Lampz;Flashers-l86

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

'******************************************************
'****  END LAMPZ
'******************************************************


'******************************************************
'***   VR Backglass Code
'******************************************************

dim Fl1lvl,Fl1On, Fl2lvl,Fl2On, Fl3lvl, Fl3On, Fl4lvl, Fl4On, Fl5lvl, Fl5On, Fl6lvl, Fl6On

'backglass positioning
Sub SetBackglass()
	Dim obj

	For Each obj In VRBackglass
		obj.x = obj.x
		obj.height = - obj.y + 280
		obj.y = -50 'adjusts the distance from the backglass towards the user
		obj.rotx=-87
	Next

End Sub

'Blinking lights
'Fade code
Sub BGTimer_timer
	dim blinklight
	If VRBGGI009.opacity = 100 Then 'If Main GI is off, sets blinking lights to off
		For each blinklight in VRBGBlink : Blinklight.visible = True : Next
	Else
		For each blinklight in VRBGBlink : Blinklight.visible = False : Next
	End If

	VRBGFL015.opacity = 50 * Fl1lvl^1.5
	VRBGFL015_2.opacity = 75 * Fl1lvl^2
	VRBGFL001.opacity = 50 * Fl2lvl^1.5
	VRBGFL001_2.opacity = 75 * Fl2lvl^2
	VRBGFL005.opacity = 50 * Fl3lvl^1.5
	VRBGFL005_2.opacity = 75 * Fl3lvl^2
	VRBGFL004.opacity = 50 * Fl4lvl^1.5
	VRBGFL004_2.opacity = 75 * Fl4lvl^2
	VRBGFL006.opacity = 50 * Fl5lvl^1.5
	VRBGFL006_2.opacity = 75 * Fl5lvl^2
	VRBGFL007.opacity = 50 * Fl6lvl^1.5
	VRBGFL007_2.opacity = 75 * Fl6lvl^2
	
	If Fl1on = True Then
		Fl1lvl = Fl1lvl * 0.88 - 0.01
		if Fl1lvl < 0 Then
			Fl1lvl = 0
			Fl1on = False
		End If
	End If
	If Fl2on = True Then
		Fl2lvl = Fl2lvl * 0.88 - 0.01
		if Fl2lvl < 0 then 
			Fl2lvl = 0
			Fl2On = False
		End If
	End If
	If Fl3On = True Then
		Fl3lvl = Fl3lvl * 0.88 - 0.01
		if Fl3lvl < 0 then 
			Fl3lvl = 0
			Fl3On = False
		End If
	End If
	If Fl4On = True Then
		Fl4lvl = Fl4lvl * 0.88 - 0.01
		if Fl4lvl < 0 then 
			Fl4lvl = 0
			Fl4On = False
		End If
	End If
	If Fl5On = True Then
		Fl5lvl = Fl5lvl * 0.88 - 0.01
		if Fl5lvl < 0 then 
			Fl5lvl = 0
			Fl5On = False
		End If
	End If
	If Fl6On = True Then
		Fl6lvl = Fl6lvl * 0.88 - 0.01
		if Fl6lvl < 0 then 
			Fl6lvl = 0
			Fl6On = False
		End If
	End If
end Sub

Dim Flasherseq1
Dim Flasherseq2

'faster blink
Sub BGTimer2_Timer
	Select Case Flasherseq1 'Int(Rnd*6)+1
		Case 1:  FL1lvl = 1 : Case 2:  FL1On = True
		Case 15: Fl4lvl = 1 : Case 16: Fl4On = True
		Case 30: Fl3lvl = 1 : Case 31: Fl3On = True
		Case 41: Fl1lvl = 1 : Case 42: Fl1On = True
		Case 55: Fl4lvl = 1 : Case 56: Fl4On = True
		Case 60: Fl3lvl = 1 : Case 61: Fl3On = True
		Case 81: Fl1lvl = 1 : Case 82: Fl1On = True
		Case 90: Fl3lvl = 1 : Case 91: Fl3On = True
		Case 95: Fl4lvl = 1 : Case 96: Fl4On = True
		
	End Select	
	Flasherseq1 = Flasherseq1 + 1
	If Flasherseq1 > 120 Then
	Flasherseq1 = 1
	End if
End Sub

'Slower Blink
Sub BGTimer3_Timer
	Select Case Flasherseq2 'Int(Rnd*6)+1
		Case 1: Fl2lvl = 1 : Case 2: Fl2On = True
		Case 10: Fl5lvl = 1 : Case 11: Fl5On = True
		Case 30: Fl6lvl = 1 : Case 31: Fl6On = True
		Case 41:Fl2lvl = 1 : Case 42: Fl2On = True
		Case 45: Fl5lvl = 1 : Case 46: Fl5On = True
		Case 60: Fl6lvl = 1 : Case 61: Fl6On = True
		Case 75: Fl5lvl = 1 : Case 76: Fl5On = True
		Case 81:Fl2lvl = 1 : Case 82: Fl2On = True
		Case 90: Fl6lvl = 1 : Case 91: Fl6On = True
		Case 105: Fl5lvl = 1 : Case 106: Fl5On = True
		Case 120: Fl6lvl = 1 : Case 121: Fl6On = True
	End Select	
	Flasherseq2 = Flasherseq2 + 1
	If Flasherseq2 > 123 Then
	Flasherseq2 = 1
	End if
End Sub
	



'***********************************************************************
'* TABLE OPTIONS *******************************************************
'***********************************************************************

Dim CabinetSide, FlipperType, ScoopLight, GumballMod, SlotMachineMod, PyramidMod, BWClockMod, MiniClockMod, SpiralSignMod
Dim InvaderMod, MysticSeerMod, TargetMod, TVMod, TownSquarePostMod, SpiralMod, ExtraMagnet, BumperPostsMod
Dim StagedFlipperMod, RobotMod, CameraMod, RocketMod, PianoMod
Dim LightLevel : LightLevel = 50
Dim VolumeDial : VolumeDial = 0.8			'Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 0.5 	'Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.5 	'Level of ramp rolling volume. Value between 0 and 1
Dim DynamicBallShadowsOn : DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow

' Base options
Const Opt_Light = 0
Const Opt_Volume = 1
Const Opt_Volume_Ramp = 2
Const Opt_Volume_Ball = 3
Const Opt_Staged_Flipper = 4
Const Opt_Cabinet_Side = 5
' Table mods & toys
Const Opt_Bumper_Posts = 6
Const Opt_BWClock = 7
Const Opt_Camera = 8
Const Opt_Extra_Magnet = 9
Const Opt_Flipper_Type = 10
Const Opt_Gumball = 11
Const Opt_Invader = 12
Const Opt_Lamp_Post = 13
Const Opt_Mini_Clock = 14
Const Opt_Mystic_Seer = 15
Const Opt_Piano = 16
Const Opt_Pyramid = 17
Const Opt_Scoop_Light = 18
Const Opt_Slot_Machine = 19
Const Opt_Spiral_Cover = 20
Const Opt_Spiral_Sign = 21
Const Opt_Robot = 22
Const Opt_Rocket = 23
Const Opt_Targets = 24
Const Opt_TV = 25
' Advanced options
Const Opt_DynBallShadow = 26
' Informations
Const Opt_Info_1 = 27
Const Opt_Info_2 = 28

Const NOptions = 29

Const FlexDMD_RenderMode_DMD_GRAY_2 = 0
Const FlexDMD_RenderMode_DMD_GRAY_4 = 1
Const FlexDMD_RenderMode_DMD_RGB = 2
Const FlexDMD_RenderMode_SEG_2x16Alpha = 3
Const FlexDMD_RenderMode_SEG_2x20Alpha = 4
Const FlexDMD_RenderMode_SEG_2x7Alpha_2x7Num = 5
Const FlexDMD_RenderMode_SEG_2x7Alpha_2x7Num_4x1Num = 6
Const FlexDMD_RenderMode_SEG_2x7Num_2x7Num_4x1Num = 7
Const FlexDMD_RenderMode_SEG_2x7Num_2x7Num_10x1Num = 8
Const FlexDMD_RenderMode_SEG_2x7Num_2x7Num_4x1Num_gen7 = 9
Const FlexDMD_RenderMode_SEG_2x7Num10_2x7Num10_4x1Num = 10
Const FlexDMD_RenderMode_SEG_2x6Num_2x6Num_4x1Num = 11
Const FlexDMD_RenderMode_SEG_2x6Num10_2x6Num10_4x1Num = 12
Const FlexDMD_RenderMode_SEG_4x7Num10 = 13
Const FlexDMD_RenderMode_SEG_6x4Num_4x1Num = 14
Const FlexDMD_RenderMode_SEG_2x7Num_4x1Num_1x16Alpha = 15
Const FlexDMD_RenderMode_SEG_1x16Alpha_1x16Num_1x7Num = 16

Const FlexDMD_Align_TopLeft = 0
Const FlexDMD_Align_Top = 1
Const FlexDMD_Align_TopRight = 2
Const FlexDMD_Align_Left = 3
Const FlexDMD_Align_Center = 4
Const FlexDMD_Align_Right = 5
Const FlexDMD_Align_BottomLeft = 6
Const FlexDMD_Align_Bottom = 7
Const FlexDMD_Align_BottomRight = 8

Const FlexDMD_Scaling_Fit = 0
Const FlexDMD_Scaling_Fill = 1
Const FlexDMD_Scaling_FillX = 2
Const FlexDMD_Scaling_FillY = 3
Const FlexDMD_Scaling_Stretch = 4
Const FlexDMD_Scaling_StretchX = 5
Const FlexDMD_Scaling_StretchY = 6
Const FlexDMD_Scaling_None = 7

Const FlexDMD_Interpolation_Linear = 0
Const FlexDMD_Interpolation_ElasticIn = 1
Const FlexDMD_Interpolation_ElasticOut = 2
Const FlexDMD_Interpolation_ElasticInOut = 3
Const FlexDMD_Interpolation_QuadIn = 4
Const FlexDMD_Interpolation_QuadOut = 5
Const FlexDMD_Interpolation_QuadInOut = 6
Const FlexDMD_Interpolation_CubeIn = 7
Const FlexDMD_Interpolation_CubeOut = 8
Const FlexDMD_Interpolation_CubeInOut = 9
Const FlexDMD_Interpolation_QuartIn = 10
Const FlexDMD_Interpolation_QuartOut = 11
Const FlexDMD_Interpolation_QuartInOut = 12
Const FlexDMD_Interpolation_QuintIn = 13
Const FlexDMD_Interpolation_QuintOut = 14
Const FlexDMD_Interpolation_QuintInOut = 15
Const FlexDMD_Interpolation_SineIn = 16
Const FlexDMD_Interpolation_SineOut = 17
Const FlexDMD_Interpolation_SineInOut = 18
Const FlexDMD_Interpolation_BounceIn = 19
Const FlexDMD_Interpolation_BounceOut = 20
Const FlexDMD_Interpolation_BounceInOut = 21
Const FlexDMD_Interpolation_CircIn = 22
Const FlexDMD_Interpolation_CircOut = 23
Const FlexDMD_Interpolation_CircInOut = 24
Const FlexDMD_Interpolation_ExpoIn = 25
Const FlexDMD_Interpolation_ExpoOut = 26
Const FlexDMD_Interpolation_ExpoInOut = 27
Const FlexDMD_Interpolation_BackIn = 28
Const FlexDMD_Interpolation_BackOut = 29
Const FlexDMD_Interpolation_BackInOut = 30

Dim OptionDMD: Set OptionDMD = Nothing
Dim bOptionsMagna, bInOptions : bOptionsMagna = False
Dim OptPos, OptSelected, OptN, OptTop, OptBot, OptSel
Dim OptFontHi, OptFontLo

Sub Options_Open
	bOptionsMagna = False
	Set OptionDMD = CreateObject("FlexDMD.FlexDMD")
	If OptionDMD is Nothing Then Exit Sub
	If ShowDT Then OptionDMDFlasher.RotX = -(Table1.Inclination + Table1.Layback)
	bInOptions = True
	OptPos = 0
	OptSelected = False
	OptionDMD.Show = False
	OptionDMD.RenderMode = FlexDMD_RenderMode_DMD_GRAY_4
	OptionDMD.Width = 128
	OptionDMD.Height = 32
	OptionDMD.Clear = True
	OptionDMD.Run = True
	Dim a, scene, font
	Set scene = OptionDMD.NewGroup("Scene")
	Set OptFontHi = OptionDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", vbWhite, vbWhite, 0)
	Set OptFontLo = OptionDMD.NewFont("FlexDMD.Resources.teeny_tiny_pixls-5.fnt", RGB(100, 100, 100), RGB(100, 100, 100), 0)
	Set OptSel = OptionDMD.NewGroup("Sel")
	Set a = OptionDMD.NewLabel(">", OptFontLo, ">>>")
	a.SetAlignedPosition 1, 16, FlexDMD_Align_Left
	OptSel.AddActor a
	Set a = OptionDMD.NewLabel(">", OptFontLo, "<<<")
	a.SetAlignedPosition 127, 16, FlexDMD_Align_Right
	OptSel.AddActor a
	scene.AddActor OptSel
	OptSel.SetBounds 0, 0, 128, 32
	OptSel.Visible = False
	
	Set a = OptionDMD.NewLabel("Info1", OptFontLo, "MAGNA EXIT/ENTER")
	a.SetAlignedPosition 1, 32, FlexDMD_Align_BottomLeft
	scene.AddActor a
	Set a = OptionDMD.NewLabel("Info2", OptFontLo, "FLIPPER SELECT")
	a.SetAlignedPosition 127, 32, FlexDMD_Align_BottomRight
	scene.AddActor a
	Set OptN = OptionDMD.NewLabel("Pos", OptFontLo, "LINE 1")
	Set OptTop = OptionDMD.NewLabel("Top", OptFontLo, "LINE 1")
	Set OptBot = OptionDMD.NewLabel("Bottom", OptFontLo, "LINE 2")
	scene.AddActor OptN
	scene.AddActor OptTop
	scene.AddActor OptBot
	Options_OnOptChg
	OptionDMD.LockRenderThread
	OptionDMD.Stage.AddActor scene
	OptionDMD.UnlockRenderThread
	OptionDMDFlasher.Visible = True
End Sub

Sub Options_UpdateDMD
	If OptionDMD is Nothing Then Exit Sub
	Dim DMDp: DMDp = OptionDMD.DmdPixels
	If Not IsEmpty(DMDp) Then
		DMDWidth = OptionDMD.Width
		DMDHeight = OptionDMD.Height
		DMDPixels = DMDp
	End If
End Sub

Sub Options_Close
	bInOptions = False
	OptionDMDFlasher.Visible = False
	If OptionDMD is Nothing Then Exit Sub
	OptionDMD.Run = False
	Set OptionDMD = Nothing
End Sub

Function Options_OnOffText(opt)
	If opt Then
		Options_OnOffText = "ON"
	Else
		Options_OnOffText = "OFF"
	End If
End Function

Sub Options_OnOptChg
	If OptionDMD is Nothing Then Exit Sub
	OptionDMD.LockRenderThread
	OptN.Text = (OptPos+1) & "/" & NOptions
	If OptSelected Then
		OptTop.Font = OptFontLo
		OptBot.Font = OptFontHi
		OptSel.Visible = True
	Else
		OptTop.Font = OptFontHi
		OptBot.Font = OptFontLo
		OptSel.Visible = False
	End If
	If OptPos = Opt_Light Then
		OptTop.Text = "LIGHT LEVEL"
		OptBot.Text = "LEVEL " & LightLevel
		SaveValue cGameName, "LIGHT", LightLevel
	ElseIf OptPos = Opt_Volume Then
		OptTop.Text = "MECH VOLUME"
		OptBot.Text = "LEVEL " & CInt(VolumeDial * 100)
		SaveValue cGameName, "VOLUME", VolumeDial
	ElseIf OptPos = Opt_Volume_Ramp Then
		OptTop.Text = "RAMP VOLUME"
		OptBot.Text = "LEVEL " & CInt(RampRollVolume * 100)
		SaveValue cGameName, "RAMPVOLUME", RampRollVolume
	ElseIf OptPos = Opt_Volume_Ball Then
		OptTop.Text = "BALL VOLUME"
		OptBot.Text = "LEVEL " & CInt(BallRollVolume * 100)
		SaveValue cGameName, "BALLVOLUME", BallRollVolume
	ElseIf OptPos = Opt_Staged_Flipper Then
		OptTop.Text = "STAGED FLIPPER"
		OptBot.Text = Options_OnOffText(StagedFlipperMod)
		SaveValue cGameName, "STAGED", StagedFlipperMod
	ElseIf OptPos = Opt_Bumper_Posts Then
		OptTop.Text = "BUMPER POST"
		OptBot.Text = Options_OnOffText(BumperPostsMod)
		SaveValue cGameName, "BUMPER", BumperPostsMod
	ElseIf OptPos = Opt_BWClock Then
		OptTop.Text = "CLOCK BW"
		OptBot.Text = Options_OnOffText(BWClockMod)
		SaveValue cGameName, "BWCLOCK", BWClockMod
	ElseIf OptPos = Opt_Mini_Clock Then
		OptTop.Text = "CLOCK TOY MOD"
		OptBot.Text = Options_OnOffText(MiniClockMod)
		SaveValue cGameName, "CLOCK", MiniClockMod
	ElseIf OptPos = Opt_Extra_Magnet Then
		OptTop.Text = "EXTRA MAGNET"
		OptBot.Text = Options_OnOffText(ExtraMagnet)
		SaveValue cGameName, "MAGNET", ExtraMagnet
	ElseIf OptPos = Opt_Gumball Then
		OptTop.Text = "GUMBALL"
		OptBot.Text = Options_OnOffText(GumballMod)
		SaveValue cGameName, "GUMBALL", GumballMod
	ElseIf OptPos = Opt_Invader Then
		OptTop.Text = "INVADER MOD"
		OptBot.Text = Options_OnOffText(InvaderMod)
		SaveValue cGameName, "INVADER", InvaderMod
	ElseIf OptPos = Opt_Lamp_Post Then
		OptTop.Text = "LAMP POST"
		OptBot.Text = Options_OnOffText(TownSquarePostMod)
		SaveValue cGameName, "LAMP", TownSquarePostMod
	ElseIf OptPos = Opt_Mystic_Seer Then
		OptTop.Text = "MYSTIC SEER"
		OptBot.Text = Options_OnOffText(MysticSeerMod)
		SaveValue cGameName, "MYSTIC", MysticSeerMod
	ElseIf OptPos = Opt_Piano Then
		OptTop.Text = "PIANO MOD"
		OptBot.Text = Options_OnOffText(PianoMod)
		SaveValue cGameName, "PIANO", PianoMod
	ElseIf OptPos = Opt_Pyramid Then
		OptTop.Text = "PYRAMID MOD"
		OptBot.Text = Options_OnOffText(PyramidMod)
		SaveValue cGameName, "PYRAMID", PyramidMod
	ElseIf OptPos = Opt_Scoop_Light Then
		OptTop.Text = "SCOOP LIGHT"
		OptBot.Text = Options_OnOffText(ScoopLight)
		SaveValue cGameName, "SCOOP", ScoopLight
	ElseIf OptPos = Opt_Slot_Machine Then
		OptTop.Text = "SLOT MACHINE"
		OptBot.Text = Options_OnOffText(SlotMachineMod)
		SaveValue cGameName, "SLOT", SlotMachineMod
	ElseIf OptPos = Opt_Spiral_Cover Then
		OptTop.Text = "SPIRAL COVER"
		OptBot.Text = Options_OnOffText(SpiralMod)
		SaveValue cGameName, "SPIRAL", SpiralMod
	ElseIf OptPos = Opt_Spiral_Sign Then
		OptTop.Text = "SPIRAL SIGN"
		OptBot.Text = Options_OnOffText(SpiralSignMod)
		SaveValue cGameName, "SIGN", SpiralSignMod
	ElseIf OptPos = Opt_TV Then
		OptTop.Text = "TV MOD"
		OptBot.Text = Options_OnOffText(TVMod)
		SaveValue cGameName, "TV", TVMod
	ElseIf OptPos = Opt_Targets Then
		OptTop.Text = "TARGET MOD"
		OptBot.Text = Options_OnOffText(TargetMod)
		SaveValue cGameName, "TARGET", TargetMod
	ElseIf OptPos = Opt_Robot Then
		OptTop.Text = "ROBOT"
		OptBot.Text = Options_OnOffText(RobotMod)
		SaveValue cGameName, "ROBOT", RobotMod
	ElseIf OptPos = Opt_Rocket Then
		OptTop.Text = "ROCKET"
		OptBot.Text = Options_OnOffText(RocketMod)
		SaveValue cGameName, "ROCKET", RocketMod
	ElseIf OptPos = Opt_Camera Then
		OptTop.Text = "CAMERA"
		OptBot.Text = Options_OnOffText(CameraMod)
		SaveValue cGameName, "CAMERA", CameraMod
	ElseIf OptPos = Opt_Flipper_Type Then
		OptTop.Text = "FLIPPER BATS"
		OptBot.Text = Options_OnOffText(FlipperType)
		SaveValue cGameName, "FLIPPER", FlipperType
	ElseIf OptPos = Opt_Cabinet_Side Then
		OptTop.Text = "CABINET SIDES"
		OptBot.Text = Options_OnOffText(CabinetSide)
		SaveValue cGameName, "SIDES", CabinetSide
	ElseIf OptPos = Opt_DynBallShadow Then
		OptTop.Text = "DYN. BALL SHADOWS"
		OptBot.Text = Options_OnOffText(DynamicBallShadowsOn)
		SaveValue cGameName, "DYNBALLSH", DynamicBallShadowsOn
	ElseIf OptPos = Opt_Info_1 Then
		OptTop.Text = "VPX " & VersionMajor & "." & VersionMinor & "." & VersionRevision
		OptBot.Text = "TWILIGHT ZONE " & TableVersion
	ElseIf OptPos = Opt_Info_2 Then
		OptTop.Text = "RENDER MODE"
		If RenderingMode = 0 Then OptBot.Text = "DEFAULT"
		If RenderingMode = 1 Then OptBot.Text = "STEREO 3D"
		If RenderingMode = 2 Then OptBot.Text = "VR"
	End If
	OptTop.SetAlignedPosition 127, 1, FlexDMD_Align_TopRight
	OptBot.SetAlignedPosition 64, 16, FlexDMD_Align_Center
	OptionDMD.UnlockRenderThread
	UpdateMods
End Sub

Sub Options_Toggle(amount)
	If OptionDMD is Nothing Then Exit Sub
	If OptPos = Opt_Light Then
		LightLevel = LightLevel + amount * 10
		If LightLevel < 0 Then LightLevel = 100
		If LightLevel > 100 Then LightLevel = 0
	ElseIf OptPos = Opt_Volume Then
		VolumeDial = VolumeDial + amount * 0.1
		If VolumeDial < 0 Then VolumeDial = 1
		If VolumeDial > 1 Then VolumeDial = 0
	ElseIf OptPos = Opt_Volume_Ramp Then
		RampRollVolume = RampRollVolume + amount * 0.1
		If RampRollVolume < 0 Then RampRollVolume = 1
		If RampRollVolume > 1 Then RampRollVolume = 0
	ElseIf OptPos = Opt_Volume_Ball Then
		BallRollVolume = BallRollVolume + amount * 0.1
		If BallRollVolume < 0 Then BallRollVolume = 1
		If BallRollVolume > 1 Then BallRollVolume = 0
	ElseIf OptPos = Opt_Staged_Flipper Then
		StagedFlipperMod = 1 - StagedFlipperMod
	ElseIf OptPos = Opt_Bumper_Posts Then
		BumperPostsMod = 1 - BumperPostsMod
	ElseIf OptPos = Opt_BWClock Then
		BWClockMod = 1 - BWClockMod
	ElseIf OptPos = Opt_Mini_Clock Then
		MiniClockMod = 1 - MiniClockMod
	ElseIf OptPos = Opt_Extra_Magnet Then
		ExtraMagnet = 1 - ExtraMagnet
	ElseIf OptPos = Opt_Gumball Then
		GumballMod = 1 - GumballMod
	ElseIf OptPos = Opt_Invader Then
		InvaderMod = 1 - InvaderMod
	ElseIf OptPos = Opt_Lamp_Post Then
		TownSquarePostMod = 1 - TownSquarePostMod
	ElseIf OptPos = Opt_Mystic_Seer Then
		MysticSeerMod = 1 - MysticSeerMod
	ElseIf OptPos = Opt_Piano Then
		PianoMod = 1 - PianoMod
	ElseIf OptPos = Opt_Pyramid Then
		PyramidMod = 1 - PyramidMod
	ElseIf OptPos = Opt_Scoop_Light Then
		ScoopLight = 1 - ScoopLight
	ElseIf OptPos = Opt_Slot_Machine Then
		SlotMachineMod = 1 - SlotMachineMod
	ElseIf OptPos = Opt_Spiral_Cover Then
		SpiralMod = 1 - SpiralMod
	ElseIf OptPos = Opt_Spiral_Sign Then
		SpiralSignMod = 1 - SpiralSignMod
	ElseIf OptPos = Opt_Targets Then
		TargetMod = 1 - TargetMod
	ElseIf OptPos = Opt_TV Then
		TVMod = 1 - TVMod
	ElseIf OptPos = Opt_Robot Then
		RobotMod = 1 - RobotMod
	ElseIf OptPos = Opt_Rocket Then
		RocketMod = 1 - RocketMod
	ElseIf OptPos = Opt_Camera Then
		CameraMod = 1 - CameraMod
	ElseIf OptPos = Opt_Flipper_Type Then
		FlipperType = 1 - FlipperType
	ElseIf OptPos = Opt_Cabinet_Side Then
		CabinetSide = 1 - CabinetSide
	ElseIf OptPos = Opt_DynBallShadow Then
		DynamicBallShadowsOn = 1 - DynamicBallShadowsOn
	End If
End Sub

Sub Options_KeyDown(ByVal keycode)
	If OptSelected Then
		If keycode = LeftMagnaSave Then ' Exit / Cancel
			OptSelected = False
		ElseIf keycode = RightMagnaSave Then ' Enter / Select
			OptSelected = False
		ElseIf keycode = LeftFlipperKey Then ' Next / +
			Options_Toggle	-1
		ElseIf keycode = RightFlipperKey Then ' Prev / -
			Options_Toggle	1
		End If
	Else
		If keycode = LeftMagnaSave Then ' Exit / Cancel
			Options_Close
		ElseIf keycode = RightMagnaSave Then ' Enter / Select
			If OptPos < Opt_Info_1 Then OptSelected = True
		ElseIf keycode = LeftFlipperKey Then ' Next / +
			OptPos = OptPos - 1
			If OptPos < 0 Then OptPos = NOptions - 1
		ElseIf keycode = RightFlipperKey Then ' Prev / -
			OptPos = OptPos + 1
			If OptPos >= NOPtions Then OptPos = 0
		End If
	End If
	Options_OnOptChg
End Sub

Function MyCDbl(str)
    Dim strt, Sep, i
    If IsNumeric(str) Then
        MyCDbl = CDbl(str)
    Else
        Sep = Mid(CStr(0.5), 2, 1)
        Select Case Sep
        Case "."
            i = InStr(1, str, ",")
        Case ","
            i = InStr(1, str, ".")
        End Select
        If i = 0 Then     
            MyCDbl = Empty
        Else
            strt = Mid(str, 1, i - 1) & Sep & Mid(str, i + 1)
            If IsNumeric(strt) Then
                MyCDbl = CDbl(strt)
            Else
                MyCDbl = Empty
            End If
        End If
    End If

End Function

Sub Options_Load
	Dim x
    x = LoadValue(cGameName, "LIGHT")
    If x <> "" Then LightLevel = CInt(x) Else LightLevel = 50
    x = LoadValue(cGameName, "VOLUME")
    If x <> "" Then VolumeDial = MyCDbl(x) Else VolumeDial = 0.8
    x = LoadValue(cGameName, "RAMPVOLUME")
    If x <> "" Then RampRollVolume = MyCDbl(x) Else RampRollVolume = 0.5
    x = LoadValue(cGameName, "BALLVOLUME")
    If x <> "" Then BallRollVolume = MyCDbl(x) Else BallRollVolume = 0.5
    x = LoadValue(cGameName, "STAGED")
    If x <> "" Then StagedFlipperMod = CInt(x) Else StagedFlipperMod = 0
    x = LoadValue(cGameName, "SIDES")
    If x <> "" Then CabinetSide = CInt(x) Else CabinetSide = 0
    x = LoadValue(cGameName, "FLIPPER")
    If x <> "" Then FlipperType = CInt(x) Else FlipperType = 0
    x = LoadValue(cGameName, "ROBOT")
    If x <> "" Then RobotMod = CInt(x) Else RobotMod = 0
    x = LoadValue(cGameName, "CAMERA")
    If x <> "" Then CameraMod = CInt(x) Else CameraMod = 0
    x = LoadValue(cGameName, "ROCKET")
    If x <> "" Then RocketMod = CInt(x) Else RocketMod = 0
    x = LoadValue(cGameName, "BUMPER")
    If x <> "" Then BumperPostsMod = CInt(x) Else BumperPostsMod = 1
    x = LoadValue(cGameName, "BWCLOCK")
    If x <> "" Then BWClockMod = CInt(x) Else BWClockMod = 0
    x = LoadValue(cGameName, "CLOCK")
    If x <> "" Then MiniClockMod = CInt(x) Else MiniClockMod = 0
    x = LoadValue(cGameName, "MAGNET")
    If x <> "" Then ExtraMagnet = CInt(x) Else ExtraMagnet = 0
    x = LoadValue(cGameName, "GUMBALL")
    If x <> "" Then GumballMod = CInt(x) Else GumballMod = 0
    x = LoadValue(cGameName, "INVADER")
    If x <> "" Then InvaderMod = CInt(x) Else InvaderMod = 0
    x = LoadValue(cGameName, "LAMP")
    If x <> "" Then TownSquarePostMod = CInt(x) Else TownSquarePostMod = 0
    x = LoadValue(cGameName, "MYSTIC")
    If x <> "" Then MysticSeerMod = CInt(x) Else MysticSeerMod = 0
    x = LoadValue(cGameName, "PIANO")
    If x <> "" Then PianoMod = CInt(x) Else PianoMod = 0
    x = LoadValue(cGameName, "PYRAMID")
    If x <> "" Then PyramidMod = CInt(x) Else PyramidMod = 0
    x = LoadValue(cGameName, "SCOOP")
    If x <> "" Then ScoopLight = CInt(x) Else ScoopLight = 0
    x = LoadValue(cGameName, "SLOT")
    If x <> "" Then SlotMachineMod = CInt(x) Else SlotMachineMod = 0
    x = LoadValue(cGameName, "SPIRAL")
    If x <> "" Then SpiralMod = CInt(x) Else SpiralMod = 0
    x = LoadValue(cGameName, "SIGN")
    If x <> "" Then SpiralSignMod = CInt(x) Else SpiralSignMod = 0
    x = LoadValue(cGameName, "TARGET")
    If x <> "" Then TargetMod = CInt(x) Else TargetMod = 0
    x = LoadValue(cGameName, "TV")
    If x <> "" Then TVMod = CInt(x) Else TVMod = 0
    x = LoadValue(cGameName, "DYNBALLSH")
    If x <> "" Then DynamicBallShadowsOn = CInt(x) Else DynamicBallShadowsOn = 1
End Sub

Sub swSlotReel_Hit:SlotReelTimer.enabled = 1:End Sub

Dim SlotPic : SlotPic = 0
Sub SlotReelTimer_Timer()
    SlotPic = SlotPic + 1
    if SlotPic > 10 Then SlotReelTimer.enabled = 0:ResetSlot.enabled = 1
    SlotReel.imageA = "slot_" & SlotPic
End Sub

Sub ResetSlot_Timer()
    SlotReel.imageA = "slot_10":vpmtimer.addtimer 150, "SlotReel.imageA = ""slot_0"" '":SlotPic = 0
    If SlotPic = 0 then ResetSlot.enabled = 0
End Sub

Dim TVPic : TVPic = 0
Sub TVTimer_Timer()
    TVPic = TVPic + 1
    if TVPic = 34 Then TVPic = 2
    Frame.imageA = "tv_" & TVPic
End Sub

Sub SpiralMove_Timer()
    SpiralToy_BM_Dark_Room.RotZ = SpiralToy_BM_Dark_Room.RotZ + 10
	dim x : x = SpiralToy_BM_Dark_Room.RotZ
	SpiralToy_LM_Lit_Room.RotZ = x ' VLM.Props;LM;2;SpiralToy
	SpiralToy_LM_GI_Left.RotZ = x ' VLM.Props;LM;2;SpiralToy
	SpiralToy_LM_Flashers_f18.RotZ = x ' VLM.Props;LM;2;SpiralToy
	SpiralToy_LM_Flashers_f20.RotZ = x ' VLM.Props;LM;2;SpiralToy
	SpiralToy_LM_Flashers_f28.RotZ = x ' VLM.Props;LM;2;SpiralToy
	SpiralToy_LM_Flashers_f38.RotZ = x ' VLM.Props;LM;2;SpiralToy
	SpiralToy_LM_Flashers_f40.RotZ = x ' VLM.Props;LM;2;SpiralToy
End Sub

Dim InvLR, InvC : InvLR = 0 : InvC = 0
Sub InvaderTimer_Timer
	InvLR = InvLR + 1
	InvC = InvC + 1
	If InvLR >= 4 Then InvLR = 0
	If InvC >= 10 Then InvC = 0
	Lampz.state(105) = InvC < 5
	Lampz.state(106) = InvLR < 2
	Lampz.state(107) = InvLR >= 2
End SUb

Sub UpdateMods
	Dim enabled

	If StagedFlipperMod = 1 Then
		keyStagedFlipperL = KeyUpperLeft
		keyStagedFlipperR = KeyUpperRight
	Else
		keyStagedFlipperL = LeftFlipperKey
		keyStagedFlipperR = RightFlipperKey
	End If


	enabled = BumperPostsMod
	Rubber9.collidable = enabled
	Rubber29.collidable = enabled
	BumperPegs_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;BumperPegs
	BumperPegs_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;BumperPegs
	BumperPegs_LM_GI_Left.Visible = enabled ' VLM.Props;LM;1;BumperPegs
	BumperPegs_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;BumperPegs
	BumperPegs_LM_Mod_l109.Visible = enabled ' VLM.Props;LM;1;BumperPegs
	BumperPegs_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;BumperPegs
	BumperPegs_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;BumperPegs
	BumperPegs_LM_Inserts_l38.Visible = enabled ' VLM.Props;LM;1;BumperPegs
	BumperPegs_LM_Bumpers_l61.Visible = enabled ' VLM.Props;LM;1;BumperPegs
	BumperPegs_LM_Bumpers_l62.Visible = enabled ' VLM.Props;LM;1;BumperPegs
	BumperPegs_LM_Inserts_l64.Visible = enabled ' VLM.Props;LM;1;BumperPegs

	enabled = CameraMod
	Camera_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;Camera
	Camera_LM_Mod_l110.Visible = enabled ' VLM.Props;LM;1;Camera
	Camera_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;Camera
	Camera_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;Camera
	Camera_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;Camera
	Camera_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;1;Camera
	Camera_LM_Inserts_l55.Visible = enabled ' VLM.Props;LM;1;Camera
	Camera_LM_Inserts_l65.Visible = enabled ' VLM.Props;LM;1;Camera

	enabled = 1 - BWClockMod
	Clock_Color_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;Clock.Color
	Clock_Color_LM_GI_Clock.Visible = enabled ' VLM.Props;LM;1;Clock.Color
	Clock_Color_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;Clock.Color
	Clock_Color_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;Clock.Color
	Clock_Color_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;Clock.Color
	Clock_Color_LM_Flashers_f20.Visible = enabled ' VLM.Props;LM;1;Clock.Color
	Clock_Color_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;1;Clock.Color
	Clock_Color_LM_Flashers_f41.Visible = enabled ' VLM.Props;LM;1;Clock.Color
	Clock_Color_LM_Inserts_l72.Visible = enabled ' VLM.Props;LM;1;Clock.Color
	Clock_Color_LM_Inserts_l73.Visible = enabled ' VLM.Props;LM;1;Clock.Color
	Clock_Color_LM_Inserts_l74.Visible = enabled ' VLM.Props;LM;1;Clock.Color
	Clock_Color_LM_Inserts_l75.Visible = enabled ' VLM.Props;LM;1;Clock.Color
	Clock_Color_LM_Flashers_l83.Visible = enabled ' VLM.Props;LM;1;Clock.Color
	Clock_Color_LM_Flashers_l84.Visible = enabled ' VLM.Props;LM;1;Clock.Color

	enabled = BWClockMod
	Clock_White_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;Clock.White
	Clock_White_LM_GI_Clock.Visible = enabled ' VLM.Props;LM;1;Clock.White
	Clock_White_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;Clock.White
	Clock_White_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;Clock.White
	Clock_White_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;Clock.White
	Clock_White_LM_Flashers_f20.Visible = enabled ' VLM.Props;LM;1;Clock.White
	Clock_White_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;1;Clock.White
	Clock_White_LM_Flashers_f41.Visible = enabled ' VLM.Props;LM;1;Clock.White
	Clock_White_LM_Inserts_l57.Visible = enabled ' VLM.Props;LM;1;Clock.White
	Clock_White_LM_Inserts_l58.Visible = enabled ' VLM.Props;LM;1;Clock.White
	Clock_White_LM_Inserts_l72.Visible = enabled ' VLM.Props;LM;1;Clock.White
	Clock_White_LM_Inserts_l73.Visible = enabled ' VLM.Props;LM;1;Clock.White
	Clock_White_LM_Inserts_l74.Visible = enabled ' VLM.Props;LM;1;Clock.White
	Clock_White_LM_Inserts_l75.Visible = enabled ' VLM.Props;LM;1;Clock.White
	Clock_White_LM_Flashers_l83.Visible = enabled ' VLM.Props;LM;1;Clock.White
	Clock_White_LM_Flashers_l84.Visible = enabled ' VLM.Props;LM;1;Clock.White
	
	enabled = MiniClockMod
	ClockToy_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;ClockToy
	ClockToy_LM_GI_Clock.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_GI_Left.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_GI_MiniPF.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Flashers_f20.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Flashers_f38.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Flashers_f39.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Flashers_f40.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Flashers_f41.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Inserts_l51.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Inserts_l52.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Inserts_l53.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Inserts_l54.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Inserts_l56.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Inserts_l57.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Inserts_l58.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Inserts_l65.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Inserts_l71.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Inserts_l72.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Inserts_l73.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Inserts_l74.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Inserts_l75.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Flashers_l81.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Inserts_l82.Visible = enabled ' VLM.Props;LM;1;ClockToy
	ClockToy_LM_Flashers_l83.Visible = enabled ' VLM.Props;LM;1;ClockToy

	enabled = (FlipperType = 0)
	FlipperL_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;2;FlipperL
	FlipperL_LM_GI_Clock.Visible = enabled ' VLM.Props;LM;2;FlipperL
	FlipperL_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;2;FlipperL
	FlipperL_LM_GI_Left.Visible = enabled ' VLM.Props;LM;2;FlipperL
	FlipperL_LM_GI_Right.Visible = enabled ' VLM.Props;LM;2;FlipperL
	FlipperL_LM_Inserts_l11.Visible = enabled ' VLM.Props;LM;2;FlipperL
	FlipperL_LM_Inserts_l13.Visible = enabled ' VLM.Props;LM;2;FlipperL
	FlipperL_LM_Inserts_l14.Visible = enabled ' VLM.Props;LM;2;FlipperL
	FlipperL_LM_Inserts_l41.Visible = enabled ' VLM.Props;LM;2;FlipperL
	FlipperL_LM_Inserts_l42.Visible = enabled ' VLM.Props;LM;2;FlipperL
	FlipperL_LM_Inserts_l43.Visible = enabled ' VLM.Props;LM;2;FlipperL
	FlipperL_LM_Inserts_l44.Visible = enabled ' VLM.Props;LM;2;FlipperL
	FlipperL_LM_Inserts_l45.Visible = enabled ' VLM.Props;LM;2;FlipperL
	FlipperL_LM_Inserts_l46.Visible = enabled ' VLM.Props;LM;2;FlipperL
	FlipperL_LM_Inserts_l47.Visible = enabled ' VLM.Props;LM;2;FlipperL
	FlipperL1_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;2;FlipperL1
	FlipperL1_LM_Mod_l110.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_GI_Clock.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_GI_Left.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_GI_MiniPF.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_Flashers_f39.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_Flashers_f40.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_Inserts_l51.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_Inserts_l52.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_Inserts_l53.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_Inserts_l54.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_Inserts_l55.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_Inserts_l56.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_Inserts_l65.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_Inserts_l72.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_Inserts_l73.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_Flashers_l81.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperL1_LM_Inserts_l82.Visible = enabled ' VLM.Props;LM;2;FlipperL1
	FlipperR_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;2;FlipperR
	FlipperR_LM_GI_Clock.Visible = enabled ' VLM.Props;LM;2;FlipperR
	FlipperR_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;2;FlipperR
	FlipperR_LM_GI_Left.Visible = enabled ' VLM.Props;LM;2;FlipperR
	FlipperR_LM_GI_Right.Visible = enabled ' VLM.Props;LM;2;FlipperR
	FlipperR_LM_Inserts_l11.Visible = enabled ' VLM.Props;LM;2;FlipperR
	FlipperR_LM_Inserts_l23.Visible = enabled ' VLM.Props;LM;2;FlipperR
	FlipperR_LM_Inserts_l24.Visible = enabled ' VLM.Props;LM;2;FlipperR
	FlipperR_LM_Inserts_l41.Visible = enabled ' VLM.Props;LM;2;FlipperR
	FlipperR_LM_Inserts_l42.Visible = enabled ' VLM.Props;LM;2;FlipperR
	FlipperR_LM_Inserts_l43.Visible = enabled ' VLM.Props;LM;2;FlipperR
	FlipperR_LM_Inserts_l44.Visible = enabled ' VLM.Props;LM;2;FlipperR
	FlipperR_LM_Inserts_l45.Visible = enabled ' VLM.Props;LM;2;FlipperR
	FlipperR_LM_Inserts_l46.Visible = enabled ' VLM.Props;LM;2;FlipperR
	FlipperR_LM_Inserts_l47.Visible = enabled ' VLM.Props;LM;2;FlipperR
	FlipperR_LM_Inserts_l48.Visible = enabled ' VLM.Props;LM;2;FlipperR
	FlipperR_LM_Inserts_l66.Visible = enabled ' VLM.Props;LM;2;FlipperR
	FlipperR1_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;2;FlipperR1
	FlipperR1_LM_GI_Clock.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_GI_Right.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Mod_l106.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Mod_l107.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Mod_l111.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Flashers_f37.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Inserts_l18.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Inserts_l21.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Inserts_l22.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Inserts_l23.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Inserts_l24.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Inserts_l25.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Inserts_l26.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Inserts_l27.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Inserts_l28.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Inserts_l34.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Inserts_l36.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Inserts_l71.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Inserts_l72.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Flashers_l85.Visible = enabled ' VLM.Props;LM;2;FlipperR1
	FlipperR1_LM_Flashers_l86.Visible = enabled ' VLM.Props;LM;2;FlipperR1

	enabled = (FlipperType = 1)
	FlipperSpL_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;2;FlipperSpL
	FlipperSpL_LM_GI_Clock.Visible = enabled ' VLM.Props;LM;2;FlipperSpL
	FlipperSpL_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;2;FlipperSpL
	FlipperSpL_LM_GI_Left.Visible = enabled ' VLM.Props;LM;2;FlipperSpL
	FlipperSpL_LM_GI_Right.Visible = enabled ' VLM.Props;LM;2;FlipperSpL
	FlipperSpL_LM_Inserts_l11.Visible = enabled ' VLM.Props;LM;2;FlipperSpL
	FlipperSpL_LM_Inserts_l13.Visible = enabled ' VLM.Props;LM;2;FlipperSpL
	FlipperSpL_LM_Inserts_l14.Visible = enabled ' VLM.Props;LM;2;FlipperSpL
	FlipperSpL_LM_Inserts_l41.Visible = enabled ' VLM.Props;LM;2;FlipperSpL
	FlipperSpL_LM_Inserts_l42.Visible = enabled ' VLM.Props;LM;2;FlipperSpL
	FlipperSpL_LM_Inserts_l43.Visible = enabled ' VLM.Props;LM;2;FlipperSpL
	FlipperSpL_LM_Inserts_l44.Visible = enabled ' VLM.Props;LM;2;FlipperSpL
	FlipperSpL_LM_Inserts_l45.Visible = enabled ' VLM.Props;LM;2;FlipperSpL
	FlipperSpL_LM_Inserts_l46.Visible = enabled ' VLM.Props;LM;2;FlipperSpL
	FlipperSpL_LM_Inserts_l47.Visible = enabled ' VLM.Props;LM;2;FlipperSpL
	FlipperSpL1_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;2;FlipperSpL1
	FlipperSpL1_LM_GI_Clock.Visible = enabled ' VLM.Props;LM;2;FlipperSpL1
	FlipperSpL1_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;2;FlipperSpL1
	FlipperSpL1_LM_GI_Left.Visible = enabled ' VLM.Props;LM;2;FlipperSpL1
	FlipperSpL1_LM_GI_MiniPF.Visible = enabled ' VLM.Props;LM;2;FlipperSpL1
	FlipperSpL1_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;2;FlipperSpL1
	FlipperSpL1_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;2;FlipperSpL1
	FlipperSpL1_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;2;FlipperSpL1
	FlipperSpL1_LM_Flashers_f39.Visible = enabled ' VLM.Props;LM;2;FlipperSpL1
	FlipperSpL1_LM_Flashers_f40.Visible = enabled ' VLM.Props;LM;2;FlipperSpL1
	FlipperSpL1_LM_Inserts_l51.Visible = enabled ' VLM.Props;LM;2;FlipperSpL1
	FlipperSpL1_LM_Inserts_l52.Visible = enabled ' VLM.Props;LM;2;FlipperSpL1
	FlipperSpL1_LM_Inserts_l53.Visible = enabled ' VLM.Props;LM;2;FlipperSpL1
	FlipperSpL1_LM_Inserts_l54.Visible = enabled ' VLM.Props;LM;2;FlipperSpL1
	FlipperSpL1_LM_Inserts_l56.Visible = enabled ' VLM.Props;LM;2;FlipperSpL1
	FlipperSpL1_LM_Inserts_l72.Visible = enabled ' VLM.Props;LM;2;FlipperSpL1
	FlipperSpL1_LM_Inserts_l73.Visible = enabled ' VLM.Props;LM;2;FlipperSpL1
	FlipperSpL1_LM_Flashers_l81.Visible = enabled ' VLM.Props;LM;2;FlipperSpL1
	FlipperSpL1_LM_Inserts_l82.Visible = enabled ' VLM.Props;LM;2;FlipperSpL1
	FlipperSpR_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;2;FlipperSpR
	FlipperSpR_LM_GI_Clock.Visible = enabled ' VLM.Props;LM;2;FlipperSpR
	FlipperSpR_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;2;FlipperSpR
	FlipperSpR_LM_GI_Left.Visible = enabled ' VLM.Props;LM;2;FlipperSpR
	FlipperSpR_LM_GI_Right.Visible = enabled ' VLM.Props;LM;2;FlipperSpR
	FlipperSpR_LM_Inserts_l11.Visible = enabled ' VLM.Props;LM;2;FlipperSpR
	FlipperSpR_LM_Inserts_l23.Visible = enabled ' VLM.Props;LM;2;FlipperSpR
	FlipperSpR_LM_Inserts_l24.Visible = enabled ' VLM.Props;LM;2;FlipperSpR
	FlipperSpR_LM_Inserts_l41.Visible = enabled ' VLM.Props;LM;2;FlipperSpR
	FlipperSpR_LM_Inserts_l42.Visible = enabled ' VLM.Props;LM;2;FlipperSpR
	FlipperSpR_LM_Inserts_l43.Visible = enabled ' VLM.Props;LM;2;FlipperSpR
	FlipperSpR_LM_Inserts_l44.Visible = enabled ' VLM.Props;LM;2;FlipperSpR
	FlipperSpR_LM_Inserts_l45.Visible = enabled ' VLM.Props;LM;2;FlipperSpR
	FlipperSpR_LM_Inserts_l46.Visible = enabled ' VLM.Props;LM;2;FlipperSpR
	FlipperSpR_LM_Inserts_l47.Visible = enabled ' VLM.Props;LM;2;FlipperSpR
	FlipperSpR_LM_Inserts_l48.Visible = enabled ' VLM.Props;LM;2;FlipperSpR
	FlipperSpR_LM_Inserts_l66.Visible = enabled ' VLM.Props;LM;2;FlipperSpR
	FlipperSpR1_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;2;FlipperSpR1
	FlipperSpR1_LM_GI_Clock.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_GI_Right.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Mod_l106.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Mod_l107.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Mod_l111.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Flashers_f37.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Inserts_l18.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Inserts_l21.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Inserts_l23.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Inserts_l24.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Inserts_l25.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Inserts_l26.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Inserts_l27.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Inserts_l28.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Inserts_l36.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Inserts_l71.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Inserts_l72.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Flashers_l85.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1
	FlipperSpR1_LM_Flashers_l86.Visible = enabled ' VLM.Props;LM;2;FlipperSpR1

	enabled = GumballMod
	Gumballs_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;Gumballs
	Gumballs_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;Gumballs
	Gumballs_LM_GI_Left.Visible = enabled ' VLM.Props;LM;1;Gumballs
	Gumballs_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;Gumballs
	Gumballs_LM_Flashers_f20.Visible = enabled ' VLM.Props;LM;1;Gumballs
	Gumballs_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;1;Gumballs
	Gumballs_LM_Flashers_f38.Visible = enabled ' VLM.Props;LM;1;Gumballs
	Gumballs_LM_Flashers_f39.Visible = enabled ' VLM.Props;LM;1;Gumballs
	Gumballs_LM_Flashers_f40.Visible = enabled ' VLM.Props;LM;1;Gumballs
	
	enabled = InvaderMod
	InvaderTimer.Enabled = enabled
	InvaderToy_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;InvaderToy
	InvaderToy_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;InvaderToy
	InvaderToy_LM_GI_Left.Visible = enabled ' VLM.Props;LM;1;InvaderToy
	InvaderToy_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;InvaderToy
	InvaderToy_LM_Mod_l105.Visible = enabled ' VLM.Props;LM;1;InvaderToy
	InvaderToy_LM_Mod_l106.Visible = enabled ' VLM.Props;LM;1;InvaderToy
	InvaderToy_LM_Mod_l107.Visible = enabled ' VLM.Props;LM;1;InvaderToy
	InvaderToy_LM_Mod_l111.Visible = enabled ' VLM.Props;LM;1;InvaderToy
	InvaderToy_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;InvaderToy
	InvaderToy_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;InvaderToy
	InvaderToy_LM_Flashers_l86.Visible = enabled ' VLM.Props;LM;1;InvaderToy
	
	enabled = CabinetSide
	
	enabled = MysticSeerMod
	MysticSeerToy_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;MysticSeerToy
	MysticSeerToy_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;MysticSeerToy
	MysticSeerToy_LM_GI_Left.Visible = enabled ' VLM.Props;LM;1;MysticSeerToy
	MysticSeerToy_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;MysticSeerToy
	MysticSeerToy_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;MysticSeerToy
	MysticSeerToy_LM_Inserts_l33.Visible = enabled ' VLM.Props;LM;1;MysticSeerToy
	MysticSeerToy_LM_Bumpers_l62.Visible = enabled ' VLM.Props;LM;1;MysticSeerToy
	
	enabled = PianoMod
	Piano_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;Piano
	Piano_LM_GI_Clock.Visible = enabled ' VLM.Props;LM;1;Piano
	Piano_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;Piano
	Piano_LM_GI_Left.Visible = enabled ' VLM.Props;LM;1;Piano
	Piano_LM_GI_MiniPF.Visible = enabled ' VLM.Props;LM;1;Piano
	Piano_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;Piano
	Piano_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;Piano
	Piano_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;Piano
	Piano_LM_Flashers_f20.Visible = enabled ' VLM.Props;LM;1;Piano
	Piano_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;1;Piano
	Piano_LM_Flashers_f38.Visible = enabled ' VLM.Props;LM;1;Piano
	Piano_LM_Flashers_f40.Visible = enabled ' VLM.Props;LM;1;Piano
	Piano_LM_Flashers_f41.Visible = enabled ' VLM.Props;LM;1;Piano
	Piano_LM_Inserts_l73.Visible = enabled ' VLM.Props;LM;1;Piano
	Piano_LM_Inserts_l75.Visible = enabled ' VLM.Props;LM;1;Piano
	Piano_LM_Flashers_l83.Visible = enabled ' VLM.Props;LM;1;Piano
	Piano_LM_Flashers_l84.Visible = enabled ' VLM.Props;LM;1;Piano
	
	enabled = PyramidMod
	Pyramid_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;Pyramid
	Pyramid_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;Pyramid
	Pyramid_LM_GI_MiniPF.Visible = enabled ' VLM.Props;LM;1;Pyramid
	Pyramid_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;Pyramid
	Pyramid_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;Pyramid
	Pyramid_LM_Flashers_f19.Visible = enabled ' VLM.Props;LM;1;Pyramid
	Pyramid_LM_Flashers_f20.Visible = enabled ' VLM.Props;LM;1;Pyramid
	Pyramid_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;1;Pyramid
	Pyramid_LM_Flashers_f38.Visible = enabled ' VLM.Props;LM;1;Pyramid
	Pyramid_LM_Flashers_f39.Visible = enabled ' VLM.Props;LM;1;Pyramid
	Pyramid_LM_Flashers_f40.Visible = enabled ' VLM.Props;LM;1;Pyramid
	Pyramid_LM_Flashers_f41.Visible = enabled ' VLM.Props;LM;1;Pyramid
	Pyramid_LM_Inserts_l76.Visible = enabled ' VLM.Props;LM;1;Pyramid

	enabled = RobotMod
	Robot_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;Robot
	Robot_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;Robot
	Robot_LM_GI_Left.Visible = enabled ' VLM.Props;LM;1;Robot
	Robot_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;Robot
	Robot_LM_Flashers_f20.Visible = enabled ' VLM.Props;LM;1;Robot
	Robot_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;1;Robot
	Robot_LM_Flashers_f38.Visible = enabled ' VLM.Props;LM;1;Robot
	Robot_LM_Flashers_f40.Visible = enabled ' VLM.Props;LM;1;Robot
	Robot_LM_Flashers_f41.Visible = enabled ' VLM.Props;LM;1;Robot
	Robot_LM_Inserts_l75.Visible = enabled ' VLM.Props;LM;1;Robot

	enabled = RocketMod
	RocketToy_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;RocketToy
	RocketToy_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;RocketToy
	RocketToy_LM_GI_Left.Visible = enabled ' VLM.Props;LM;1;RocketToy
	RocketToy_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;RocketToy
	RocketToy_LM_Mod_l111.Visible = enabled ' VLM.Props;LM;1;RocketToy
	RocketToy_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;RocketToy
	RocketToy_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;RocketToy
	RocketToy_LM_Flashers_f20.Visible = enabled ' VLM.Props;LM;1;RocketToy
	RocketToy_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;1;RocketToy
	RocketToy_LM_Inserts_l14.Visible = enabled ' VLM.Props;LM;1;RocketToy
	RocketToy_LM_Inserts_l22.Visible = enabled ' VLM.Props;LM;1;RocketToy
	RocketToy_LM_Inserts_l23.Visible = enabled ' VLM.Props;LM;1;RocketToy
	RocketToy_LM_Inserts_l24.Visible = enabled ' VLM.Props;LM;1;RocketToy
	RocketToy_LM_Inserts_l41.Visible = enabled ' VLM.Props;LM;1;RocketToy
	RocketToy_LM_Inserts_l42.Visible = enabled ' VLM.Props;LM;1;RocketToy
	RocketToy_LM_Inserts_l43.Visible = enabled ' VLM.Props;LM;1;RocketToy
	RocketToy_LM_Inserts_l44.Visible = enabled ' VLM.Props;LM;1;RocketToy
	RocketToy_LM_Inserts_l45.Visible = enabled ' VLM.Props;LM;1;RocketToy
	RocketToy_LM_Inserts_l48.Visible = enabled ' VLM.Props;LM;1;RocketToy
	RocketToy_LM_Inserts_l66.Visible = enabled ' VLM.Props;LM;1;RocketToy

	enabled = SpiralSignMod
	Sign_Spiral_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;Sign_Spiral
	Sign_Spiral_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;Sign_Spiral
	Sign_Spiral_LM_GI_Left.Visible = enabled ' VLM.Props;LM;1;Sign_Spiral
	Sign_Spiral_LM_GI_MiniPF.Visible = enabled ' VLM.Props;LM;1;Sign_Spiral
	Sign_Spiral_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;Sign_Spiral
	Sign_Spiral_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;Sign_Spiral
	Sign_Spiral_LM_Flashers_f20.Visible = enabled ' VLM.Props;LM;1;Sign_Spiral
	Sign_Spiral_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;1;Sign_Spiral
	Sign_Spiral_LM_Flashers_f38.Visible = enabled ' VLM.Props;LM;1;Sign_Spiral
	Sign_Spiral_LM_Flashers_f39.Visible = enabled ' VLM.Props;LM;1;Sign_Spiral
	Sign_Spiral_LM_Flashers_f40.Visible = enabled ' VLM.Props;LM;1;Sign_Spiral
	Sign_Spiral_LM_Inserts_l54.Visible = enabled ' VLM.Props;LM;1;Sign_Spiral
	Sign_Spiral_LM_Flashers_l81.Visible = enabled ' VLM.Props;LM;1;Sign_Spiral
	
	enabled = SlotMachineMod
	swSlotReel.enabled = enabled
	SlotReel.visible = enabled
	SlotMachineToy_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;SlotMachineToy
	SlotMachineToy_LM_GI_Clock.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_GI_Left.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_Mod_l106.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_Mod_l111.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_Flashers_f20.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_Flashers_f38.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_Flashers_f40.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_Flashers_f41.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_Inserts_l64.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_Inserts_l65.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_Inserts_l67.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_Inserts_l71.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_Inserts_l72.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_Inserts_l73.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_Inserts_l75.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_Flashers_l85.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	SlotMachineToy_LM_Flashers_l86.Visible = enabled ' VLM.Props;LM;1;SlotMachineToy
	
	enabled = SpiralMod
	SpiralToy_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;3;SpiralToy
	SpiralToy_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;3;SpiralToy
	SpiralToy_LM_GI_Left.Visible = enabled ' VLM.Props;LM;3;SpiralToy
	SpiralToy_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;3;SpiralToy
	SpiralToy_LM_Flashers_f20.Visible = enabled ' VLM.Props;LM;3;SpiralToy
	SpiralToy_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;3;SpiralToy
	SpiralToy_LM_Flashers_f38.Visible = enabled ' VLM.Props;LM;3;SpiralToy
	SpiralToy_LM_Flashers_f40.Visible = enabled ' VLM.Props;LM;3;SpiralToy
	
	enabled = TVMod
	Frame.visible = enabled
	TVtoy_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;TVtoy
	TVtoy_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;TVtoy
	TVtoy_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;TVtoy
	TVtoy_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;TVtoy
	TVtoy_LM_Flashers_f20.Visible = enabled ' VLM.Props;LM;1;TVtoy
	TVtoy_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;1;TVtoy
	TVtoy_LM_Flashers_f41.Visible = enabled ' VLM.Props;LM;1;TVtoy

	enabled = TownSquarePostMod
	lTownSquarePost.visible = enabled
	Lampz.state(109) = enabled
	TownSquarePost_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;TownSquarePost
	TownSquarePost_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;TownSquarePost
	TownSquarePost_LM_GI_Left.Visible = enabled ' VLM.Props;LM;1;TownSquarePost
	TownSquarePost_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;TownSquarePost
	TownSquarePost_LM_Mod_l109.Visible = enabled ' VLM.Props;LM;1;TownSquarePost
	TownSquarePost_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;TownSquarePost
	TownSquarePost_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;TownSquarePost
	TownSquarePost_LM_Inserts_l16.Visible = enabled ' VLM.Props;LM;1;TownSquarePost
	TownSquarePost_LM_Inserts_l37.Visible = enabled ' VLM.Props;LM;1;TownSquarePost
	TownSquarePost_LM_Inserts_l38.Visible = enabled ' VLM.Props;LM;1;TownSquarePost
	TownSquarePost_LM_Bumpers_l61.Visible = enabled ' VLM.Props;LM;1;TownSquarePost
	TownSquarePost_LM_Bumpers_l63.Visible = enabled ' VLM.Props;LM;1;TownSquarePost
	TownSquarePost_LM_Inserts_l64.Visible = enabled ' VLM.Props;LM;1;TownSquarePost
	TownSquarePost_LM_Flashers_l85.Visible = enabled ' VLM.Props;LM;1;TownSquarePost
	
	enabled = ExtraMagnet
	sw82.enabled = enabled
	sw82_help.enabled = enabled
	UpperRightMagnet.enabled = enabled	
	URMagnet_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;URMagnet
	URMagnet_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;URMagnet
	URMagnet_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;URMagnet
	URMagnet_LM_Flashers_f20.Visible = enabled ' VLM.Props;LM;1;URMagnet
	URMagnet_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;1;URMagnet
	URMagnet_LM_Flashers_f41.Visible = enabled ' VLM.Props;LM;1;URMagnet

	If TargetMod = 0 Then enabled = True else enabled = False
	sw47_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw47
	sw47_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;sw47
	sw47_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;sw47
	sw47_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;1;sw47
	sw47_LM_Inserts_l51.Visible = enabled ' VLM.Props;LM;1;sw47
	sw47_LM_Inserts_l82.Visible = enabled ' VLM.Props;LM;1;sw47
	sw48_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw48
	sw48_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;sw48
	sw48_LM_GI_Left.Visible = enabled ' VLM.Props;LM;1;sw48
	sw48_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;sw48
	sw48_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;sw48
	sw48_LM_Inserts_l13.Visible = enabled ' VLM.Props;LM;1;sw48
	sw48_LM_Inserts_l35.Visible = enabled ' VLM.Props;LM;1;sw48
	sw48_LM_Inserts_l37.Visible = enabled ' VLM.Props;LM;1;sw48
	sw48_LM_Inserts_l43.Visible = enabled ' VLM.Props;LM;1;sw48
	sw48_LM_Inserts_l44.Visible = enabled ' VLM.Props;LM;1;sw48
	sw48_LM_Inserts_l45.Visible = enabled ' VLM.Props;LM;1;sw48
	sw48_LM_Inserts_l64.Visible = enabled ' VLM.Props;LM;1;sw48
	sw64_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw64
	sw64_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;sw64
	sw64_LM_GI_MiniPF.Visible = enabled ' VLM.Props;LM;1;sw64
	sw64_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;1;sw64
	sw64_LM_Inserts_l57.Visible = enabled ' VLM.Props;LM;1;sw64
	sw64_LM_Inserts_l75.Visible = enabled ' VLM.Props;LM;1;sw64
	sw65_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw65
	sw65_LM_GI_Clock.Visible = enabled ' VLM.Props;LM;1;sw65
	sw65_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;sw65
	sw65_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;sw65
	sw65a_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw65a
	sw65a_LM_GI_Clock.Visible = enabled ' VLM.Props;LM;1;sw65a
	sw65a_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;sw65a
	sw65a_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;sw65a
	sw65a_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;sw65a
	sw65a_LM_Flashers_f20.Visible = enabled ' VLM.Props;LM;1;sw65a
	sw65a_LM_Flashers_l83.Visible = enabled ' VLM.Props;LM;1;sw65a
	sw66_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw66
	sw66_LM_GI_Clock.Visible = enabled ' VLM.Props;LM;1;sw66
	sw66_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;sw66
	sw66_LM_GI_MiniPF.Visible = enabled ' VLM.Props;LM;1;sw66
	sw66_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;sw66
	sw66_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;sw66
	sw66_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;1;sw66
	sw66_LM_Inserts_l72.Visible = enabled ' VLM.Props;LM;1;sw66
	sw66_LM_Inserts_l73.Visible = enabled ' VLM.Props;LM;1;sw66
	sw67_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw67
	sw67_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;sw67
	sw67_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;sw67
	sw67_LM_Inserts_l72.Visible = enabled ' VLM.Props;LM;1;sw67
	sw67_LM_Inserts_l73.Visible = enabled ' VLM.Props;LM;1;sw67
	sw68_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw68
	sw68_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;sw68
	sw68_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;sw68
	sw68_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;sw68
	sw68_LM_Inserts_l71.Visible = enabled ' VLM.Props;LM;1;sw68
	sw68_LM_Inserts_l73.Visible = enabled ' VLM.Props;LM;1;sw68
	sw77_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw77
	sw77_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;sw77
	sw77_LM_GI_Left.Visible = enabled ' VLM.Props;LM;1;sw77
	sw77_LM_Mod_l109.Visible = enabled ' VLM.Props;LM;1;sw77
	sw77_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;sw77
	sw77_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;sw77
	sw77_LM_Inserts_l16.Visible = enabled ' VLM.Props;LM;1;sw77
	sw77_LM_Inserts_l17.Visible = enabled ' VLM.Props;LM;1;sw77
	sw77_LM_Inserts_l37.Visible = enabled ' VLM.Props;LM;1;sw77
	sw77_LM_Inserts_l38.Visible = enabled ' VLM.Props;LM;1;sw77
	sw77_LM_Inserts_l64.Visible = enabled ' VLM.Props;LM;1;sw77
	sw77_LM_Inserts_l65.Visible = enabled ' VLM.Props;LM;1;sw77
	sw78_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw78
	sw78_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;sw78
	sw78_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;sw78
	sw78_LM_Mod_l109.Visible = enabled ' VLM.Props;LM;1;sw78
	sw78_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;sw78
	sw78_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;sw78
	sw78_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;1;sw78
	sw78_LM_Inserts_l17.Visible = enabled ' VLM.Props;LM;1;sw78
	sw78_LM_Bumpers_l63.Visible = enabled ' VLM.Props;LM;1;sw78
	sw78_LM_Inserts_l65.Visible = enabled ' VLM.Props;LM;1;sw78

	enabled = Not enabled
	sw47m_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw47m
	sw47m_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;sw47m
	sw47m_LM_Inserts_l82.Visible = enabled ' VLM.Props;LM;1;sw47m
	sw48m_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw48m
	sw48m_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;sw48m
	sw48m_LM_GI_Left.Visible = enabled ' VLM.Props;LM;1;sw48m
	sw48m_LM_Inserts_l37.Visible = enabled ' VLM.Props;LM;1;sw48m
	sw64m_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw64m
	sw64m_LM_GI_Clock.Visible = enabled ' VLM.Props;LM;1;sw64m
	sw64m_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;sw64m
	sw64m_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;sw64m
	sw64m_LM_Inserts_l75.Visible = enabled ' VLM.Props;LM;1;sw64m
	sw64m_LM_Flashers_l83.Visible = enabled ' VLM.Props;LM;1;sw64m
	sw64m_LM_Flashers_l84.Visible = enabled ' VLM.Props;LM;1;sw64m
	sw65am_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw65am
	sw65am_LM_GI_Clock.Visible = enabled ' VLM.Props;LM;1;sw65am
	sw65am_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;sw65am
	sw65am_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;sw65am
	sw65am_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;sw65am
	sw65am_LM_Flashers_f20.Visible = enabled ' VLM.Props;LM;1;sw65am
	sw65am_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;1;sw65am
	sw65m_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw65m
	sw65m_LM_GI_Clock.Visible = enabled ' VLM.Props;LM;1;sw65m
	sw65m_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;sw65m
	sw65m_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;sw65m
	sw65m_LM_Flashers_f18.Visible = enabled ' VLM.Props;LM;1;sw65m
	sw65m_LM_Flashers_f20.Visible = enabled ' VLM.Props;LM;1;sw65m
	sw65m_LM_Flashers_f28.Visible = enabled ' VLM.Props;LM;1;sw65m
	sw65m_LM_Flashers_f41.Visible = enabled ' VLM.Props;LM;1;sw65m
	sw66m_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw66m
	sw66m_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;sw66m
	sw66m_LM_Inserts_l72.Visible = enabled ' VLM.Props;LM;1;sw66m
	sw67m_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw67m
	sw67m_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;sw67m
	sw67m_LM_GI_Right.Visible = enabled ' VLM.Props;LM;1;sw67m
	sw68m_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw68m
	sw68m_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;sw68m
	sw68m_LM_Inserts_l71.Visible = enabled ' VLM.Props;LM;1;sw68m
	sw77m_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw77m
	sw77m_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;sw77m
	sw77m_LM_Mod_l109.Visible = enabled ' VLM.Props;LM;1;sw77m
	sw77m_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;sw77m
	sw77m_LM_Inserts_l64.Visible = enabled ' VLM.Props;LM;1;sw77m
	sw78m_BM_Dark_Room.Visible = enabled ' VLM.Props;BM;1;sw78m
	sw78m_LM_Lit_Room.Visible = enabled ' VLM.Props;LM;1;sw78m
	sw78m_LM_Flashers_f17.Visible = enabled ' VLM.Props;LM;1;sw78m
	sw78m_LM_Inserts_l65.Visible = enabled ' VLM.Props;LM;1;sw78m

	Lampz.state(108) = ScoopLight

	Lampz.state(150) = LightLevel
End Sub
