'***********************   VPW Presents   ***********************
'   ___ _   _______ _____  _____  ____________ _________________ 
'  |_  | | | |  _  \  __ \|  ___| |  _  \ ___ \  ___|  _  \  _  \
'    | | | | | | | | |  \/| |__   | | | | |_/ / |__ | | | | | | |
'    | | | | | | | | | __ |  __|  | | | |    /|  __|| | | | | | |
'/\__/ / |_| | |/ /| |_\ \| |___  | |/ /| |\ \| |___| |/ /| |/ / 
'\____/ \___/|___/  \____/\____/  |___/ \_| \_\____/|___/ |___/  
                                     
'****************************************************************
'
' Judge Dredd - IPDB No. 1322
' © Bally/Midway 1993
' Widebody "Superpin" Line
' https://www.ipdb.org/machine.cgi?id=1322
'
'****************************************************************
'	How To Play:
'****************************************************************
'
' - Start Button	- Start normal Game
' - Plunger 		- Launch Ball (note: real machine does not have a plunger, right firebutton launches balls)
' - Right Magnasave	- Start "Super Game" (instant multiball - needs 2 credits)
' - Left Magnasave	- Change LUT until game starts, then launches ball from left plunger lane in Air Raid / Missile mode.
'
' - How to activate DeadWorld - Hit J-U-D-G-E drop targets in order (ball in judge subway lights a letter)
' - How to start DW multiball - Get 3 balls into deadworld using left pursuit ramp after lighting J-U-D-G-E.
' - How to get Ultimate Challenge - Defeat all 4 dark judges or light all chain missions.
'
'****************************************************************
'	V-Pin Workshop Judge Dredd Team
'****************************************************************
'Sixtoe - Project Lead, VPX monkey, not sleeping, living on gin, jack of all trades, master of none.
'Tomate - The Baron of Blender, creator of 3d things, helping Sixtoe to not suck (as much) at Blender.
'iaakki - Scripting, Insert lighting.
'Benji - Physics work, deadworld & banana lamp inserts prims.
'Daphishbowl & iaakki - Custom Deadworld & Crane code.
'Cyberpez - Opto's and VUK prims, aftermarket crane models, general assistance.
'Flupper - Flipper primitives, ramp tutorials, general assistance with rendering issues.
'RothebauerW - Physics tutorial, drop and standup target bugfixing, general assistance.
'Fluffhead, Apophis & Wytle - RTX Ball Shadows.
'gtxjoe - Ball trough tweaking assistance, no more balls destroyed!
'3rdaxis - Primitive tweaks, VR cabinet improvements, Working coin door and manual.
'Embee & Kingdids - Original crane primitive and texture redraw
'DJRobX - Still answering Sixtoes coding questions...
'Rik & VPW Team - Testing
'Everyone in VPW for their support and encouragement!
'The PinMAME and VPX Developers for the programmes we all use!
'
'V1.0
'Initial Release
'
'V1.1
'Fixed missing flipper rubber sounds.
'Rebuilt entire subway entrance and fixed wall partially blocking subway (after it was added at last minute to fix a ball trap).
'Adjusted crane ramp exit hole area.
'Changed cabinet POV with 1:1 one from Hauntfreaks.
'Fixed DMD surround in VR.

Option Explicit
Randomize

'****************************************************************
'	Table Options
'****************************************************************

'///////////////////////---- Volume ----/////////////////////////
'VolumeDial is the global volume for mechanical sounds

Const VolumeDial = 0.8				'Recommended values should be no higher than 1

'///////////////////////---- VR Room ----////////////////////////

Const VRRoom = 0 					'0 - VR Room Off, 1 - Minimal Room, 2 - 360 Room, 3 - Ultra Minimal

'/////////////////////---- Cabinet Mode ----/////////////////////

Const CabinetMode = 0 				'0 - Rails, 1 - Hidden Rails

'///////////////////////---- Crane Mod ----//////////////////////

Const CraneMod = 0					'0 - Original, 1 - 3D Printed, 2 - Powder Coated Black, 3 - Metal

'/////////////////////---- Physics Mods ----/////////////////////

Const RubberizerEnabled =	1 		'0 = normal flip rubber, 1 = more lively rubber for flips, 2 = different rubberizer
Const FlipperCoilRampupMode = 0		'0 = fast, 1 = medium, 2 = slow (tap passes should work)
Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 1.0		'Level of bounces. 0.2 - 1.5 are probably usable values.

'/////////////////////---- Ball Shadows ----/////////////////////

Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow, 1 = enable dynamic ball shadow
Const AmbientShadowOn = 1			'0 = ambient ball shadow, 1 = enable ambient shadow

'//////////////---- LUT (Colour Look Up Table) ----//////////////
'0 = Fleep Natural Dark 1
'1 = Fleep Natural Dark 2
'2 = Fleep Warm Dark
'3 = Fleep Warm Bright
'4 = Fleep Warm Vivid Soft
'5 = Fleep Warm Vivid Hard
'6 = Skitso Natural and Balanced
'7 = Skitso Natural High Contrast
'8 = 3rdaxis Referenced THX Standard
'9 = CalleV Punchy Brightness and Contrast
'10 = HauntFreaks Desaturated
'11 = Tomate Washed Out 
'12 = VPW Original 1 to 1
'13 = Bassgeige
'14 = Blacklight
'15 = B&W Comic Book

Dim LUTset, DisableLUTSelector, LutToggleSound
LutToggleSound = True
LoadLUT
'LUTset = 11			' Override saved LUT for debug
SetLUT
DisableLUTSelector = 0  ' Disables the ability to change LUT option with magna saves in game when set to 1

'****************************************************************
'	END OF TABLE OPTIONS
'****************************************************************

Const BallSize = 50	'How big the ball is, don't change as it will break the physics.
Const BallMass = 1	'How "heavy" the ball is, don't change, don't change as it will break the physics.

Dim DesktopMode: DesktopMode = Table1.ShowDT
Dim UseVPMDMD
If VRRoom <> 0 Then UseVPMDMD = True Else UseVPMDMD = DesktopMode
Const UseVPMColoredDMD = true	'Passes raw colored DMD data from VPM to VPX.
Const UseVPMModSol = 1			'Modulated" solenoids for flashers / lights, give a dimming value of 0-255 rather than VPX smoothing between 0 and 1.

If Table1.ShowFSS = True or DesktopMode = False Then
	ScoreText.visible = False
End If

'Set which rom / vpinmame shortname we're using, which is "JD_L1", DO NOT use any other rom (e.g. JD_L7).
'Before Judge Dredd was released a major operator apparently expressed concern over how reliable the deadworld ball lock would be so it was deactived for production machines.
Const cGameName = "jd_l1"	'***DO NOT CHANGE***

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
debug.print Err.Description
On Error Goto 0

LoadVPM "01560000", "WPC.VBS", 3.36

'****************************************************************
'		STANDARD DEFINITIONS
'****************************************************************
'Main definitions
Const UseSolenoids = 2		'Enable fastflips for williams tables (1 disables)
Const UseLamps = 0			'Use core.vbs script light support instead of the custom lamptimer scripts
Const UseGI = 0				'Use core.vbs GI routine for older machines or machines with no GI
Const UseSync = 0			'Use vsync - Use if table runs too fast
Const HandleMech = 0		'Use core.vbs mechanicals (cvpmMech) pre-programmed into VPX.

'Sounds definitions - Defaulted to empty as they are set elsewhere in the script.
Const SSolenoidOn = ""		'Set solonoid sound when they activate
Const SSolenoidOff = ""		'Set solonoid sound when they deactivate
Const SFlipperOn = ""		'Set flipper sound when they activate
Const SFlipperOff = ""		'Set flipper sound when they deactivate
Const SCoin = ""			'Set coin entry sound

'****************************************************************
'		SOLENOID CALLBACKS
'****************************************************************
SolCallBack(1) = "CraneMag"						'Globe Magnet (Magnet inside of crane arm)
SolCallBack(2) = "bsBotVUK.SolOut"				'Bottom (Left) Vertical Up Kicker / Popper
SolCallBack(3) = "bsTopVUK.SolOut"				'Top (Right) Vertical Up Kicker / Popper
SolCallBack(4) = "CraneArm"						'Globe Arm (Crane arm movement)
SolCallBack(5) = "ResetDrops"					'Reset Drop Targets
SolCallBack(6) = "SolWheelDrive"				'Globe Motor (Rotate Deadworld)
SolCallBack(7) = "SolKnocker"					'Knocker
SolCallBack(8) = "JDPlunger"					'Right Shooter / Plunger
SolCallBack(9) = "KickBack"						'Left Shooter / Missile Launcher
SolCallBack(10) = "TripDrop"					'Trip Drop Target
SolCallBack(11) = "Diverter"					'Diverter (Top Ramp)
'SolCallBack(12) = 								'Not Used
'SolCallBack(14) = 								'Not Used
SolCallBack(13) = "JDTrough"					'Ball Trough
'SolCallBack(15) = 								'Left Sllingshot
'SolCallBack(16) = 								'Right Slingshot
SolCallBack(17) = "SetLamp 117,"				'Judge Fire Flasher (Centre Playfield)
SolCallBack(18) = "SetLamp 118,"				'Judge Fear Flasher (Centre Playfield)
SolCallBack(19) = "SetLamp 119,"				'Judge Death Flasher (Centre Playfield)
SolCallBack(20) = "SetLamp 120,"				'Judge Mortis Flasher (Centre Playfield)
SolCallBack(21) = "FlashSol21"		'"SetLamp 121," 	'Pursuit Left Flashers (Police Lights)
SolCallBack(22) = "FlashSol22"		'"SetLamp 122,"	'Pursuit Right Flashers (Police Lights)
SolCallBack(23) = "SetLamp 123,"				'Blackout Flasher (Centre playfield on bike)
SolCallBack(24) = "SetLamp 124,"				'Cursed Earth Flashers (Under Playfield underneath Deadworld)
SolCallBack(25) = "SetLamp 125,"				'Lower Left Flashers x2 (Left Shooter Exit & Lower VUK Exit plastics)
SolCallBack(26) = "FlashSol26"		'"SetLamp 126,"				'Globe Flashers (Inside Deadworld)
SolCallBack(27) = "SetLamp 127,"				'Under Ramp Flashers x2 (Under stakeout and sniper ramp)
SolCallBack(28) = "SetLamp 128,"				'Backglass Flasher

'Flippers - Controlled directly from VPX
SolCallback(sLRFlipper) = "SolRFlipper"			'Right Flipper
SolCallback(sLLFlipper) = "SolLFlipper"			'Left Flipper
SolCallback(sURFlipper) = "SolURFlipper"		'Upper Right Flipper
SolCallback(sULFlipper) = "SolULFlipper"		'Upper Left Flipper

'****************************************************************
'		TABLE INIT
'****************************************************************

'Using table width and height in script slows down the performance, so they're set here once.
Dim tablewidth: tablewidth = Table1.width
Dim tableheight: tableheight = Table1.height
Dim bsTopVUK, bsBotVUK
Dim activeShadowBall
Dim CoindoorIsOpen, CDA
Dim ManualPageNumber: ManualPageNumber = 1
Dim DoorReady: DoorReady = true
Dim ManualMove:ManualMove= 1
Dim DoorMove:DoorMove=-1
Dim DCO:DCO=False
Dim MA:MA=False

Sub Table1_Init()
	vpmInit Me
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "Judge Dredd - Bally 1993" & vbNewLine & "by V-Pin Workshop"
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.HandleMechanics = 0
		.Hidden = 0
		On Error Resume Next
		.Run
	If Err Then MsgBox Err.Description
		debug.print Err.Description
		On Error Goto 0
	End With

'Main Timer init
	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

'Nudging & objects turned off when table tilted
	vpmNudge.TiltSwitch = 14
	vpmNudge.Sensitivity = 4
	vpmNudge.TiltObj = Array(LeftSlingShot, RightSlingShot)

'****************************************************************
'		SETUP MACHINE STATE
'****************************************************************
'Close coin door - High Power Mode On
	Controller.Switch(22)=1

'VUK (Vertical Up Kicker) Setup for Top and Bottom
	Set bsBotVUK = New cvpmBallStack
	With bsBotVUK
		.InitSaucer sw73, 73, 0, 100			'Kicker Name, Switch Activated, Direction Kicked in Degrees, Force of KickOut
		.KickZ=1.2								'Vertical Angle of Kickout **in radians** (NOT degrees), 1.2 = 68 degrees
    End With

	Set bsTopVUK = New cvpmBallStack
	With bsTopVUK
		.InitSaucer sw74, 74, 280, 50
		.KickZ=1.2
    End With

	Set activeShadowBall = new cvpmDictionary

'Reactor Lane Captive Ball Creation
	CapBall1.createball				'Creates a ball in reactor lane at bottom when table loaded.
	CapBall1.kick 180,1				'Kicks out the ball at 180 degrees at 1 power, just enough to get it on the playfield.
	CapBall2.createball				'Middle Ball
	CapBall2.kick 180,1
	CapBall3.createball				'Top Ball
	CapBall3.kick 180,1

'Ball Trough Wall Setup (Under Apron)
	W81.isdropped = 1				'Drops the walls below apron when table loaded.
	W82.isdropped = 1
	W83.isdropped = 1
	W84.isdropped = 1
	W85.isdropped = 1
	W86.isdropped = 1

'Impulse Plunger Setup
	Sol8.Pullback					'Pulls back main plunger ready to fire when activated
	Sol9.Pullback					'Kickback / Missile Plunger

'Backglass Setup
	SetBackGlass

'Crane Mod Setup

	SetCraneMod

End Sub

'****************************************************************
'		SET UP BACKGLASS
'****************************************************************
'This enables objects (flashers) in the "BackGlass" collection to be set up "flat" in the editor
'It essentially moves them all together upright so they're in the right place.
'The makes is very easy to position them using a template as seen on layer 10.

Sub SetBackglass()
	Dim obj
	For Each obj In BackGlass
		obj.x = obj.x
		obj.height = - obj.y + 475
		obj.y = -60
	Next
End Sub

'****************************************************************
'		CONTROLS
'****************************************************************

Sub Table1_Paused:Controller.Pause=1:End Sub
Sub Table1_unPaused:Controller.Pause=0:End Sub
Sub Table1_exit:SaveLUT:Controller.Stop:End Sub

Sub Table1_KeyDown(ByVal KeyCode)							'Sub for What happens when you push a button down
	If keycode = LeftFlipperKey Then
		PinCab_ButtonL.TransX = PinCab_ButtonL.TransX -10	'Moves Left Flipper Cabinet Button
		FlipperActivate LeftFlipper, LFPress
		If CoindoorIsOpen = 1 Then							'AXS Coindoor
			Playsound "ball_bounce2"
			ManualPageNumber = ManualPageNumber - 1
			if ManualPageNumber = 0 then ManualPageNumber = 47   ' Set your instruction manual page number here.
			VR_Instructions.Image = "Manual " & ManualPageNumber
		End If
	End If
	If keycode = RightFlipperKey Then
		PinCab_ButtonR.TransX = PinCab_ButtonR.TransX +10	'Moves Right Flipper Cabinet Button
		FlipperActivate RightFlipper, RFPress
		If CoindoorIsOpen = 1 Then 							'AXS Coindoor
			Playsound "ball_bounce2"
			ManualPageNumber = ManualPageNumber + 1
			if ManualPageNumber = 48 then ManualPageNumber = 1  ' Set your instruction manual page number +1 here.
			VR_Instructions.Image = "Manual " & ManualPageNumber
		End If
	End If
	If keycode = StartGameKey then							'Start Game
		PinCab_Start.Y = PinCab_Start.Y -3					'Moves Start Game Cabinet Button
		soundStartButton()
		DisableLUTSelector = 1
	End If
	If KeyCode = 3 Then										'Extra ball (Buy-In)
		PinCab_EB.Y = PinCab_EB.Y -3						'Moves Extra Ball Cabinet Button
		Controller.Switch(31)=1
	End If
	If KeyCode = PlungerKey Then Controller.Switch(12)=1	'Right Fire Button / Launch Ball
 	If Keycode = LeftMagnaSave Then							'Left Fire Button
		PinCab_MagL.TransX = PinCab_MagL.TransX -10
		Controller.Switch(11)=1
		if DisableLUTSelector = 0 then
            LUTSet = LUTSet  + 1
			if LutSet > 15 then LUTSet = 0
			lutsetsounddir = 1
			If LutToggleSound then
				If lutsetsounddir = 1 And LutSet <> 15 Then
					Playsound "click", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, 1
				End If
				If lutsetsounddir = -1 And LutSet <> 15 Then
					Playsound "click", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, -1
				End If
				If LutSet = 15 Then
					Playsound "gun", 0, 1 * VolumeDial, 0, 0.2, 0, 0, 0, -1
				End If
				LutSlctr.enabled = true
			end if
			SetLUT
			ShowLUT
		end if
	End If
 	If Keycode = RightMagnaSave Then						'Super Game
		PinCab_MagR.TransX = PinCab_MagR.TransX +10			'Moves Right Fire Button
		PinCab_Super.Y = PinCab_Super.Y -3					'Moves Super Game Cabinet Button
		Controller.Switch(44)=1
'		if DisableLUTSelector = 0 then
'			LUTSet = LUTSet - 1
'			if LutSet < 0 then LUTSet = 15
'			lutsetsounddir = -1
'			If LutToggleSound then
'				If lutsetsounddir = 1 And LutSet <> 15 Then
'					Playsound "click", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, 1
'				End If
'				If lutsetsounddir = -1 And LutSet <> 15 Then
'					Playsound "click", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, -1
'				End If
'				If LutSet = 15 Then
'					Playsound "scream", 0, 1 * VolumeDial, 0, 0.2, 0, 0, 0, -1
'				End If
'				LutSlctr.enabled = true
'			end if
'			SetLUT
'			ShowLUT
'		end if
	End If

	If keycode = LeftTiltKey Then Nudge 90, 5:SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 5:SoundNudgeRight()
	If keycode = CenterTiltKey Then Nudge 0, 3:SoundNudgeCenter()

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySoundAtLevelStatic ("Coin_In_1"), CoinSoundLevel, Drain
			Case 1: PlaySoundAtLevelStatic ("Coin_In_2"), CoinSoundLevel, Drain
			Case 2: PlaySoundAtLevelStatic ("Coin_In_3"), CoinSoundLevel, Drain
		End Select
	End If

	If keycode = keyCoinDoor then ' use 'keyCoinDoor' instead of '207' for Rom driven tables.
		If DoorReady = true then
			DoorReady = false 
			If DoorMove = -1 then 
				PlaySound "CoindoorOpen":PlaySound "CoindoorOpen":PlaySound "CoindoorOpen":PlaySound "CoindoorOpen":PlaySound "CoindoorOpen":DCO = True:CoindoorIsOpen = 1
			Else 
				PlaySound "page2":MA = True:CoindoorIsOpen = 0':ManualPageNumber = 1
				VR_Instructions.Image = "Manual " & ManualPageNumber
			End If
		End If
	End If

	If keycode = 8 and CoindoorIsOpen = 1 Then CoindoorB1.transy = CoindoorB1.transy + 10: playsound "Coindoor_Button"	
	If keycode = 9 and CoindoorIsOpen = 1 Then CoindoorB2.transy = CoindoorB2.transy + 10: playsound "Coindoor_Button"
	If keycode = 10 and CoindoorIsOpen = 1 Then CoindoorB3.transy = CoindoorB3.transy + 10: playsound "Coindoor_Button"
	If keycode = 11 and CoindoorIsOpen = 1 Then CoindoorB4.transy = CoindoorB4.transy + 10: playsound "Coindoor_Button"

	If Keycode = 5 then'AddCreditKey or keycode = 4 or keycode = 5 or keycode = 6 Then
		If CoindoorIsOpen = 0 Then
			If VRCoinTimer.enabled = False Then
				VR_Coin.visible = True
				VRCointimer.enabled = true
				playsound"AXSCoinL"
			End If
		End If
	End if

	If Keycode = 6 then'AddCreditKey or keycode = 4 or keycode = 5 or keycode = 6 Then
		If CoindoorIsOpen = 0 Then
			If VRCoinTimer2.enabled = False Then
				VR_Coin2.visible = True
				VRCointimer2.enabled = true
				playsound"AXSCoinR"
			End If
		End If
	End if

	If vpmKeyDown(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)								'Sub for What happens when you let go of a button
	If keycode = LeftFlipperKey Then
		PinCab_ButtonL.TransX = PinCab_ButtonL.TransX +10	'Resets Left Flipper Cabinet Button Position
		FlipperDeActivate LeftFlipper, LFPress
	End If
	If keycode = RightFlipperKey Then
		PinCab_ButtonR.TransX = PinCab_ButtonR.TransX -10	'Resets Right Flipper Cabinet Button Position
		FlipperDeActivate RightFlipper, RFPress
	End If
	If keycode = StartGameKey then							'Start Game
		PinCab_Start.Y = PinCab_Start.Y +3					'Resets Start Game Cabinet Button Position
	End If
	If KeyCode = 3 Then										'Extra ball (Buy-In)
		PinCab_EB.Y = PinCab_EB.Y +3						'Resets Extra Ball Game Cabinet Button Position
		Controller.Switch(31)=0
	End If
	If KeyCode = PlungerKey Then Controller.Switch(12)=0	'Release Right Fire Cabinet Button / Launch Ball
 	If Keycode = LeftMagnaSave Then
		PinCab_MagL.TransX = PinCab_MagL.TransX +10
		Controller.Switch(11)=0
	End If
 	If Keycode = RightMagnaSave Then						'Super Game
		PinCab_MagR.TransX = PinCab_MagR.TransX -10			'Resets Right Fire Button Position
		PinCab_Super.Y = PinCab_Super.Y +3					'Resets Super Game Cabinet Button Position
		Controller.Switch(44)=0
	End If

    If keycode = 8 and CoindoorIsOpen = 1 Then CoindoorB1.transy = CoindoorB1.transy - 10: playsound "Coindoor_ButtonOff"
	If keycode = 9 and CoindoorIsOpen = 1 Then CoindoorB2.transy = CoindoorB2.transy - 10: playsound "Coindoor_ButtonOff"	
	If keycode = 10 and CoindoorIsOpen = 1 Then CoindoorB3.transy = CoindoorB3.transy - 10: playsound "Coindoor_ButtonOff"
	If keycode = 11 and CoindoorIsOpen = 1 Then	CoindoorB4.transy = CoindoorB4.transy - 10: playsound "Coindoor_ButtonOff"	

	If vpmKeyUp(keycode) Then Exit Sub
End Sub

'LUT selector timer

dim lutsetsounddir
sub LutSlctr_timer
	If lutsetsounddir = 1 And LutSet <> 15 Then
		Playsound "squeek", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, -1
	End If
	If lutsetsounddir = -1 And LutSet <> 15 Then
		Playsound "squeek", 0, LutToggleSoundLevel * VolumeDial, 0, 0.2, 0, 0, 0, 1
	End If
	If LutSet = 15 Then
		Playsound "scream", 0, 0.1*VolumeDial, 0, 0.2, 0, 0, 0, 1
	End If
	LutSlctr.enabled = False
end sub


'****************************************************************
'		TIMER CODE & PRIMITIVE POSITION TRACKERS
'****************************************************************

Sub FrameTimer_Timer()
'Flipper Trackers
	LeftBat.RotZ = LeftFlipper.CurrentAngle
	LeftBatu.RotZ = LeftFlipper1.CurrentAngle
	batleftshadow.ObjRotZ = LeftFlipper.CurrentAngle
	batuleftshadow.ObjRotZ = LeftFlipper1.CurrentAngle
	RightBat.RotZ = RightFlipper.CurrentAngle
	RightBatU.RotZ = RightFlipper1.CurrentAngle
	batrightshadow.ObjRotZ = RightFlipper.CurrentAngle
	baturightshadow.ObjRotZ = RightFlipper1.CurrentAngle

'Standup Target Trackers
	sw18p_off.rotx = sw18p.rotx
	sw18p_off.roty = sw18p.roty
	sw18ap_off.rotx = sw18ap.rotx
	sw18ap_off.roty = sw18ap.roty
	sw18bp_off.rotx = sw18bp.rotx
	sw18bp_off.roty = sw18bp.roty
	sw25p_off.rotx = sw25p.rotx
	sw25p_off.roty = sw25p.roty
	sw27p_off.rotx = sw27p.rotx
	sw27p_off.roty = sw27p.roty
	sw36p_off.rotx = sw36p.rotx
	sw36p_off.roty = sw36p.roty
	sw68p_off.rotx = sw68p.rotx
	sw68p_off.roty = sw68p.roty

'Drop Target Trackers
	sw54prim_off.transz = sw54prim.transz
	sw54prim_off.rotx = sw54prim.rotx
	sw54prim_off.roty = sw54prim.roty
	sw55prim_off.transz = sw55prim.transz
	sw55prim_off.rotx = sw55prim.rotx
	sw55prim_off.roty = sw55prim.roty
	sw56prim_off.transz = sw56prim.transz
	sw56prim_off.rotx = sw56prim.rotx
	sw56prim_off.roty = sw56prim.roty
	sw57prim_off.transz = sw57prim.transz
	sw57prim_off.rotx = sw57prim.rotx
	sw57prim_off.roty = sw57prim.roty
	sw58prim_off.transz = sw58prim.transz
	sw58prim_off.rotx = sw58prim.rotx
	sw58prim_off.roty = sw58prim.roty

'Other Graphics Trackers
	DiverterP.ObjRotZ = JDDiverter.CurrentAngle -90
	DiverterP_Off.ObjRotZ = JDDiverter.CurrentAngle -90

'Crane Mods -cp
	Crane_Arm.rotx = crane.rotx
	Crane_Arm.roty = crane.roty
	Crane_Arm.rotz = crane.rotz
	Crane_Mod3d_a.rotx = crane.rotx
	Crane_Mod3d_a.roty = crane.roty
	Crane_Mod3d_a.rotz = crane.rotz
	Crane_Mod3d_b.rotx = crane.rotx
	Crane_Mod3d_b.roty = crane.roty
	Crane_Mod3d_b.rotz = crane.rotz
	Crane_Mod3d_LEDs.rotx = crane.rotx
	Crane_Mod3d_LEDs.roty = crane.roty
	Crane_Mod3d_LEDs.rotz = crane.rotz
	Crane_Mod_a.rotx = crane.rotx
	Crane_Mod_a.roty = crane.roty
	Crane_Mod_a.rotz = crane.rotz
	Crane_Mod_b.rotx = crane.rotx
	Crane_Mod_b.roty = crane.roty
	Crane_Mod_b.rotz = crane.rotz
	Crane_Mod_LEDs.rotx = crane.rotx
	Crane_Mod_LEDs.roty = crane.roty
	Crane_Mod_LEDs.rotz = crane.rotz

	dw_screws_off.rotz = dw_screws.rotz

	Ramp_Wire_Off.BlendDisableLighting = Ramp_Wire.BlendDisableLighting

	PinCab_MagLC.TransX = PinCab_MagL.TransX
	PinCab_MagRC.TransX = PinCab_MagR.TransX

	LampTimer
	Lampztimer
	RollingUpdate
	'BallShadowUpdate
End Sub

'****************************************************************
'		FLIPPER CONTROL
'****************************************************************

Const ReflipAngle = 20

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

Sub SolURFlipper(Enabled)
	If Enabled Then
		RightFlipper1.RotateToEnd
		If rightflipper1.currentangle > rightflipper1.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper1
		Else
			SoundFlipperUpAttackRight RightFlipper1
			RandomSoundFlipperUpRight RightFlipper1
		End If
	Else
		RightFlipper1.RotateToStart
		RightFlipper1.RotateToStart
		If RightFlipper1.currentangle > RightFlipper1.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper1
		End If
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolULFlipper(Enabled)
	If Enabled Then
		LeftFlipper1.RotateToEnd
		If leftflipper1.currentangle < leftflipper1.endangle + ReflipAngle Then
			RandomSoundReflipUpLeft LeftFlipper1
		Else
			SoundFlipperUpAttackLeft LeftFlipper1
			RandomSoundFlipperUpLeft LeftFlipper1
		End If
	Else
		LeftFlipper1.RotateToStart
		If LeftFlipper1.currentangle < LeftFlipper1.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper1
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

'***********************************************************************
' Begin NFozzy Physics Scripting:  Flipper Tricks and Rubber Dampening '
'***********************************************************************

'****************************************************************
'	Flipper Collision Subs
'****************************************************************

Sub LeftFlipper_Collide(parm)
	FlipperLeftHitParm = parm/10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	if RubberizerEnabled = 1 then Rubberizer(parm)
	if RubberizerEnabled = 2 then Rubberizer2(parm)
End Sub

Sub RightFlipper_Collide(parm)
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	if RubberizerEnabled = 1 then Rubberizer(parm)
	if RubberizerEnabled = 2 then Rubberizer2(parm)
End Sub

Sub LeftFlipper1_Collide(parm)
	FlipperLeftHitParm = parm/10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
	if RubberizerEnabled = 1 then Rubberizer(parm)
	if RubberizerEnabled = 2 then Rubberizer2(parm)
End Sub

Sub RightFlipper1_Collide(parm)
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
	CheckLiveCatch Activeball, RightFlipper1, RFCount1, parm
	if RubberizerEnabled = 1 then Rubberizer(parm)
	if RubberizerEnabled = 2 then Rubberizer2(parm)
End Sub

'****************************************************************
'	iaakki's Rubberizer
'****************************************************************

sub Rubberizer(parm)
	if parm < 10 And parm > 2 And Abs(activeball.angmomz) < 10 then
		'debug.print "parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = activeball.angmomz * 1.2
		activeball.vely = activeball.vely * 1.2
		'debug.print ">> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	Elseif parm <= 2 and parm > 0.2 and activeball.vely < 0 Then
		'debug.print "* parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = activeball.angmomz * -1.1
		activeball.vely = activeball.vely * 1.4
		'debug.print "**** >> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	end if
end sub

sub Rubberizer2(parm)
	if parm < 10 And parm > 2 And Abs(activeball.angmomz) < 10 then
		'debug.print "parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = -activeball.angmomz * 2
		activeball.vely = activeball.vely * 1.2
		'debug.print ">> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	Elseif parm <= 2 and parm > 0.2 and activeball.vely < 0 Then
		'debug.print "* parm: " & parm & " momz: " & activeball.angmomz &" vely: "& activeball.vely
		activeball.angmomz = -activeball.angmomz * 0.5
		activeball.vely = activeball.vely * (1.2 + rnd(1)/3 )
		'debug.print "**** >> newmomz: " & activeball.angmomz&" newvely: "& activeball.vely
	end if
end sub

'****************************************************************
'	FLIPPER CORRECTION INITIALIZATION
'****************************************************************

Const LiveCatch = 16

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

'****************************************************************
'	FLIPPER CORRECTION FUNCTIONS
'****************************************************************

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
				'playsound "fx_knocker"
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'****************************************************************
'	FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS 
'****************************************************************

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

'****************************************************************
'	FLIPPER TRICKS
'****************************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks LeftFlipper1, LFPress1, LFCount1, LFEndAngle1, LFState1
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperTricks RightFlipper1, RFPress1, RFCount1, RFEndAngle1, RFState1

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
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				end If
			Next
		End If
	Else 
		If Flipper1.currentangle <> EndAngle1 then 
			EOSNudge1 = 0
		end if
	End If
End Sub

'****************************************************************
'	Maths
'****************************************************************
Const PI = 3.1415927

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

'****************************************************************
'	Check ball distance from Flipper for Rem
'****************************************************************

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

'****************************************************************
'	End - Check ball distance from Flipper for Rem
'****************************************************************

dim LFPress, RFPress, LFPress1, RFPress1, LFCount, LFCount1, RFCount, RFCount1
dim LFState, LFState1, RFState, RFState1
dim EOST, EOSA, Frampup, FElasticity, FReturn
dim RFEndAngle, RFEndAngle1, LFEndAngle, LFEndAngle1

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
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.025

LFEndAngle = Leftflipper.endangle
LFEndAngle1 = Leftflipper1.endangle
RFEndAngle = RightFlipper.endangle
RFEndAngle1 = RightFlipper1.endangle

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
	Dim LiveCatchBounce															'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.5 Then						'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)	'Partial catch when catch happens a bit late
		end If
'		debug.print "Live catch! Bounce: " & LiveCatchBounce

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
	End If
End Sub

'****************************************************************
'	PHYSICS DAMPENERS
'****************************************************************
'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR

Sub RDampen_Timer()
	Cor.Update
	CoindoorTimer
End Sub

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
		if cor.ballvel(aBall.id) <> 0 then
			RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
		Else
			RealCOR = BallSpeed(aBall) / 0.0000001
		end if
		coef = desiredcor / realcor
		if debugOn then str = name & " in vel:" & round(cor.ballvel(aBall.id),2 ) & vbnewline & "desired cor: " & round(desiredcor,4) & vbnewline & _
		"actual cor: " & round(realCOR,4) & vbnewline & "ballspeed coef: " & round(coef, 3) & vbnewline 
		if Print then debug.print Round(cor.ballvel(aBall.id),2) & ", " & round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
		'playsound "fx_knocker"
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

'****************************************************************
'	TRACK ALL BALL VELOCITIES FOR RUBBER DAMPENER AND DROP TARGETS
'****************************************************************

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

'****************************************************************
'	iaakki's TargetBouncer for targets and posts
'****************************************************************
Dim zMultiplier

sub TargetBouncer(aBall,defvalue)
	if TargetBouncerEnabled <> 0 and aball.z < 30 then
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

'****************************************************************
'	END nFozzy Physics
'****************************************************************


'****************************************************************
'	Flupper Flasher Domes (Heavily Modified)
'****************************************************************

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness, FlasherSpeed

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.2		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.2		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherBloomIntensity = 0.4		' *** lower this, if the blooms / hazes are too bright (i.e. 0.1)	***
FlasherOffBrightness = 1		' *** brightness of the flasher dome when switched off (range 0-2)	***
FlasherSpeed = 0.85				' *** 0.1 fast, 0.99 slow											***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20)

Set objflasher(1) = Eval("f21bloom")
Set objflasher(2) = Eval("f22bloom")
Set objflasher(3) = Eval("f26bloom")
F21.IntensityScale = 0
F21a.IntensityScale = 0
F22.IntensityScale = 0
F22a.IntensityScale = 0
F26.IntensityScale = 0
f21bloom.visible = 0 : bg_s21.visible = 0
f22bloom.visible = 0 : bg_s22.visible = 0
f26bloom.visible = 0 : bg_s26.visible = 0 : bg_s26a.visible = 0
FlasherPL_LS.visible = 0 : FlasherPL_RS.visible = 0 : FlasherPL_BW.visible = 0
FlasherPR_LS.visible = 0 : FlasherPR_RS.visible = 0 : FlasherPR_BW.visible = 0 : FlasherPR_T.visible = 0

'OnPrimSwap(aTarget) '1 = LPursuit gion, 2 = RPursuit gion, 3 = LPursuit gioff, 4 = RPursuit gioff, 5 = gioff, 6 = gion

		'off prims are at on images; on prims are transparent
		'*on prims swap to Off images -> OnPrimSwap 5
		'fade up on prims -> timer
		'*off prims swap to Off images
		'set on prims fully transparent

Dim ramplight

Sub FlashFlasher21(nr)
	If not objflasher(nr).TimerEnabled Then
		objflasher(nr).TimerEnabled = True 
		f21bloom.visible = 1 : bg_s21.visible = 1 : PLAYFIELD_PL.visible = 1
		FlasherPL_LS.visible = 1 : FlasherPL_RS.visible = 1 : FlasherPL_BW.visible = 1
	End If

	F21.IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	F21a.IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	f21bloom.opacity = 100 *  FlasherBloomIntensity * ObjLevel(nr)^2.5
	bg_s21.opacity = 500 * ObjLevel(nr)^2.5
	Pursuit_LLights.BlendDisableLighting = FlasherOffBrightness + 100 * ObjLevel(nr)^1
	Pursuit_LLights1.BlendDisableLighting = FlasherOffBrightness + 100 * ObjLevel(nr)^1
	PLAYFIELD_PL.opacity = 300 *  FlasherBloomIntensity * ObjLevel(nr)^1.5

	DW_Disc.BlendDisableLighting = FlasherOffBrightness + 1 * max( max(objlevel(1), objlevel(2)), objlevel(3))^3
	Ramp_Wire.BlendDisableLighting = FlasherOffBrightness + 1 * max( max(objlevel(1), objlevel(2)), objlevel(3))^3
	towers_p.BlendDisableLighting = 75 * FlasherOffBrightness * max(objlevel(1), objlevel(2))^2
	ramp_ent_left_flash.BlendDisableLighting = 50 * FlasherOffBrightness * ObjLevel(nr)^1

	FlasherPL_LS.opacity = 300 * FlasherBloomIntensity * ObjLevel(nr)^1
	FlasherPL_RS.opacity = 300 * FlasherBloomIntensity * ObjLevel(nr)^2
	FlasherPL_BW.opacity = 300 * FlasherBloomIntensity * ObjLevel(nr)^1.5

	ObjLevel(nr) = ObjLevel(nr) * FlasherSpeed - 0.01

	If ObjLevel(nr) < 0 Then 
		objflasher(nr).TimerEnabled = False
		f21bloom.visible = 0 : bg_s21.visible = 0 : PLAYFIELD_PL.visible = 0
		FlasherPL_LS.visible = 0 : FlasherPL_RS.visible = 0 : FlasherPL_BW.visible = 0
	End If
End Sub

Sub FlashFlasher22(nr)
	If not objflasher(nr).TimerEnabled Then 
		objflasher(nr).TimerEnabled = True
		f22bloom.visible = 1 : bg_s22.visible = 1 : PLAYFIELD_PR.visible = 1
		FlasherPR_LS.visible = 1 : FlasherPR_RS.visible = 1 : FlasherPR_BW.visible = 1 : FlasherPR_T.visible = 1
	End If

	F22.IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	F22a.IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	f22bloom.opacity = 100 *  FlasherBloomIntensity * ObjLevel(nr)^2.5
	bg_s22.opacity = 500 * ObjLevel(nr)^2.5
	Pursuit_RLights.BlendDisableLighting = FlasherOffBrightness + 100 * ObjLevel(nr)^1
	Pursuit_RLights1.BlendDisableLighting = FlasherOffBrightness + 100 * ObjLevel(nr)^1
	PLAYFIELD_PR.opacity = 300 *  FlasherBloomIntensity * ObjLevel(nr)^1.5

	DW_Disc.BlendDisableLighting = FlasherOffBrightness + 1 * max( max(objlevel(1), objlevel(2)), objlevel(3))^3
	Ramp_Wire.BlendDisableLighting = FlasherOffBrightness + 1 * max( max(objlevel(1), objlevel(2)), objlevel(3))^3
	towers_p.BlendDisableLighting = 100 * FlasherOffBrightness * max(objlevel(1), objlevel(2))^2
	ramp_ent_right_flash.BlendDisableLighting = 50 * FlasherOffBrightness * ObjLevel(nr)^1

	FlasherPR_LS.opacity = 300 * FlasherBloomIntensity * ObjLevel(nr)^2
	FlasherPR_RS.opacity = 300 * FlasherBloomIntensity * ObjLevel(nr)^1
	FlasherPR_BW.opacity = 300 * FlasherBloomIntensity * ObjLevel(nr)^1.5
	FlasherPR_T.opacity = 3000 * FlasherBloomIntensity * ObjLevel(nr)^1.5

	ObjLevel(nr) = ObjLevel(nr) * FlasherSpeed - 0.01

	If ObjLevel(nr) < 0 Then 
		objflasher(nr).TimerEnabled = False
		f22bloom.visible = 0 : bg_s22.visible = 0 : PLAYFIELD_PR.visible = 0
		FlasherPR_LS.visible = 0 : FlasherPR_RS.visible = 0 : FlasherPR_BW.visible = 0 : FlasherPR_T.visible = 0
	End If
End Sub

Dim kkc
Sub FlashFlasher26(nr)
	If not objflasher(nr).TimerEnabled Then
		objflasher(nr).TimerEnabled = True
		f26bloom.visible = 1 : bg_s26.visible = 1 : bg_s26a.visible = 1
	End If

	F26.IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^2.5
	f26bloom.opacity = 100 * FlasherBloomIntensity * ObjLevel(nr)^2.5
	DW_In_F26.BlendDisableLighting = FlasherOffBrightness + 300 * ObjLevel(nr)^2
	DW_Globe.BlendDisableLighting = FlasherOffBrightness + 75 * ObjLevel(nr)^2
	DW_Disc.BlendDisableLighting = FlasherOffBrightness + 1 * max( max(objlevel(1), objlevel(2)), objlevel(3))^2
	Ramp_Wire.BlendDisableLighting = FlasherOffBrightness + 1 * max( max(objlevel(1), objlevel(2)), objlevel(3))^2

	For each kkc in Cranes:kkc.blenddisablelighting = Ramp_Wire.blenddisablelighting:Next

	bg_s26.opacity = 500 * ObjLevel(nr)^2.5
	bg_s26a.opacity = 500 * ObjLevel(nr)^2.5

	ObjLevel(nr) = ObjLevel(nr) * FlasherSpeed - 0.01

	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : f26bloom.visible = 0 : bg_s26.visible = 0 : bg_s26a.visible = 0 : End If
End Sub

Sub f21bloom_Timer() : FlashFlasher21(1) : End Sub 
Sub f22bloom_Timer() : FlashFlasher22(2) : End Sub 
Sub f26bloom_Timer() : FlashFlasher26(3) : End Sub 

'****************************************************************
'	Flasher Timers
'****************************************************************

Sub FlashSol21(flstate)
	If Flstate Then
		Objlevel(1) = 1 : f21bloom_Timer
	End If
End Sub

Sub FlashSol22(flstate)
	If Flstate Then
		Objlevel(2) = 1 : f22bloom_Timer
	End If
End Sub

Sub FlashSol26(flstate)
	If Flstate Then
		Objlevel(3) = 1 : f26bloom_Timer
	End If
End Sub

'****************************************************************
'	Begin nFozzy lamp handling
'****************************************************************

Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
Dim ModLampz : Set ModLampz = New DynamicLamps
InitLampsNF              ' Setup lamp assignments

Sub LampTimer()
	dim x, chglamp
	chglamp = Controller.ChangedLamps
	If Not IsEmpty(chglamp) Then
		For x = 0 To UBound(chglamp) 			'nmbr = chglamp(x, 0), state = chglamp(x, 1)
			Lampz.state(chglamp(x, 0)) = chglamp(x, 1)
		next
	End If
	Lampz.Update1	'update (fading logic only)
	ModLampz.Update1

	'if lampz.lvl(81) = 0 then decal_so.visible = 0 else decal_so.visible = 1
	'if lampz.lvl(82) = 0 then decal_bo.visible = 0 else decal_bo.visible = 1

End Sub

dim FrameTime, InitFrameTime : InitFrameTime = 0
Sub Lampztimer()
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime	'Count frametime. Unused atm?
	Lampz.Update 'updates on frametime (Object updates only)
	ModLampz.Update
End Sub

Function FlashLevelToIndex(Input, MaxSize)
	FlashLevelToIndex = cInt(MaxSize * Input)
End Function

'Material swap arrays.
Dim TextureArray1: TextureArray1 = Array("Plastic with an image trans", "Plastic with an image trans","Plastic with an image trans","Plastic with an image")
Dim DLintensity

'****************************************************************
'	Prim *Image* Swaps
'****************************************************************
Sub ImageSwap(pri, group, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 3)
		Case 1:pri.Image = group(0) 'Full
		Case 2:pri.Image = group(1) 'Fading...
		Case 3:pri.Image = group(2) 'Fading...
        Case 4:pri.Image = group(3) 'Off
    End Select
pri.blenddisablelighting = aLvl * DLintensity
End Sub

'****************************************************************
'	Prim *Material* Swaps
'****************************************************************
Sub MatSwap(pri, group, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 3)
		Case 1:pri.Material = group(0) 'Full
		Case 2:pri.Material = group(1) 'Fading...
		Case 3:pri.Material = group(2) 'Fading...
        Case 4:pri.Material = group(3) 'Off
    End Select
	pri.blenddisablelighting = aLvl * DLintensity
End Sub

Sub FadeMaterialToys(pri, group, ByVal aLvl)	'cp's script
'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
    Select case FlashLevelToIndex(aLvl, 3)
		Case 0:pri.Material = group(0) 'Off
		Case 1:pri.Material = group(1) 'Fading...
		Case 2:pri.Material = group(2) 'Fading...
        Case 3:pri.Material = group(3) 'Full
    End Select
	'if tb.text <> pri.image then tb.text = pri.image : debug.print pri.image end If	'debug
	pri.blenddisablelighting = aLvl * 1 'Intensity Adjustment
End Sub

Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl^1.5 * DLintensity * 0.4
End Sub

sub DecalFlash(aMat, Byval aLvl)
	'Decal_flash1, Decal_flash2
	'debug.print aLvl

	if aMat = 1 then
		UpdateMaterial "Decal_flash1",0,0,0,0,0,0,aLvl*0.9,RGB(255,255,255),0,0,False,True,0,0,0,0
	elseif aMat = 2 then
		UpdateMaterial "Decal_flash2",0,0,0,0,0,0,aLvl*0.9,RGB(255,255,255),0,0,False,True,0,0,0,0
	end if
end sub

Sub DisableLightingBulb(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl^0.7 * DLintensity * 0.4
End Sub

Sub DisableLightingColor(pri, DLintensity, lcolor, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	Select case lcolor
		case 1:pri.material = "InsertWhiteOnBulb"
		case 2:pri.material = "InsertRedOnBulb"
		case 3:pri.material = "InsertYellowOnBulb"
		case 4:pri.material = "InsertGreenOnBulb"
	end select
	pri.blenddisablelighting = aLvl * DLintensity * 0.4
End Sub
 
Sub InitLampsNF()
	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating
	ModLampz.Filter = "LampFilter"

	'Adjust fading speeds (1 / full MS fading time)
	dim x
	for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/4 : Lampz.FadeSpeedDown(x) = 1/20 : next
	'for x = 11 to 14 : Lampz.FadeSpeedUp(x) = 1/5 : Lampz.FadeSpeedDown(x) = 1/40 : next
	for x = 0 to 5 : ModLampz.FadeSpeedUp(x) = 1/5 : ModLampz.FadeSpeedDown(x) = 1/40 : Next
	for x = 6 to 28 : ModLampz.FadeSpeedUp(x) = 1/10 : ModLampz.FadeSpeedDown(x) = 1/40 : Next

	'for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/80 : Lampz.FadeSpeedDown(x) = 1/100 : next
	'Lampz.FadeSpeedUp(110) = 1/64 'GI

	'Lamp Assignments
	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays

'Crime Scene Multicolour Inserts
	'11 blue -> 1
	'12 red --> 2
	'13 yellow -> 3
	'14 green -> 4
	Lampz.MassAssign(11)=L11
	Lampz.MassAssign(11)=L11g
	Lampz.MassAssign(11)=L11f
	Lampz.Callback(11) = "DisableLightingColor p11, 60, 1,"
	Lampz.Callback(11) = "DisableLighting p11bulb, 340,"
	Lampz.MassAssign(12)=L12
	Lampz.MassAssign(12)=L12g
	Lampz.MassAssign(12)=L12f
	Lampz.Callback(12) = "DisableLightingColor p12, 60, 2,"
	Lampz.Callback(12) = "DisableLighting p12bulb, 340,"
	Lampz.MassAssign(13)=L13
	Lampz.MassAssign(13)=L13g
	Lampz.MassAssign(13)=L13f
	Lampz.Callback(13) = "DisableLightingColor p13, 60, 3,"
	Lampz.Callback(13) = "DisableLighting p13bulb, 340,"
	Lampz.MassAssign(14)=L14
	Lampz.MassAssign(14)=l14g
	Lampz.MassAssign(14)=l14f
	Lampz.Callback(14) = "DisableLightingColor p14, 60, 4,"
	Lampz.Callback(14) = "DisableLighting p14bulb, 340,"

	Lampz.MassAssign(15)=L15
	Lampz.MassAssign(15)=l15g
	Lampz.MassAssign(15)=l15f
	Lampz.Callback(15) = "DisableLightingColor p15, 60, 1,"
	Lampz.Callback(15) = "DisableLighting p15bulb, 340,"
	Lampz.MassAssign(16)=L16
	Lampz.MassAssign(16)=l16g
	Lampz.MassAssign(16)=l16f
	Lampz.Callback(16) = "DisableLightingColor p16, 60, 2,"
	Lampz.Callback(16) = "DisableLighting p16bulb, 340,"
	Lampz.MassAssign(17)=L17
	Lampz.MassAssign(17)=l17g
	Lampz.MassAssign(17)=l17ff
	Lampz.Callback(17) = "DisableLightingColor p17, 60, 3,"
	Lampz.Callback(17) = "DisableLighting p17bulb, 340,"
	Lampz.MassAssign(18)=L18
	Lampz.MassAssign(18)=l18g
	Lampz.MassAssign(18)=l18ff
	Lampz.Callback(18) = "DisableLightingColor p18, 60, 4,"
	Lampz.Callback(18) = "DisableLighting p18bulb, 340,"

	Lampz.MassAssign(21)=L21
	Lampz.MassAssign(21)=L21g
	Lampz.MassAssign(21)=L21f
	Lampz.Callback(21) = "DisableLightingColor p21, 60, 1,"
	Lampz.Callback(21) = "DisableLighting p21bulb, 340,"
	Lampz.MassAssign(22)=L22
	Lampz.MassAssign(22)=L22g
	Lampz.MassAssign(22)=L22f
	Lampz.Callback(22) = "DisableLightingColor p22, 60, 2,"
	Lampz.Callback(22) = "DisableLighting p22bulb, 340,"
	Lampz.MassAssign(23)=L23
	Lampz.MassAssign(23)=L23g
	Lampz.MassAssign(23)=L23f
	Lampz.Callback(23) = "DisableLightingColor p23, 60, 3,"
	Lampz.Callback(23) = "DisableLighting p23bulb, 340,"
	Lampz.MassAssign(24)=L24
	Lampz.MassAssign(24)=l24g
	Lampz.MassAssign(24)=l24f
	Lampz.Callback(24) = "DisableLightingColor p24, 60, 4,"
	Lampz.Callback(24) = "DisableLighting p24bulb, 340,"

	Lampz.MassAssign(25)=L25
	Lampz.MassAssign(25)=l25g
	Lampz.MassAssign(25)=l25f
	Lampz.Callback(25) = "DisableLightingColor p25, 60, 1,"
	Lampz.Callback(25) = "DisableLighting p25bulb, 340,"
	Lampz.MassAssign(26)=L26
	Lampz.MassAssign(26)=l26g
	Lampz.MassAssign(26)=l26f
	Lampz.Callback(26) = "DisableLightingColor p26, 60, 2,"
	Lampz.Callback(26) = "DisableLighting p26bulb, 340,"
	Lampz.MassAssign(27)=L27
	Lampz.MassAssign(27)=l27g
	Lampz.MassAssign(27)=l27f
	Lampz.Callback(27) = "DisableLightingColor p27, 60, 3,"
	Lampz.Callback(27) = "DisableLighting p27bulb, 340,"
	Lampz.MassAssign(28)=L28
	Lampz.MassAssign(28)=l28g
	Lampz.MassAssign(28)=l28f
	Lampz.Callback(28) = "DisableLightingColor p28, 60, 4,"
	Lampz.Callback(28) = "DisableLighting p28bulb, 340,"

	Lampz.MassAssign(31)=L31
	Lampz.MassAssign(31)=l31g
	Lampz.MassAssign(31)=l31f
	Lampz.Callback(31) = "DisableLightingColor p31, 60, 1,"
	Lampz.Callback(31) = "DisableLighting p31bulb, 340,"
	Lampz.MassAssign(32)=L32
	Lampz.MassAssign(32)=l32g
	Lampz.MassAssign(32)=l32f
	Lampz.Callback(32) = "DisableLightingColor p32, 60, 2,"
	Lampz.Callback(32) = "DisableLighting p32bulb, 340,"
	Lampz.MassAssign(33)=L33
	Lampz.MassAssign(33)=l33g
	Lampz.MassAssign(33)=l33f
	Lampz.Callback(33) = "DisableLightingColor p33, 60, 3,"
	Lampz.Callback(33) = "DisableLighting p33bulb, 340,"
	Lampz.MassAssign(34)=L34
	Lampz.MassAssign(34)=l34g
	Lampz.MassAssign(34)=l34f
	Lampz.Callback(34) = "DisableLightingColor p34, 60, 4,"
	Lampz.Callback(34) = "DisableLighting p34bulb, 340,"

'Lock Lights - Cooling Towers can be hooked up to them.
	Lampz.MassAssign(35)=L35
	Lampz.MassAssign(35)=L35g
	Lampz.Callback(35) = "DisableLighting p35, 40,"
	Lampz.Callback(35) = "DisableLightingBulb p35bulb, 30,"
'	Lampz.Callback(35) = "DisableLighting Tower1_Lid, 0.6,"
'	Lampz.Callback(35) = "DisableLighting Tower1_Base, 0.1,"
'	Lampz.MassAssign(35)=L35a									'Cooling Tower Lock
	Lampz.MassAssign(36)=L36
	Lampz.MassAssign(36)=l36g
	Lampz.Callback(36) = "DisableLighting p36, 40,"
	Lampz.Callback(36) = "DisableLightingBulb p36bulb, 30,"
'	Lampz.Callback(36) = "DisableLighting Tower2_Lid, 0.6,"
'	Lampz.Callback(36) = "DisableLighting Tower2_Base, 0.1,"
'	Lampz.MassAssign(36)=L36a									'Cooling Tower Lock
	Lampz.MassAssign(37)=L37
	Lampz.MassAssign(37)=L37g
	Lampz.Callback(37) = "DisableLighting p37, 40,"
	Lampz.Callback(37) = "DisableLightingBulb p37bulb, 30,"
'	Lampz.Callback(37) = "DisableLighting Tower3_Lid, 0.6,"
'	Lampz.Callback(37) = "DisableLighting Tower3_Base, 0.1,"
'	Lampz.MassAssign(37)=L37a									'Cooling Tower Lock

	Lampz.Callback(38) = "DisableLighting PinCab_EB, 2,"		'Extra Ball / Buy In Button

	Lampz.MassAssign(41)=L41
    Lampz.MassAssign(41)=L41g
	Lampz.MassAssign(41)=L41f
    Lampz.Callback(41) = "DisableLighting p41, 100,"
    Lampz.Callback(41) = "DisableLightingBulb p41bulb, 80,"
    Lampz.MassAssign(42)=L42
    Lampz.MassAssign(42)=L42g
	Lampz.MassAssign(42)=L42f
    Lampz.Callback(42) = "DisableLighting p42, 100,"
    Lampz.Callback(42) = "DisableLightingBulb p42bulb, 80,"
    Lampz.MassAssign(43)=L43
    Lampz.MassAssign(43)=L43g
	Lampz.MassAssign(43)=L43f
    Lampz.Callback(43) = "DisableLighting p43, 100,"
    Lampz.Callback(43) = "DisableLightingBulb p43bulb, 80,"
    Lampz.MassAssign(44)=L44
    Lampz.MassAssign(44)=L44g
	Lampz.MassAssign(44)=L44f
    Lampz.Callback(44) = "DisableLighting p44, 100,"
    Lampz.Callback(44) = "DisableLightingBulb p44bulb, 80,"

	Lampz.MassAssign(45)=L45
	Lampz.MassAssign(45)=L45g
	Lampz.Callback(45) = "DisableLighting p45, 40,"
	Lampz.Callback(45) = "DisableLightingBulb p45bulb, 30,"
	Lampz.MassAssign(46)=L46
	Lampz.MassAssign(46)=L46g
	Lampz.Callback(46) = "DisableLighting p46, 40,"
	Lampz.Callback(46) = "DisableLightingBulb p46bulb, 30,"
	Lampz.MassAssign(47)=L47
	Lampz.MassAssign(47)=L47g
	Lampz.Callback(47) = "DisableLighting p47, 40,"
	Lampz.Callback(47) = "DisableLightingBulb p47bulb, 30,"
	Lampz.MassAssign(48)=L48
	Lampz.MassAssign(48)=l48r
	Lampz.MassAssign(48)=L48g
	Lampz.Callback(48) = "DisableLighting p48, 30,"
	Lampz.Callback(48) = "DisableLightingBulb p48bulb, 50,"

	Lampz.MassAssign(51)=L51
	Lampz.MassAssign(51)=l51g
	Lampz.Callback(51) = "DisableLighting p51, 40,"
	Lampz.Callback(51) = "DisableLightingBulb p51bulb, 30,"
	Lampz.MassAssign(52)=L52
	Lampz.MassAssign(52)=l52g
	Lampz.Callback(52) = "DisableLighting p52, 40,"
	Lampz.Callback(52) = "DisableLightingBulb p52bulb, 30,"
	Lampz.MassAssign(53)=L53
	Lampz.MassAssign(53)=l53g
	Lampz.Callback(53) = "DisableLighting p53, 40,"
	Lampz.Callback(53) = "DisableLightingBulb p53bulb, 30,"
	Lampz.MassAssign(54)=L54
	Lampz.MassAssign(54)=l54g
	Lampz.Callback(54) = "DisableLighting p54, 40,"
	Lampz.Callback(54) = "DisableLightingBulb p54bulb, 30,"
	Lampz.MassAssign(55)=L55
	Lampz.MassAssign(55)=l55g
	Lampz.Callback(55) = "DisableLighting p55, 40,"
	Lampz.Callback(55) = "DisableLightingBulb p55bulb, 30,"
	Lampz.MassAssign(56)=L56
	Lampz.MassAssign(56)=l56g
	Lampz.Callback(56) = "DisableLighting p56, 40,"
	Lampz.Callback(56) = "DisableLightingBulb p56bulb, 30,"
	Lampz.MassAssign(57)=L57
	Lampz.MassAssign(57)=l57g
	Lampz.Callback(57) = "DisableLighting p57, 40,"
	Lampz.Callback(57) = "DisableLightingBulb p57bulb, 30,"

	Lampz.MassAssign(58)=L58	'Cursed Earth / Subway Entrance Lamp
	Lampz.MassAssign(58)=L58a	'Cursed Earth / Subway Entrance Lamp

	Lampz.MassAssign(61)=L61
	Lampz.MassAssign(61)=L61g
	Lampz.MassAssign(61)=l61r
	Lampz.Callback(61) = "DisableLighting p61, 120,"
	Lampz.Callback(61) = "DisableLightingBulb p61bulb, 60,"
	Lampz.MassAssign(61)=L61a
	Lampz.MassAssign(61)=l61ag
	Lampz.MassAssign(61)=l61ar
	Lampz.Callback(61) = "DisableLighting p61a, 120,"
	Lampz.Callback(61) = "DisableLightingBulb p61abulb, 60,"

	Lampz.MassAssign(62)=L62
	Lampz.MassAssign(62)=L62g
	Lampz.Callback(62) = "DisableLighting p62, 40,"
	Lampz.Callback(62) = "DisableLightingBulb p62bulb, 50,"
	Lampz.MassAssign(63)=L63
	Lampz.MassAssign(63)=L63g
	Lampz.Callback(63) = "DisableLighting p63, 33,"
	Lampz.MassAssign(64)=L64
	Lampz.MassAssign(64)=L64g
	Lampz.MassAssign(64)=L64f
	Lampz.Callback(64) = "DisableLighting p64, 130,"
	Lampz.Callback(64) = "DisableLightingBulb p64bulb, 70,"
	Lampz.MassAssign(65)=L65
	Lampz.MassAssign(65)=l65ag
	Lampz.MassAssign(65)=L65f
	Lampz.Callback(65) = "DisableLighting p65, 110,"
	Lampz.Callback(65) = "DisableLightingBulb p65bulb, 60,"
	Lampz.MassAssign(66)=L66
	Lampz.MassAssign(66)=l66g
	Lampz.Callback(66) = "DisableLighting p66, 40,"
	Lampz.Callback(66) = "DisableLightingBulb p66bulb, 50,"
	Lampz.MassAssign(67)=L67
	Lampz.MassAssign(67)=L67g
	Lampz.Callback(67) = "DisableLighting p67, 33,"
	Lampz.MassAssign(68)=L68
	Lampz.MassAssign(68)=l68r
	Lampz.MassAssign(68)=L68g
	Lampz.Callback(68) = "DisableLighting p68, 120,"

	Lampz.MassAssign(71)=L71										'J
	Lampz.MassAssign(71)=L71r
	Lampz.MassAssign(71)=L71g
	Lampz.Callback(71) = "DisableLighting p71, 110,"
	Lampz.Callback(71) = "DisableLightingBulb p71bulb, 60,"
	Lampz.MassAssign(72)=L72										'U
	Lampz.MassAssign(72)=L72r
	Lampz.MassAssign(72)=L72g
	Lampz.Callback(72) = "DisableLighting p72, 110,"
	Lampz.Callback(72) = "DisableLightingBulb p72bulb, 60,"
	Lampz.MassAssign(73)=L73										'D
	Lampz.MassAssign(73)=L73r
	Lampz.MassAssign(73)=L73g
	Lampz.Callback(73) = "DisableLighting p73, 110,"
	Lampz.Callback(73) = "DisableLightingBulb p73bulb, 60,"
	Lampz.MassAssign(74)=L74										'G
	Lampz.MassAssign(74)=L74r
	Lampz.MassAssign(74)=L74g
	Lampz.Callback(74) = "DisableLighting p74, 110,"
	Lampz.Callback(74) = "DisableLightingBulb p74bulb, 60,"
	Lampz.MassAssign(75)=L75										'E
	Lampz.MassAssign(75)=L75r
	Lampz.MassAssign(75)=L75g
	Lampz.Callback(75) = "DisableLighting p75, 110,"
	Lampz.Callback(75) = "DisableLightingBulb p75bulb, 60,"

	Lampz.MassAssign(76)=L76
	Lampz.MassAssign(76)=L76g
	Lampz.Callback(76) = "DisableLighting p76, 33,"
	Lampz.MassAssign(77)=L77
	Lampz.MassAssign(77)=L77g
	Lampz.Callback(77) = "DisableLighting p77, 120,"

	Lampz.MassAssign(78)=L78
	Lampz.MassAssign(78)=L78g
	Lampz.Callback(78) = "DisableLighting p78, 30,"
	Lampz.Callback(78) = "DisableLightingBulb p78bulb, 50,"

	Lampz.MassAssign(81)=L81
	Lampz.Callback(81) = "DecalFlash 1,"							'Stake Out Decal (Under)
	
	Lampz.MassAssign(82)=L82
	Lampz.Callback(82) = "DecalFlash 2,"							'Black Out Decal (Under)

	Lampz.MassAssign(83)=L83
	Lampz.MassAssign(83)=L83g
	Lampz.Callback(83) = "DisableLighting p83, 14,"					'Drain Guard
	Lampz.Callback(83) = "DisableLightingBulb p83bulb, 70,"			'Drain Guard


	Lampz.MassAssign(84)=L84
	Lampz.MassAssign(84)=L84g
	Lampz.Callback(84) = "DisableLighting p84, 80,"
	Lampz.Callback(84) = "DisableLightingBulb p84bulb, 40,"
	Lampz.MassAssign(85)=L85
	Lampz.MassAssign(85)=L85g
	Lampz.MassAssign(85)=L85f
	Lampz.Callback(85) = "DisableLighting p85, 40,"
	Lampz.Callback(85) = "DisableLightingBulb p85bulb, 30,"
	Lampz.MassAssign(86)=L86
	Lampz.MassAssign(86)=L86g
	Lampz.Callback(86) = "DisableLighting p86, 33,"

	Lampz.Callback(87) = "DisableLighting PinCab_Super, 2,"			'Super Game Button
	Lampz.Callback(88) = "DisableLighting PinCab_Start, 2,"			'Start Button

'mod lamps

	Lampz.MassAssign(117)=F17										'Judge Fear Flasher
	Lampz.MassAssign(117)=f17g
	Lampz.MassAssign(117)=L17f
	Lampz.Callback(117) = "DisableLighting pf17, 120,"
	Lampz.Callback(117) = "DisableLightingBulb pf17bulb, 40,"
	Lampz.MassAssign(117)=bg_s17

	Lampz.MassAssign(118)=F18										'Judge Mortis Flasher
	Lampz.MassAssign(118)=F18g
	Lampz.MassAssign(118)=L18f
	Lampz.Callback(118) = "DisableLighting pf18, 120,"
	Lampz.Callback(118) = "DisableLightingBulb pf18bulb, 80,"
	Lampz.MassAssign(118)=bg_s18

	Lampz.MassAssign(119)=F19										'Judge Death Flasher
	Lampz.MassAssign(119)=F19g
	Lampz.MassAssign(119)=L19f
	Lampz.Callback(119) = "DisableLighting pf19, 120,"
	Lampz.Callback(119) = "DisableLightingBulb pf19bulb, 60,"
	Lampz.MassAssign(119)=bg_s19
	Lampz.MassAssign(119)=bg_s19a

	Lampz.MassAssign(120)=F20										'Judge Fire Flasher
	Lampz.MassAssign(120)=F20g
	Lampz.MassAssign(120)=L20f
	Lampz.Callback(120) = "DisableLighting pf20, 120,"
	Lampz.Callback(120) = "DisableLightingBulb pf20bulb, 60,"
	Lampz.MassAssign(120)=bg_s20

'	Lampz.MassAssign(121)=F21
'	Lampz.MassAssign(121)=F21a
'	Lampz.MassAssign(121)=f21bloom
'	Lampz.Callback(121) = "DisableLighting Pursuit_LLights, 100,"	'Placeholder Left Pursuit Left
'	Lampz.MassAssign(121)=bg_s21
'
'	Lampz.MassAssign(122)=F22
'	Lampz.MassAssign(122)=F22a
'	Lampz.MassAssign(122)=f22bloom
'	Lampz.Callback(122) = "DisableLighting Pursuit_RLights, 100,"	'Placeholder Left Pursuit Right
'	Lampz.MassAssign(122)=bg_s22

	Lampz.MassAssign(123)=F23										'Blackout
	Lampz.MassAssign(123)=F23g
	Lampz.Callback(123) = "DisableLighting pf23, 7,"
	Lampz.Callback(123) = "DisableLightingBulb pf23bulb, 45,"
	Lampz.MassAssign(123)=bg_s23
	Lampz.MassAssign(123)=bg_s23a

	Lampz.MassAssign(124)=F24
	Lampz.MassAssign(124)=F24a
	Lampz.Callback(124) = "DisableLighting DW_Pole, 4,"

	Lampz.MassAssign(125)=F25
	Lampz.Callback(125) = "DisableLighting F25Bulb, 33,"
	Lampz.MassAssign(125)=F25a
	Lampz.Callback(125) = "DisableLighting F25aBulb, 33,"
	Lampz.MassAssign(125)=f25bloom
	Lampz.MassAssign(125)=bg_s25
	Lampz.MassAssign(125)=bg_s25a

'	Lampz.MassAssign(126)=F26
'	Lampz.Callback(126) = "DisableLighting DW_In_F26, 33,"			'Deadworld Flasher Bulb
'	Lampz.MassAssign(126)=f26bloom
'	Lampz.Callback(126) = "DisableLighting DW_Globe, 33,"			'Deadworld Globe
'	Lampz.MassAssign(126)=bg_s26
'	Lampz.MassAssign(126)=bg_s26a

	Lampz.MassAssign(127)=F27
	Lampz.Callback(127) = "DisableLighting F27Bulb, 33,"
	Lampz.MassAssign(127)=F27a
	Lampz.Callback(127) = "DisableLighting F27aBulb, 33,"
	Lampz.MassAssign(127)=bg_s27
	Lampz.MassAssign(127)=f27bloom

	Lampz.MassAssign(127)=bg_s28
	Lampz.MassAssign(127)=bg_s28a
	Lampz.MassAssign(127)=bg_s28b

'****************************************************************
'						GI assignments
'****************************************************************

	'ModLampz.Callback(0) = "GIUpdates"
	'ModLampz.Callback(1) = "GIUpdates"
	ModLampz.Callback(2) = "GIUpdates"
	'ModLampz.Callback(3) = "GIUpdates"
	'ModLampz.Callback(4) = "GIupdates"

	ModLampz.MassAssign(0)= ColToArray(GIstring1)			' wht/vio	Left/Right Middle GI, Backbox Bottom / Coin Door Maybe?
	ModLampz.MassAssign(1)= ColToArray(GIstring2) 			' wht/grn	Top & Under Deadworld GI, Backbox Middle
	ModLampz.MassAssign(2)= ColToArray(GIstring3) 			' wht/yel	Low Left, Slingshots & Upper Left JD Backbox
	ModLampz.MassAssign(3)= ColToArray(GIstring4) 			' wht/org	Deadworld Planet GI
	ModLampz.MassAssign(4)= ColToArray(GIstring5)			' wht/brn	Backbox Top Right, Front Buttons


'Turn off all lamps on startup
	lampz.Init			'This just turns state of any lamps to 1
	ModLampz.Init

'Immediate update to turn on GI, turn off lamps
	lampz.update
	ModLampz.Update

End Sub

sub OnPrimsTransparency(aValue)
	UpdateMaterial "GI_ON_CAB"		,0,0,0,0,0,0,aValue^3,RGB(255,255,255),0,0,False,True,0,0,0,0
	UpdateMaterial "GI_ON_Plastic"	,0,0,0,0,0,0,aValue^2,RGB(255,255,255),0,0,False,True,0,0,0,0
	UpdateMaterial "GI_ON_Metals"	,0,0,0,0,0,0,aValue^1,RGB(255,255,255),0,0,False,True,0,0,0,0
end sub

sub OnPrimsFlashTransparency(aValue)
	UpdateMaterial "GI_ON_CAB"		,0,0,0,0,0,0,aValue^1.5,RGB(255,255,255),0,0,False,True,0,0,0,0
	UpdateMaterial "GI_ON_Plastic"	,0,0,0,0,0,0,aValue^2,RGB(255,255,255),0,0,False,True,0,0,0,0
	UpdateMaterial "GI_ON_Metals"	,0,0,0,0,0,0,aValue^2.5,RGB(255,255,255),0,0,False,True,0,0,0,0
end Sub

dim ii

sub OffPrimSwap(aTarget)'1 = LPursuit gion, 2 = RPursuit gion, 3 = LPursuit gioff, 4 = RPursuit gioff, 5 = gioff, 6 = gion
	Select Case aTarget
		Case 6: 'on
			For each ii in cPlastics_off:   ii.image="JD_Plastics_gi_on":Next
			For each ii in cG01_off:        ii.image="JD_G01_gi_on":Next
			For each ii in cG02_off:        ii.image="JD_G02_gi_on":Next		
			For each ii in cG03_off:        ii.image="JD_G03_gi_on":Next	
			For each ii in cMetals_off:     ii.image="JD_Metals_gi_on":Next
			For each ii in cJDPursuit_off:  ii.image="JD_Pursuit_gi_on":Next
			For each ii in cRampwire_off:   ii.image="JD_Wireramps_gi_on":Next
			For each ii in cDecals_off:     ii.image="JD_Decals_gi_on":Next
		Case 5: 'off
			For each ii in cPlastics_off:   ii.image="JD_Plastics_gi_off":Next
			For each ii in cG01_off:        ii.image="JD_G01_gi_off":Next
			For each ii in cG02_off:        ii.image="JD_G02_gi_off":Next		
			For each ii in cG03_off:        ii.image="JD_G03_gi_off":Next	
			For each ii in cMetals_off:     ii.image="JD_Metals_gi_off":Next
			For each ii in cJDPursuit_off:  ii.image="JD_Pursuit_gi_off":Next
			For each ii in cRampwire_off:   ii.image="JD_Wireramps_gi_off":Next
			For each ii in cDecals_off:     ii.image="JD_Decals_gi_off":Next
	End Select
end sub

sub OnPrimSwap(aTarget) '1 = LPursuit gion, 2 = RPursuit gion, 3 = LPursuit gioff, 4 = RPursuit gioff, 5 = gioff, 6 = gion
	Select Case aTarget
		Case 6: 'on
			For each ii in cPlastics_on:   ii.image="JD_Plastics_gi_on":Next
			For each ii in cG01_on:        ii.image="JD_G01_gi_on":Next
			For each ii in cG02_on:        ii.image="JD_G02_gi_on":Next		
			For each ii in cG03_on:        ii.image="JD_G03_gi_on":Next	
			For each ii in cMetals_on:     ii.image="JD_Metals_gi_on":Next
			For each ii in cJDPursuit_on:  ii.image="JD_Pursuit_gi_on":Next
			For each ii in cRampwire_on:   ii.image="JD_Wireramps_gi_on":Next
			For each ii in cDecals_on:     ii.image="JD_Decals_gi_on":Next
		Case 5: 'off
			For each ii in cPlastics_on:   ii.image="JD_Plastics_gi_off":Next
			For each ii in cG01_on:        ii.image="JD_G01_gi_off":Next
			For each ii in cG02_on:        ii.image="JD_G02_gi_off":Next		
			For each ii in cG03_on:        ii.image="JD_G03_gi_off":Next	
			For each ii in cMetals_on:     ii.image="JD_Metals_gi_off":Next
			For each ii in cJDPursuit_on:  ii.image="JD_Pursuit_gi_off":Next
			For each ii in cRampwire_on:   ii.image="JD_Wireramps_gi_off":Next
			For each ii in cDecals_on:     ii.image="JD_Decals_gi_off":Next
	End Select
end Sub

'dim GILevel, GITarget, swapflag

'reset gioff material; just to make sure it matches to fading materials
UpdateMaterial "GI_OFF_Material",	0,0,0,0,0,0,1,RGB(255,255,255),0,0,False,True,0,0,0,0


dim gi2lvl, gi2prevlvl, kk, ballbrightness
const ballbrightMax = 255
const ballbrightMin = 115

Sub GIupdates(ByVal aLvl)	'GI update odds and ends go here
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically

	dim gi0lvl,gi1lvl,gi3lvl,gi4lvl
	if Lampz.UseFunction then 	'Callbacks don't get this filter automatically
		gi0lvl = LampFilter(ModLampz.Lvl(0))
		gi1lvl = LampFilter(ModLampz.Lvl(1))
		gi2lvl = LampFilter(ModLampz.Lvl(2))
		gi3lvl = LampFilter(ModLampz.Lvl(3))
		gi4lvl = LampFilter(ModLampz.Lvl(4))
	Else
		gi0lvl = ModLampz.Lvl(0)
		gi1lvl = ModLampz.Lvl(1)
		gi2lvl = ModLampz.Lvl(2)
		gi3lvl = ModLampz.Lvl(3)
		gi4lvl = ModLampz.Lvl(4)
	end if

'	debug.print "gi0lvl: " & gi0lvl
'	debug.print "gi1lvl: " & gi1lvl
'	debug.print "gi2lvl: " & gi2lvl
'	debug.print "gi3lvl: " & gi3lvl
'	debug.print "gi4lvl: " & gi4lvl


	if gi2lvl = 0 then										
		ballbrightness = ballbrightMin
	Elseif gi2lvl = 1 then									
		ballbrightness = ballbrightMax
	Else
		'debug.print gi2lvl & " --> " & ballbrightness
		ballbrightness = CInt(((ballbrightMax - ballbrightMin) * gi2lvl) + ballbrightMin)
	end if

	if Not IsEmpty(gBOT) then 
		if gi2lvl <> gi2prevlvl then
			For kk = 0 to UBound(gBOT)
				'debug.print "ball brightness: " & kk
				gBOT(kk).color = ballbrightness + (ballbrightness * 256) + (ballbrightness * 256 * 256)
			Next
		end if
	end if

	OnPrimsTransparency gi2lvl

	PLAYFIELD_GI_OFF.opacity = 100 - (100 * gi2lvl^2)
	
	RightBat.blenddisablelighting = 0.7 * gi2lvl^2
	LeftBat.blenddisablelighting = 0.7 * gi2lvl^2
	RightBatU.blenddisablelighting = 0.7 * gi2lvl^2
	LeftBatU.blenddisablelighting = 0.7 * gi2lvl^2

	For each kk in Ramps_Plastic:kk.blenddisablelighting = 0.2 * gi2lvl^2:Next
	For each kk in Cranes:kk.blenddisablelighting = DW_Disc.blenddisablelighting:Next

	DW_Disc.blenddisablelighting = 0.3 + 0.6 * gi2lvl^3
	DW_Globe.blenddisablelighting = 0.8 * gi2lvl^3
	DW_In_Disc.blenddisablelighting = 0.02 * gi2lvl^3
	DW_In_GI.blenddisablelighting = 25 * gi2lvl^2

	G03_Rails.blenddisablelighting = 0.01 * gi2lvl^2

	Coindoor_Inserts.blenddisablelighting = 0.5 * gi2lvl^2

	For each kk in GI_Bulbs:kk.blenddisablelighting = 1 * gi2lvl^0.8:Next

	gi2prevlvl = gi2lvl

'	'DOF
'	if gi0lvl = 0 Then
'		DOF 103, DOFOff
'	else
'		DOF 103, DOFOn
'	end If

End Sub


'Lamp Filter
Function LampFilter(aLvl)
	LampFilter = aLvl^1.6	'exponential curve?
End Function

Dim GIoffMult : GIoffMult = 2 'adjust how bright the inserts get when the GI is off
Dim GIoffMultFlashers : GIoffMultFlashers = 2	'adjust how bright the Flashers get when the GI is off

'Helper functions

Function ColtoArray(aDict)	'converts a collection to an indexed array. Indexes will come out random probably.
	redim a(999)
	dim count : count = 0
	dim x  : for each x in aDict : set a(Count) = x : count = count + 1 : Next
	redim preserve a(count-1) : ColtoArray = a
End Function

Set GICallback2 = GetRef("SetGI")

Sub SetGI(aNr, aValue)
	'debug.print "GI nro: " & aNr & " and step: " & aValue
	ModLampz.SetGI aNr, aValue 'Redundant. Could reassign GI indexes here
End Sub

Sub SetLamp(aNr, aOn)
	Lampz.state(aNr) = abs(aOn)
End Sub

Sub SetModLamp(aNr, aInput)
	ModLampz.state(aNr) = abs(aInput)/255
End Sub

'****************************************************************
'					End nFozzy lamp handling
'****************************************************************



'****************************************************************
'					Sling Shot Animations
'RStep and LStep are the variables that increment the animation
'****************************************************************

Dim RStep, LStep

Sub RightSlingShot_Slingshot
	RSling1.Visible = 1
	SlingRightP.TransZ = 20			'Sling Metal Bracket
	SlingRightPOff.TransZ = 20
	RStep = 0
	RightSlingShot.TimerEnabled = 1
	RightSlingShot.TimerInterval = 10
	vpmTimer.PulseSw 52
	RandomSoundSlingshotRight SlingRightP
End Sub

Sub RightSlingShot_Timer
  Select Case RStep
    Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:SlingRightP.TransZ = 10:SlingRightPOff.TransZ = 10
    Case 4:RSLing2.Visible = 0:SlingRightP.TransZ = 0:RightSlingShot.TimerEnabled = 0
  End Select
  RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	LSling1.Visible = 1
	SlingLeftP.TransZ = 20			'Sling Metal Bracket
	SlingLeftPOff.TransZ = 20
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
	LeftSlingShot.TimerInterval = 10
	vpmTimer.PulseSw 51
	RandomSoundSlingshotLeft SlingLeftP
End Sub

Sub LeftSlingShot_Timer
  Select Case LStep
    Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:SlingLeftP.TransZ = 10:SlingLeftPOff.TransZ = 10
    Case 4:LSLing2.Visible = 0:SlingLeftP.TransZ = 0:LeftSlingShot.TimerEnabled = 0
  End Select
  LStep = LStep + 1
End Sub

'****************************************************************
'						SOLENOID SUBS
'****************************************************************

Sub Diverter(enabled)
	If enabled Then
		JDDiverter.RotateToEnd
		PlaySoundAt "Diverter_Up",JDDiverter
	else
		JDDiverter.RotateToStart
		PlaySoundAt "Diverter_Down",JDDiverter
	End If
End Sub

Sub KickBack(enabled)
	If enabled Then
		Sol9.Fire
		PlaySoundAtVol "KickBack", Sol9, 1
	else
		Sol9.Pullback
	End If
End Sub

Sub JDPlunger(enabled)
	If enabled Then
		Sol8.Fire
		PlaySoundAtVol "PlungerFire", Sol8, 1
	else
		Sol8.Pullback
	End If
End Sub

Sub JDTrough(enabled)
	If enabled Then
		sw86.kick 37,30
		RandomSoundBallRelease SW86
		vpmTimer.PulseSw 87
	End If
End Sub

'****************************************************************
'			DEADWORLD CONTROL CODE - HERE BE DRAGONS.
'****************************************************************

Dim Tball1, tcount,xstart,ystart,zstart,xbstart,ybstart,zbstart,xphstart,yphstart,zphstart
Dim dradius, xyangle, zangle, dzangle

dim startAngle, TballWaiting, dzangle2, dzangle3, Tball2, Tball3

Set Tball1 = Nothing
Set Tball2 = Nothing
Set Tball3 = Nothing


'Function PI()
'	PI = 4*Atn(1)
'End Function

Function dSin(degrees)
	dsin = sin(degrees * PI/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * PI/180)
End Function

Function Radians(angle)
	Radians = PI * angle / 180
End Function

xphstart = 550.9
yphstart = 376.1
zphstart = 149

dradius= 192.5

'startAngle = AnglePP(DW_Disc2.x, DW_Disc2.y, xphstart, yphstart)
'startAngle = -42.5
startAngle = -27

zstart = dradius*sin(radians(-6))*cos(Radians(dzangle)) + dradius*sin(radians(3))*sin(Radians(dzangle)) + 15
xstart = dradius*cos(radians(startAngle))
ystart = dradius*sin(radians(startAngle))

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

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

const Slot1Pos = -58
const Slot2Pos = -178
const Slot3Pos = -295

const Drop1Pos = -215 '-95
const Drop2Pos = -328 '-220
const Drop3Pos = -95  '-328

Dim SlotBall:Set SlotBall = Nothing 								' assign the ball
Dim SlotPos:SlotPos=-1												' What position are we in
Dim Slot1Status, Slot2Status, Slot3Status
Slot1Status = 0: Slot2Status = 0: Slot3Status = 0

Sub SolWheelDrive(enabled)
'Debug.print "SolWheelDrive:" & enabled
    if enabled then
		set SlotBall=Nothing
		SlotPos=-1
        FWTimer.Enabled = 1
		PlaySoundAtLevelStaticLoop "Motor_DW", 0.25, DW_Pole
    else
        FWTimer.Enabled = 0
		Stopsound "Motor_DW"
    end if
End Sub

dim DW_angle : DW_angle = 0
dim DW_angle2

Sub FWTimer_Timer
    DW_Disc.RotZ = DW_angle
	DW_Screws.RotZ = DW_angle
	DW_angle = DW_angle - 0.4

	DW_Globe.RotZ = DW_angle2
	DW_angle2 = DW_angle2 + 0.4

	if DW_angle < -360 Then
		DW_angle = 0
	end if

'debug.print "DW_angle:" & DW_angle

	If (DW_angle < Drop1Pos and DW_angle > Drop1Pos - 1) and (Slot1Status = 1) Then
		Controller.Switch(77) = 1
'Debug.print "Setting SLOT1"
		SlotPos=1
		set SlotBall=tball1
	elseif (DW_angle < Drop2Pos and DW_angle > Drop2Pos - 1) and (Slot2Status = 1)  Then
		Controller.Switch(77) = 1
'Debug.print "Setting SLOT2"
		SlotPos=2
		set SlotBall=Tball2
	elseif (DW_angle < Drop3Pos and DW_angle > Drop3Pos - 1) and (Slot3Status = 1)  Then
		Controller.Switch(77) = 1
'Debug.print "Setting SLOT3"
		SlotPos=3
		set SlotBall=Tball3
	Else 
		Controller.Switch(77) = 0
	End If

End Sub


Sub TriggerDW_Hit						' We have a ball ready to be locked 
'	debug.print "TriggerDW_Hit"
	'msgbox "x: " & activeball.x & " y: " & activeball.y & " z: " & activeball.z
'	msgbox AnglePP(DW_Disc2.x, DW_Disc2.y, activeball.x, activeball.y)
	dim b

	For b = lob to UBound(gBOT)
		if activeball.id = gBOT(b).id then
'			debug.print "**Match! "&activeball.id &" vs "& gBOT(b).id
			If ramprolling(b) = True Then
				ramprolling(b) = False
				StopSound ("PlasticRamp_" & b):StopSound ("WireRamp_" & b)
			End If
			if rolling(b) = True then 
				rolling(b) = False
				StopSound("BallRoll_" & b)
			end if
		Else
'			debug.print activeball.id &" vs "& gBOT(b).id
		end if
	next

	activeball.velx=0
	activeball.vely=0
	activeball.velz=0
	activeball.x=xphstart
	activeball.y=yphstart
	activeball.z=zphstart
	Set TballWaiting = Activeball
	WheelHit.enabled = 1				' Start timer to catch the ball
	'TriggerDW.enabled = 0
End Sub

Sub WheelHit_Timer						' Add timer to catch the ball and store it
	dim b
	'Ball waits for the slot
	TballWaiting.x = xphstart
	TballWaiting.y = yphstart
	TballWaiting.z = zphstart
	TballWaiting.velx = 0
	TballWaiting.vely = 0
	TballWaiting.velz = 0

	'add flag to dwballs so we can skip rolling sound updates
	for b=0 to ubound(gBOT)
		if TballWaiting.ID = gBOT(b).ID then
			dwballs(b) = 1
		end if
	next

'	debug.print "wheelhit disc2: " & DW_angle

	If (DW_angle < Slot1Pos and DW_angle > Slot1Pos - 1) and (Slot1Status = 0) Then
'		debug.print "slot 1: " & DW_angle & " -> " & xphstart & yphstart & zphstart
		WheelHit.Enabled = 0
		Slot1Status = 1
		'let ball to drop into slot
		set Tball1 = TballWaiting
		Tball1.x = xphstart' - 5
		Tball1.y = yphstart' + 10
		Tball1.z = zphstart' - 3
'		debug.print "slot 1 started: " & DW_angle & " -> " & xphstart & yphstart & zphstart
		TriggerDW.TimerEnabled = 1
	End If
	if (DW_angle < Slot2Pos and DW_angle > Slot2Pos - 1) and (Slot2Status = 0)  Then
'		debug.print "slot 2: " & DW_angle
		WheelHit.Enabled = 0
		Slot2Status = 1
		'let ball to drop into slot
		set Tball2 = TballWaiting
		Tball2.x = xphstart' - 5
		Tball2.y = yphstart' + 10
		Tball2.z = zphstart' - 3
		TriggerDW.TimerEnabled = 1
	End If
	if (DW_angle < Slot3Pos and DW_angle > Slot3Pos - 1) and (Slot3Status = 0)  Then
'		debug.print "slot 3: " & DW_angle
		WheelHit.Enabled = 0
		Slot3Status = 1
		'let ball to drop into slot
		set Tball3 = TballWaiting
		Tball3.x = xphstart' - 5
		Tball3.y = yphstart' + 10
		Tball3.z = zphstart' - 3
		TriggerDW.TimerEnabled = 1
	End If

End Sub

Sub TriggerDW_timer
	'debug.print "asd x:" & activeball.x & " y " & activeball.y & " z " & activeball.z
	'debug.print dzangle
	'WheelHit.Enabled = 0
	
	Dim xd, yd, zd
	If tcount =  1 Then
		xbstart = xphstart - 22
		ybstart = yphstart + 14
		zbstart = zphstart - 7
'		debug.print "init tcount 1: " & " -> " & xbstart & ybstart & zbstart
		dzangle = startAngle
	End If

	If tcount > 2 Then
		if Slot1Status = 1 then
			dzangle = DW_angle + 30
			
			Tball1.x = xbstart - (xstart - dradius*cos(radians(dzangle)))
			Tball1.y = ybstart - (ystart - dradius*sin(radians(dzangle)))
			Tball1.z = zbstart - (zstart - dradius*sin(radians(-3))*cos(Radians(dzangle)) + dradius*sin(radians(6))*sin(Radians(dzangle)) + 15)
			Tball1.velx = 0
			Tball1.vely = 0
			Tball1.velz = 0
			'debug.print "init tcount " & tcount & " -> x:" & Tball1.x & " y:" & Tball1.y & " z:" & Tball1.z
			if dzangle < -360 Then
				dzangle = 0
			end if
		Else
			dzangle = startAngle
		end If
		if Slot2Status = 1 then
			dzangle2 = DW_angle + 150
			
			Tball2.x = xbstart - (xstart - dradius*cos(radians(dzangle2)))
			Tball2.y = ybstart - (ystart - dradius*sin(radians(dzangle2)))
			Tball2.z = zbstart - (zstart - dradius*sin(radians(-3))*cos(Radians(dzangle2)) + dradius*sin(radians(6))*sin(Radians(dzangle2)) + 15)
			Tball2.velx = 0
			Tball2.vely = 0
			Tball2.velz = 0
			'debug.print "init tcount " & tcount & " -> x:" & Tball1.x & " y:" & Tball1.y & " z:" & Tball1.z
			if dzangle2 < -360 Then
				dzangle2 = 0
			end if
		Else
			dzangle2 = startAngle
		end If
		if Slot3Status = 1 then
			dzangle3 = DW_angle + 270
			
			Tball3.x = xbstart - (xstart - dradius*cos(radians(dzangle3)))
			Tball3.y = ybstart - (ystart - dradius*sin(radians(dzangle3)))
			Tball3.z = zbstart - (zstart - dradius*sin(radians(-3))*cos(Radians(dzangle3)) + dradius*sin(radians(6))*sin(Radians(dzangle3)) + 15)
			Tball3.velx = 0
			Tball3.vely = 0
			Tball3.velz = 0
			'debug.print "init tcount " & tcount & " -> x:" & Tball1.x & " y:" & Tball1.y & " z:" & Tball1.z
			if dzangle3 < -360 Then
				dzangle3 = 0
			end if
		Else
			dzangle3 = startAngle
		end If
	End If
	tcount = tcount + 1
End Sub

Sub CraneArm(enabled)
'Debug.print "CraneArm:" & Enabled
	If enabled AND Crane_X.enabled = False Then
		CX_action = kCranePos_Disc:Crane_X.enabled = 1
		PlaySoundAtVol ("Motor_Crane"),SW67, 0.25
		Crane_Mod_LEDs.BlendDisableLighting = 10
		Crane_Mod3d_LEDs.BlendDisableLighting = 10
	elseif enabled=False then
		CX_action = kCranePos_Zero:Crane_X.enabled = 1
		StopSound "Motor_Crane"
		Crane_Mod_LEDs.BlendDisableLighting = .1
		Crane_Mod3d_LEDs.BlendDisableLighting = .1
	End if 
End Sub

Dim Magon

Sub CraneMag(enabled)
'Debug.print "CraneMag:" & enabled
	If enabled Then
		Magon = 1
		Controller.Switch(28) = 1
	else
		Set SlotBall=Nothing 
		Mag_off.enabled = 1
		Controller.Switch(28) = 0
	End If
End Sub

Sub Mag_off_Timer()
	Magon = 0
	me.enabled = 0
End Sub

'Crane Exit Opto, *critical* to keeping track of balls.
Sub SW62_Hit:							
'	Debug.print "SW62 HIT"
	Controller.Switch(62) = 1
End Sub

Sub SW62_UnHit()
	Controller.Switch(62) = 0
End Sub

'****************************************************************
'		Crane action if balls exist in the Deadworld holes
'****************************************************************
dim CX_action

Crane_X.Interval = 1	' Make it fast so we dont lose the ball
Dim holdX
Dim holdY
dim holdZ
const kSpeed=.05
const kCranePos_Left = 1
const kCranePos_Disc = 2
const kCranePos_Down = 3
const kCranePos_Up	 = 4
const kCranePos_Zero = 5
Sub Crane_X_Timer()
'Debug.print "Crane_X:" & CX_action & " Magon:" & Magon & " SlotBall:" & (SlotBall is Nothing)
	dim b
  Select Case CX_action
	case kCranePos_Left: 						' Go way left to orientate or Drop
		If Crane.RotY>=14 Then CX_action=kCranePos_Disc
		Crane.Roty = Crane.Roty + kSpeed 	 	

		if Magon and Not SlotBall is Nothing Then		' Pick up this ball
			holdX=holdX-(kSpeed*5)
			SlotBall.velx=0
			SlotBall.vely=0
			SlotBall.velz=0
			SlotBall.X=holdX
			SlotBall.Y=holdY
			SlotBall.Z=holdZ
'Debug.print "CRANE HAS BALL - LEFT: SlotPos:" & SlotPos
			If Crane.RotY>=14 Then
				for b=0 to ubound(gBOT)
					if SlotBall.ID = gBOT(b).ID then
'						debug.print "zeroing dwball for ID: " & SlotBall.ID & " and b: " & b
						dwballs(b) = 0
					end if
				next
			end if
		End if 

	case kCranePos_Disc:						' Go to Disc
		If Crane.RotY<-9 Then 
			CX_action = kCranePos_Down
			Controller.Switch(71)=1
		End if 
		Crane.RotY = Crane.RotY - kSpeed
	case kCranePos_Down:						' Lower
		If Crane.RotX<=80 Then 					' 86
			CX_action = kCranePos_Up
			if Not SlotBall is Nothing then
				holdX=SlotBall.X
				holdY=SlotBall.Y
				holdZ=SlotBall.Z
			End if 
		End if 
		Crane.RotX = Crane.RotX - kSpeed
	case kCranePos_Up:							' Raise
		Controller.Switch(71)=0
		If Crane.RotX=>89 Then CX_action = kCranePos_Left
		Crane.RotX = Crane.RotX + kSpeed

		if Magon and Not SlotBall is Nothing Then	' Pick up this ball
			if SlotPos=1 then Slot1Status =0
			if SlotPos=2 then Slot2Status =0
			if SlotPos=3 then Slot3Status =0
			holdZ=holdZ+(kSpeed*4)
			SlotBall.X=holdX
			SlotBall.Y=holdY
			SlotBall.Z=holdZ
'Debug.print "CRANE HAS BALL - UP SlotPos:" & SlotPos

		End if 

	case kCranePos_Zero:						' Go Back to 0 
		if Crane.RotY=0 then
			Me.enabled = False
		elseif Crane.RotY<0 then 
			Crane.Roty = Crane.Roty + kSpeed
			if Crane.Roty>=0 then 
				Crane.Roty=0
				Me.enabled = False
			End if 
		else
			Crane.Roty = Crane.Roty - kSpeed
			if Crane.Roty<=0 then 
				Crane.Roty=0
				Me.enabled = False
			End if 
		End if
  End Select
End Sub

'****************************************************************
'					END OF MAIN DEADWORLD CODE
'****************************************************************

'This disables the kickers after they create the captive balls on initial boot;
Sub CapBall1_Unhit()
    me.enabled = 0		
End Sub

Sub CapBall2_Unhit()
    me.enabled = 0
End Sub

Sub CapBall3_Unhit()
    me.enabled = 0
End Sub

'****************************************************************
'		Drop Target Controls
'****************************************************************

Sub Sw54_Hit:DTHit 54:TargetBouncer Activeball, 1.5:End Sub		'Drop Target "J"
Sub Sw55_Hit:DTHit 55:TargetBouncer Activeball, 1.5:End Sub		'Drop Target "U"
Sub Sw56_Hit:DTHit 56:TargetBouncer Activeball, 1.5:End Sub		'Drop Target "D"
Sub Sw57_Hit:DTHit 57:TargetBouncer Activeball, 1.5:End Sub		'Drop Target "G"
Sub Sw58_Hit:DTHit 58:TargetBouncer Activeball, 1.5:End Sub		'Drop Target "E"

Sub ResetDrops(enabled)
	if enabled then
		PlaySoundAt SoundFX(DTResetSound,DOFContactors), RubberBandPost6
		DTRaise 54
		DTRaise 55
		DTRaise 56
		DTRaise 57
		DTRaise 58
	end if
End Sub

Sub TripDrop(Enabled)
	if enabled then
		DTDrop 56		'Drops the center "D" drop target for certain modes to enable subway to be hit.
	end If
End Sub

'****************************************************************
'		Standup Target Controls
'****************************************************************

Sub sw25_Hit:STHit 25:End Sub
Sub sw27_Hit:STHit 27:End Sub
Sub sw36_Hit:STHit 36:End Sub
Sub sw68_Hit:STHit 68:End Sub
Sub sw18_Hit:STHit 180:End Sub		'Defining different custom calls here
Sub sw18a_Hit:STHit 181:End Sub		'as this is a bank of 3 targets
Sub sw18b_Hit:STHit 182:End Sub		'with the same switch number (sw18)

'****************************************************************

Sub Subway_Entrance_hit:PlaySoundAt "SubwayHit", RubberBandPost6:End Sub			'Subway Entrance Hitsound
Sub sw37_Hit:vpmTimer.PulseSw 37:PlaySoundAtBall "Subway":End Sub					'Subway Enter 1 (Start)
Sub sw38_Hit:vpmTimer.PulseSw 38:End Sub											'Subway Enter 2 (Mid-way)

Sub sw15_Hit:Controller.Switch(15) = 1:End Sub										'Left Shoot Lane KickBack
Sub sw15_UnHit:Controller.Switch(15) = 0:End Sub								 	'Left Shoot Lane KickBack
Sub sw41_Hit:Controller.Switch(41) = 1:End Sub										'Right Ball Shooter
Sub sw41_UnHit:Controller.Switch(41) = 0:End Sub									'Right Ball Shooter

Sub sw16_Hit:vpmTimer.PulseSw 16:End Sub											'Rollover Left Outlane
Sub sw17_Hit:vpmTimer.PulseSw 17:End Sub											'Rollover Left Return
Sub sw33_Hit:vpmTimer.PulseSw 33:End Sub											'Rollover Left Orbit
Sub sw34_Hit:vpmTimer.PulseSw 34:End Sub											'Rollover Right Return
Sub sw35_Hit:vpmTimer.PulseSw 35:End Sub											'Rollover Centre Orbit
Sub sw42_Hit:vpmTimer.PulseSw 42:End Sub											'Rollover Right Outlane
Sub sw43_Hit:vpmTimer.PulseSw 43:End Sub											'Rollover Right Outside Return

Sub sw26_Hit:Controller.Switch(26) = 1:End Sub										'Captive Ball 1
Sub sw26_UnHit:Controller.Switch(26) = 0:End Sub									'Captive Ball 1
Sub sw53_Hit:Controller.Switch(53) = 1:End Sub										'Captive Ball 2
Sub sw53_UnHit:Controller.Switch(53) = 0:End Sub									'Captive Ball 2

Sub sw66_Hit:vpmTimer.PulseSw 66:End Sub											'Opto "Blackout" Plastic Ramp
Sub sw72_Hit:vpmTimer.PulseSw 72:End Sub											'Opto Right Playfield Level Orbit

'****************************************************************
'		Bottom Left VUK to wire run
'****************************************************************
Sub sw73_Hit:bsBotVUK.AddBall 0:SoundSaucerLock:End Sub								'VUK Bottom Saucer Hit
Sub sw73_UnHit:PlaySoundAt "topvuk", sw73:End Sub									'VUK Bottom Kicked Out

'****************************************************************
'		Top Right VUK to top wire run
'****************************************************************
Sub sw74_Hit:bsTopVUK.AddBall 0:SoundSaucerLock:End Sub								'VUK Top Saucer Hit
Sub sw74_UnHit:PlaySoundAt "topvuk", sw74:End Sub									'VUK Top Kickout on to wire ramp
Sub RampWireTopEnd_Hit:PlaySoundAt "WireRamp_Stop", RampWireTopEnd:End Sub			'VUK Top Wire Ramp End Stop

'****************************************************************
'		"Air Raid" ramp to bottom wire run
'****************************************************************
Sub sw75_Hit:vpmTimer.PulseSw 75:PlaySoundAt "wire_enter", sw75:End Sub				'Opto Air Raid Ramp transition to Wire Ramp

'****************************************************************
'		"Stake Out" ramp to middle wire run
'****************************************************************
Sub sw65_Hit:vpmTimer.PulseSw 65:End Sub											'Opto Right Ramp Entrace (Not used?)
Sub sw76_Hit:vpmTimer.PulseSw 76:PlaySoundAt "wire_enter", sw76:End Sub				'Opto Right Ramp Transition to Wire Ramp
Sub RampWireMidEnd_Hit:PlaySoundAt "WireRamp_Stop", RampWireMidEnd:End Sub

'****************************************************************
'		Main Deadworld Plastic Orbit Ramp
'****************************************************************
'Left pursuit ramp entrance trigger 
'Position changed between L1 and L7 ROM Revisions - Do Not Modify.
'
'Sub SW32_Hit()			        		'Opto at entrance of ramp
'  If cGameName = "jd_l1" Then
'    vpmTimer.PulseSw 32
'  End If
'End Sub
'
'Sub SW67_Hit()					        'Opto almost at top of ramp
'  If cGameName = "jd_l7" Then
'    vpmTimer.PulseSw 67
'  End If
'End Sub
'
'Custom VPW / VPX Code Hack - Activating JD-L1 rom switch at new (better) JD-L7 position, needs L1 rom.

Sub sw67_Hit:vpmTimer.PulseSw 32:End Sub											'Opto Main Ramp Entrance

Sub sw63_Hit:vpmTimer.PulseSw 63:End Sub											'Opto Main Ramp Deadworld Entrance
Sub sw64_Hit:vpmTimer.PulseSw 64:End Sub											'Opto Main Ramp Exit
Sub RampMainEnd_Hit:PlaySoundAt "WireRamp_Stop", RampMainEnd:End Sub

Sub Drain_Hit
    RandomSoundDrain Drain
    Activeball.x = 54: Activeball.y = 2073    'Move ball to end of ball trough
    Drain.Kick 45,10
    Drain.enabled = 0
    TDrain.enabled = 1
End Sub

Sub TDrain_Timer()
	Drain.enabled = 1
	me.enabled = 0
End Sub

Sub SolKnocker(enabled)
	If enabled Then
		KnockerSolenoid()
	End If
End Sub

'****************************************************************
'				Populate The Trough on Game Launch.
'****************************************************************

Dim tball
tball = 1

Sub load_trough_timer()
  Select Case tball
    Case 1:Kicker_Load.createball:Kicker_Load.kick 45,10:tball = 2
    Case 2:Kicker_Load.createball:Kicker_Load.kick 45,10:tball = 3
    Case 3:Kicker_Load.createball:Kicker_Load.kick 45,10:tball = 4
    Case 4:Kicker_Load.createball:Kicker_Load.kick 45,10:tball = 5
    Case 5:Kicker_Load.createball:Kicker_Load.kick 45,10:tball = 6
    Case 6:Kicker_Load.createball:Kicker_Load.kick 45,10:tball = 7
    Case 7:me.enabled = 0
  End Select
End Sub

'Trough switches.

Sub SW86_Hit()
  W86.isdropped = 0
  Controller.Switch(86) = 1
End Sub

Sub SW86_Unhit()
  W86.isdropped = 1
  Controller.Switch(86) = 0
End Sub

Sub SW85_Hit()
  W85.isdropped = 0
  Controller.Switch(85) = 1
End Sub

Sub SW85_Unhit()
  W85.isdropped = 1
  Controller.Switch(85) = 0
End Sub

Sub SW84_Hit()
  W84.isdropped = 0
  Controller.Switch(84) = 1
End Sub

Sub SW84_Unhit()
  W84.isdropped = 1
  Controller.Switch(84) = 0
End Sub

Sub SW83_Hit()
  W83.isdropped = 0
  Controller.Switch(83) = 1
End Sub

Sub SW83_Unhit()
  W83.isdropped = 1
  Controller.Switch(83) = 0
End Sub

Sub SW82_Hit()
  W82.isdropped = 0
  Controller.Switch(82) = 1
End Sub

Sub SW82_Unhit()
  W82.isdropped = 1
  Controller.Switch(82) = 0
End Sub

Sub SW81_Hit()
  Controller.Switch(81) = 1
End Sub

Sub SW81_Unhit()
  Controller.Switch(81) = 0
End Sub

'****************************************************************
'		DROP TARGETS INITIALIZATION
'****************************************************************

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
Dim DT54, DT55, DT56, DT57, DT58

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
'       isDropped:  Boolean which determines whether a drop target is dropped. Set to false if they are initially raised, true if initially dropped.

Set DT54 = (new DropTarget)(sw54, sw54off, sw54prim, 54, 0, false)
Set DT55 = (new DropTarget)(sw55, sw55off, sw55prim, 55, 0, false)
Set DT56 = (new DropTarget)(sw56, sw56off, sw56prim, 56, 0, false)
Set DT57 = (new DropTarget)(sw57, sw57off, sw57prim, 57, 0, false)
Set DT58 = (new DropTarget)(sw58, sw58off, sw58prim, 58, 0, false)

Dim DTArray
DTArray = Array(DT54, DT55, DT56, DT57, DT58)

	'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 'in milliseconds
Const DTDropUpSpeed = 40 'in milliseconds
Const DTDropUnits = 44 'VP units primitive drops so top of at or below the playfield
Const DTDropUpUnits = 10 'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 'max degrees primitive rotates when hit
Const DTDropDelay = 20 'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40 'time in milliseconds before target drops back to normal up position after the solenoid fires to raise the target
Const DTBrickVel = 0 'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0 'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "target" 'Drop Target Hit sound
Const DTDropSound = "DropDown" 'Drop Target Drop sound
Const DTResetSound = "DropUp" 'Drop Target reset sound

Const DTMass = 0.2 'Mass of the Drop Target (between 0 and 1), higher values provide more resistance

'****************************************************************
'		DROP TARGETS FUNCTIONS
'****************************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)
	PlaySoundAtVol  DTHitSound, Activeball, Vol(Activeball)*22.5
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
	DoSTAnim
'	If psw1.transz < -DTDropUnits/2 Then drop1.visible = 0 else drop1.visible = 1
'	If psw2.transz < -DTDropUnits/2 Then drop2.visible = 0 else drop2.visible = 1
'	If psw3.transz < -DTDropUnits/2 Then drop3.visible = 0 else drop3.visible = 1
'	If psw4.transz < -DTDropUnits/2 Then drop4.visible = 0 else drop4.visible = 1
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

'****************************************************************
'		STAND-UP TARGET INITIALIZATION
'****************************************************************

Class StandupTarget
  Private m_primary, m_prim, m_sw, m_animate, m_target

  Public Property Get Primary(): Set Primary = m_primary: End Property
  Public Property Let Primary(input): Set m_primary = input: End Property

  Public Property Get Prim(): Set Prim = m_prim: End Property
  Public Property Let Prim(input): Set m_prim = input: End Property

  Public Property Get Sw(): Sw = m_sw: End Property
  Public Property Let Sw(input): m_sw = input: End Property

  Public Property Get Animate(): Animate = m_animate: End Property
  Public Property Let Animate(input): m_animate = input: End Property

  Public Property Get Target(): Target = m_target: End Property
  Public Property Let Target(input): m_target = input: End Property

  Public default Function init(primary, prim, sw, animate, target)
    Set m_primary = primary
    Set m_prim = prim
    m_sw = sw
    m_animate = animate
    m_target = target

    Set Init = Me
  End Function
End Class

'Define a variable for each stand-up target
Dim ST18, ST18a, ST18b, ST25, ST27, ST36, ST68

'Set array with stand-up target objects

'StandupTargetvar = Array(primary, prim, swtich)
' 	primary: 			vp target to determine target hit
'	prim:				primitive target used for visuals and animation
'						****IMPORTANT!!!****
'	------	transy must be used to offset the target animation  ------
'	switch:				ROM switch number
'	animate:			Arrary slot for handling the animation instructions, set to 0
'	target identifier:	The target

Set ST18 = (new StandupTarget)(sw18, sw18p, 18, 0, 180)
Set ST18a = (new StandupTarget)(sw18a, sw18ap, 18, 0, 181)
Set ST18b = (new StandupTarget)(sw18b, sw18bp, 18, 0, 182)
Set ST25 = (new StandupTarget)(sw25, sw25p, 25, 0, 25)
Set ST27 = (new StandupTarget)(sw27, sw27p, 27, 0, 27)
Set ST36 = (new StandupTarget)(sw36, sw36p, 36, 0, 36)
Set ST68 = (new StandupTarget)(sw68, sw68p, 68, 0, 68)

'Add all the Stand-up Target Arrays to Stand-up Target Animation Array
' STAnimationArray = Array(ST1, ST2, ....)
Dim STArray
STArray = Array(ST18, ST18a, ST18b, ST25, ST27, ST36, ST68)

'Configure the behavior of Stand-up Targets
Const STAnimStep =  1.5 		'vpunits per animation step (control return to Start)
Const STMaxOffset = 9 			'max vp units target moves when hit
'Const STHitSound = "target"	'Stand-up Target Hit sound - **Replaced with Fleep Code
Const STMass = 0.2				'Mass of the Stand-up Target (between 0 and 1), higher values provide more resistance

'****************************************************************
'		STAND-UP TARGETS FUNCTIONS
'****************************************************************

Sub STHit(switch)
	Dim i
	i = STArrayID(switch)

'	PlayTargetSound		'Replaced with Fleep Code
	STArray(i).animate =  STCheckHit(Activeball,STArray(i).primary)

	If STArray(i).animate <> 0 Then
		DTBallPhysics Activeball, STArray(i).primary.orientation, STMass
	End If
	DoSTAnim
End Sub

Function STArrayID(switch)
	Dim i
	For i = 0 to uBound(STArray) 
		If STArray(i).target = switch Then STArrayID = i:Exit Function 
	Next
End Function

'Check if target is hit on it's face
Function STCheckHit(aBall, target) 
	dim bangle, bangleafter, rangle, rangle2, perpvel, perpvelafter
	rangle = (target.orientation - 90) * 3.1416 / 180
	bangle = atn2(cor.ballvely(aball.id),cor.ballvelx(aball.id))
	bangleafter = Atn2(aBall.vely,aball.velx)

	perpvel = cor.BallVel(aball.id) * cos(bangle-rangle)
	perpvelafter = BallSpeed(aBall) * cos(bangleafter - rangle) 

	If perpvel <= 0 or perpvelafter >= 0 Then
		STCheckHit = 0 
	Else
		STCheckHit = 1
	End If
End Function

Sub DoSTAnim()
	Dim i
	For i=0 to Ubound(STArray)
		STArray(i).animate = STAnimate(STArray(i).primary,STArray(i).prim,STArray(i).sw,STArray(i).animate)
	Next
End Sub

Function STAnimate(primary, prim, switch, animate)
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

'****************************************************************
'		Class Jungle NF
'****************************************************************

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
	Public FadeSpeedDown(140), FadeSpeedUp(140)
	Private Lock(140), Loaded(140), OnOff(140)
	Public UseFunction
	Private cFilter
	Public UseCallback(140), cCallback(140)
	Public Lvl(140), Obj(140)
	Private Mult(140)
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
			OnOff(x) = False
			Lock(x) = True : Loaded(x) = False
			Mult(x) = 1
		Next
		Name = "LampFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OF THESE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
		for x = 0 to uBound(OnOff) 		'clear out empty obj
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		Next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property		'debug.print Lampz.Locked(100)	'debug
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

		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		'if idx = 132 then msgbox out	'debug
		ExecuteGlobal Out

	End Property

	Public Property Let state(ByVal idx, input) 'Major update path
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
		'debug.print debugstr
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 0.2 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'Just runs TurnOnStates right now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all boolean numeric fading. If done fading, Lock(x) = True. Update on a '1' interval Timer!
		dim x : for x = 0 to uBound(OnOff)
			if not Lock(x) then 'and not Loaded(x) then
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
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
				if OnOff(x) then 'Fade Up
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= 1 then Lvl(x) = 1 : Lock(x) = True
				elseif Not OnOff(x) then 'fade down
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= 0 then Lvl(x) = 0 : Lock(x) = True
				end if
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx : for x = 0 to uBound(OnOff)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(Lvl(x)*Mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*Mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(Lvl(x)*Mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)
					End If
				end if
				if TypeName(lvl(x)) <> "Double" and typename(lvl(x)) <> "Integer" then msgbox "uhh " & 2 & " = " & lvl(x)
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x))	'Callback
				If UseCallBack(x) then Proc name & x,Lvl(x)*mult(x)	'Proc
				If Lock(x) Then
					if Lvl(x) = 1 or Lvl(x) = 0 then Loaded(x) = True	'finished fading
				end if
			end if
		Next
	End Sub
End Class

'version 0.11 - Mass Assign, Changed modulate style
'version 0.12 - Update2 (single -1 timer update) update method for core.vbs
'Version 0.12a - Filter can now be publicly accessed via 'FilterOut'
'Version 0.12b - Changed MassAssign from a sub to an indexed property (new syntax: lampfader.MassAssign(15) = Light1 )
'Version 0.13 - No longer requires setlocale. Callback() can be assigned multiple times per index
'Version 0.13a - fixed DynamicLamps hopefully
' Note: if using multiple 'DynamicLamps' objects, change the 'name' variable to avoid conflicts with callbacks

Class DynamicLamps 'Lamps that fade up and down. GI and Flasher handling
	Public Loaded(50), FadeSpeedDown(50), FadeSpeedUp(50)
	Private Lock(50), SolModValue(50)
	Private UseCallback(50), cCallback(50)
	Public Lvl(50)
	Public Obj(50)
	Private UseFunction, cFilter
	private Mult(50)
	Public Name

	Public FrameTime
	Private InitFrame

	Private Sub Class_Initialize()
		InitFrame = 0
		dim x : for x = 0 to uBound(Obj)
			FadeSpeedup(x) = 0.01
			FadeSpeedDown(x) = 0.01
			lvl(x) = 0.0001 : SolModValue(x) = 0
			Lock(x) = True : Loaded(x) = False
			mult(x) = 1
			Name = "DynamicFaderNF" 'NEEDS TO BE CHANGED IF THERE'S MULTIPLE OBJECTS, OTHERWISE CALLBACKS WILL INTERFERE WITH EACH OTHER!!
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader' : Loaded(x) = True
		next
	End Sub

	Public Property Get Locked(idx) : Locked = Lock(idx) : End Property
	'Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function

	Public Property Let Callback(idx, String)
		UseCallBack(idx) = True
		'cCallback(idx) = String 'old execute method
		'New method: build wrapper subs using ExecuteGlobal, then call them
		cCallback(idx) = cCallback(idx) & "___" & String	'multiple strings dilineated by 3x _

		dim tmp : tmp = Split(cCallback(idx), "___")

		dim str, x : for x = 0 to uBound(tmp)	'build proc contents
			'debugstr = debugstr & x & "=" & tmp(x) & vbnewline
			'If Not tmp(x)="" then str = str & "	" & tmp(x) & " aLVL" & "	'" & x & vbnewline	'more verbose
			If Not tmp(x)="" then str = str & tmp(x) & " aLVL:"
		Next

		dim out : out = "Sub " & name & idx & "(aLvl):" & str & "End Sub"
		'if idx = 132 then msgbox out	'debug
		ExecuteGlobal Out
	End Property


	Public Property Let State(idx,Value)
		'If Value = SolModValue(idx) Then Exit Property ' Discard redundant updates
		If Value <> SolModValue(idx) Then ' Discard redundant updates
			SolModValue(idx) = Value
			Lock(idx) = False : Loaded(idx) = False
		End If
	End Property

	Public Property Get state(idx) : state = SolModValue(idx) : end Property

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

	'solcallback (solmodcallback) handler
	Sub SetLamp(aIdx, aInput) : state(aIdx) = aInput : End Sub	'0->1 Input
	Sub SetModLamp(aIdx, aInput) : state(aIdx) = aInput/255 : End Sub	'0->255 Input
	Sub SetGI(aIdx, ByVal aInput) : if aInput = 8 then aInput = 7 end if : state(aIdx) = aInput/7 : End Sub	'0->8 WPC GI input

	Public Sub TurnOnStates()	'If obj contains any light objects, set their states to 1 (Fading is our job!)
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then
				'debugstr = debugstr & "array found at " & idx & "..."
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x) ': debugstr = debugstr & tmp(x).name & " state'd" & vbnewline

				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx) ': debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline

			end if
		Next
		'debug.print debugstr
	End Sub

	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1

	Public Sub Init()	'just call turnonstates for now
		TurnOnStates
	End Sub

	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx) = False : Loaded(aIdx) = False: End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property

	Public Sub Update1()	 'Handle all numeric fading. If done fading, Lock(x) = True
		'dim stringer
		dim x : for x = 0 to uBound(Lvl)
			'stringer = "Locked @ " & SolModValue(x)
			if not Lock(x) then 'and not Loaded(x) then
				If lvl(x) < SolModValue(x) then '+
					'stringer = "Fading Up " & lvl(x) & " + " & FadeSpeedUp(x)
					Lvl(x) = Lvl(x) + FadeSpeedUp(x)
					if Lvl(x) >= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				ElseIf Lvl(x) > SolModValue(x) Then '-
					Lvl(x) = Lvl(x) - FadeSpeedDown(x)
					'stringer = "Fading Down " & lvl(x) & " - " & FadeSpeedDown(x)
					if Lvl(x) <= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				End If
			end if
		Next
		'tbF.text = stringer
	End Sub

	Public Sub Update2()	 'Both updates on -1 timer (Lowest latency, but less accurate fading at 60fps vsync)
		FrameTime = gametime - InitFrame : InitFrame = GameTime	'Calculate frametime
		dim x : for x = 0 to uBound(Lvl)
			if not Lock(x) then 'and not Loaded(x) then
				If lvl(x) < SolModValue(x) then '+
					Lvl(x) = Lvl(x) + FadeSpeedUp(x) * FrameTime
					if Lvl(x) >= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				ElseIf Lvl(x) > SolModValue(x) Then '-
					Lvl(x) = Lvl(x) - FadeSpeedDown(x) * FrameTime
					if Lvl(x) <= SolModValue(x) then Lvl(x) = SolModValue(x) : Lock(x) = True
				End If
			end if
		Next
		Update
	End Sub

	Public Sub Update()	'Handle object updates. Update on a -1 Timer! If done fading, loaded(x) = True
		dim x,xx
		for x = 0 to uBound(Lvl)
			if not Loaded(x) then
				if IsArray(obj(x) ) Then	'if array
					If UseFunction then
						for each xx in obj(x) : xx.IntensityScale = cFilter(abs(Lvl(x))*mult(x)) : Next
					Else
						for each xx in obj(x) : xx.IntensityScale = Lvl(x)*mult(x) : Next
					End If
				else						'if single lamp or flasher
					If UseFunction then
						obj(x).Intensityscale = cFilter(abs(Lvl(x))*mult(x))
					Else
						obj(x).Intensityscale = Lvl(x)*mult(x)
					End If
				end if
				'If UseCallBack(x) then execute cCallback(x) & " " & (Lvl(x)*mult(x))	'Callback
				If UseCallBack(x) then Proc name & x,Lvl(x)*mult(x)	'Proc
				If Lock(x) Then
					Loaded(x) = True
				end if
			end if
		Next
	End Sub
End Class

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

'****************************************************************
'						Fleep Audio Package
'****************************************************************

'////////////////////////////  MECHANICAL SOUNDS  ///////////////////////////
'//  This part in the script is an entire block that is dedicated to the physics sound system.
'//  Various scripts and sounds that may be pretty generic and could suit other WPC systems
'//  most are tailored specifically for this table.

'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1								'volume level; range [0, 1]
NudgeLeftSoundLevel = 1							'volume level; range [0, 1]
NudgeRightSoundLevel = 1						'volume level; range [0, 1]
NudgeCenterSoundLevel = 1						'volume level; range [0, 1]
StartButtonSoundLevel = 0.1						'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 					'volume level; range [0, 1]
PlungerPullSoundLevel = 1						'volume level; range [0, 1]
RollingSoundFactor = 1.1/5                

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010		'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635		'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45					'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel		'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel		'sound helper; not configurable
SlingshotSoundLevel = 0.95						'volume level; range [0, 1]
BumperSoundFactor = 4.25						'volume multiplier; must not be zero
KnockerSoundLevel = 1							'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2			'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5				'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5					'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5				'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025			'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025			'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8		'volume level; range [0, 1]
WallImpactSoundFactor = 0.075					'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////
Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5							'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10					'volume multiplier; must not be zero
DTSoundLevel = 0.25								'volume multiplier; must not be zero
RolloverSoundLevel = 0.25						'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8							'volume level; range [0, 1]
BallReleaseSoundLevel = 1						'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2			'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015				'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025/5						'volume multiplier; must not be zero

Dim LutToggleSoundLevel
LutToggleSoundLevel = 0.01

'/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////
'/////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ///////////////////////
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

Sub PlaySoundAtLevelTimerActiveBall(playsoundparams, aVol, ballvariable)
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
'		Fleep  Supporting Ball & Sound Functions
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

Function VolPlasticRampRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlasticRampRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
    PitchPlayfieldRoll = BallVel(ball) ^2 * 15
End Function

Function PitchPlasticRamp(ball) ' Calculates the pitch of the sound based on the ball speed - used for plastic ramps roll sound
    PitchPlasticRamp = BallVel(ball) * 20
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
        PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Sol8'Plunger
End Sub

Sub SoundPlungerReleaseBall()
        PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Sol8'Plunger        
End Sub

Sub SoundPlungerReleaseNoBall()
        PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Sol8'Plunger
End Sub

'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////
Sub KnockerSolenoid()
        PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, DiverterP
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

'////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'///////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  /////////////////////////
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

'////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////
'/////////////////////////////  RUBBERS AND POSTS  /////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  //////////////////////////////
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

'////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  '////////////////////
Sub RandomSoundRubberStrong(voladj)
        Select Case Int(Rnd*10)+1
                Case 1 : PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
                Case 2 : PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
                Case 3 : PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
                Case 4 : PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
                Case 5 : PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
                Case 6 : PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
                Case 7 : PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
                Case 8 : PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
                Case 9 : PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
                Case 10 : PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6 * voladj
        End Select
End Sub

'////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  '////////////////////
Sub RandomSoundRubberWeak()
		PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd*9)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'//////////////////////////////  WALL IMPACTS  '//////////////////////////////
Sub Walls_Hit(idx)
		RandomSoundWall
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

'//////////////////////////////  METAL TOUCH SOUNDS  /////////////////////////////
Sub RandomSoundMetal()
		PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd*13)+1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'////////////////////////////////  METAL - EVENTS  ///////////////////////////////
Sub Metals_Hit (idx)
		RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
		RandomSoundMetal
End Sub

'////////////////////////////  BOTTOM ARCH BALL GUIDE  ///////////////////////////
'////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////
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
	TargetBouncer Activeball, 1.5
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
'                      End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'***********************************************************************
'		BALL ROLLING AND DROP SOUNDS & Wylte's Ray Tracing Ball Shadows
'***********************************************************************

Const tnob = 10			'total number of balls = Number of balls installed in machine +1
Const lob = 3			'number of locked balls

' *** Shadow Options ***
Const fovY					= -2	'Offset y position under ball to account for layback or inclination (more pronounced need further back, -2 seems best for alignment at slings)
Const DynamicBSFactor 		= 1 	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.7	'0 to 1, higher is darker
Const Wideness				= 20	'Sets how wide the shadows can get (20 +5 thinness should be most realistic)
Const Thinness				= 5		'Sets minimum as ball moves away from source
' ***				 ***

ReDim sourcenames(tnob)
ReDim currentShadowCount(tnob)
ReDim rolling(tnob)
ReDim ramprolling(tnob)
ReDim dwballs(tnob)
ReDim inlaneballs(tnob)

InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
		ramprolling(i) = False
		inlaneballs(i) = False
	Next
End Sub

dim objrtx1(20), objrtx2(20)
dim objBallShadow(10)
Dim BallShadowA
BallShadowA = Array (BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5,BallShadowA6,BallShadowA7,BallShadowA8,BallShadowA9,BallShadowA10)
DynamicBSInit

sub DynamicBSInit()
	Dim iii

	for iii = 0 to tnob									'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = iii/1000 + 0.11
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = (iii)/1000 + 0.12
		objrtx2(iii).visible = 0
		currentShadowCount(iii) = 0
		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		objBallShadow(iii).Z = iii/1000 + 0.14
		objBallShadow(iii).visible = 0
	Next
end sub

dim gBOT
gBOT = GetBalls

Sub RollingUpdate()
	Dim falloff:	falloff = 150			'Max distance to light sources, can be changed if you have a reason
	Dim s,b,t,bb
	Dim ShadowOpacity, ShadowOpacity2 
	Dim Source, LSd, CntDwn, currentMat, AnotherSource

'    Dim cntActBall : cntActBall = 0

'	' stop the sound of deleted balls
'	For b = UBound(gBOT) + 1 to tnob
'		rolling(b) = False
'		ramprolling(b) = False
'		StopSound ("BallRoll_" & b)
'		StopSound ("PlasticRamp_" & b)
'		StopSound ("WireRamp_" & b)
'	Next

	' hide shadow of deleted balls
	For t = lob to UBound(gBOT)
		' hide shadow of deleted balls
        If Not (InRect(gBOT(t).X, gBOT(t).y, 422,2060,850,1780,850,1920,460,2130) Or InRect(gBOT(t).x, gBOT(t).y, 15,880,88,880,105,970,44,1010)) Then
            'msgbox "on table"
            if Not (activeShadowBall.Exists(gBOT(t).ID)) Then
				activeShadowBall.Add gBOT(t).ID, ""
            end If
        else
			'msgbox "remove ball"
            if (activeShadowBall.Exists(gBOT(t).ID)) then
				activeShadowBall.Remove(gBOT(t).ID)
            end if
        end If
	Next

	' exit the sub if no balls on the table
	If UBound(gBOT) = -1 Then Exit Sub

	' play the rolling sound for each ball
	For b = 0 to UBound(gBOT)
'		If BallVel(gBOT(b) ) > 1 Then
'			rolling(b) = True
		If Not (InRect(gBOT(b).X, gBOT(b).y, 16,2030,810,2110,755,2170,12,2100) or InRect(gBOT(b).X, gBOT(b).y, 915,945,985,955,900,1225,837,1195) or InRect(gBOT(b).X, gBOT(b).y, 928,715,995,710,985,955,915,945)) Then
'*Six - ^inrect for apron feed and 2x inrects for captive ball lane (avoiding right hand stakeout ramp)

			'Normal ambient shadow
			If AmbientShadowOn = 1 And inlaneballs(b) = False Then
				If gBOT(b).X < tablewidth/2 Then
					objBallShadow(b).X = ((gBOT(b).X) - (Ballsize/10) + ((gBOT(b).X - (tablewidth/2))/12.5)) + 5
				Else
					objBallShadow(b).X = ((gBOT(b).X) + (Ballsize/10) + ((gBOT(b).X - (tablewidth/2))/12.5)) - 5
				End If
				objBallShadow(b).Y = gBOT(b).Y + fovY
				if gBOT(b).z < 0 Then 
					objBallShadow(b).visible = 0
				Else
					objBallShadow(b).visible = 1
				End If
			Elseif AmbientShadowOn = 1 And inlaneballs(b) = True Then
				objBallShadow(b).visible = 0
				objBallShadow(b).X = gBOT(b).X
				objBallShadow(b).Y = gBOT(b).Y
			End If

            '***Balls Rolling in Subway***
			if gBOT(b).z < 0 Then
				'objBallShadow(b).visible = 0
'				BallShadowA(b).visible = 0
            '***Balls Rolling on Playfield*** - By knowing the ball is at playfield level.
			ElseIf gBOT(b).z < 30 Then
				'if b = 3 then debug.print "pf"
				If BallVel(gBOT(b) ) > 1 Then
					If ramprolling(b) = True Then
						ramprolling(b) = False
						StopSound ("PlasticRamp_" & b)
						StopSound ("WireRamp_" & b)
					End If
					rolling(b) = True
					PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(gBOT(b)) * 1.1 * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))
				Else
					If rolling(b) = True Then
						StopSound("BallRoll_" & b)
						rolling(b) = False
					End If
				end If
'				objBallShadow(b).RotX = 0
'				objBallShadow(b).Z = b/1000 + 0.14
				BallShadowA(b).visible = 0
				objBallShadow(b).size_x = 6.5
				objBallShadow(b).size_y = 5.0

				'If cntActBall < 3 And DynamicBallShadowsOn = 1 Then
				If DynamicBallShadowsOn = 1 And inlaneballs(b) = False Then
					If (activeShadowBall.Exists(gBOT(b).ID)) Then
						'cntActBall = cntActBall + 1
						'RTX shadows

				
						For Each Source in RtxBS							'Rename this to match your collection name
							'debug.print "RTXShadow for ball: " & s
							LSd=DistanceFast((gBOT(b).x-Source.x),(gBOT(b).y-Source.y))			'Calculating the Linear distance to the Source
							If gBOT(b).z < 28 and gBOT(b).Z >0 Then
								If LSd < falloff And Source.state = 1 Then			'If the ball is within the falloff range of a light
									currentShadowCount(b) = currentShadowCount(b) + 1
									if currentShadowCount(b) = 1 Then					'1 dynamic shadow source
										sourcenames(b) = source.name
										currentMat = objrtx1(b).material
										objrtx2(b).visible = 0 : objrtx1(b).visible = 1 : objrtx1(b).X = gBOT(b).X : objrtx1(b).Y = gBOT(b).Y + fovY
										objrtx1(b).rotz = AnglePP(Source.x, Source.y, gBOT(b).X, gBOT(b).Y) + 90
										ShadowOpacity = (falloff-LSd)/falloff									'Sets opacity/darkness of shadow by distance to light
										objrtx1(b).size_y = Wideness*ShadowOpacity+Thinness						'Scales shape of shadow with distance/opacity
										UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0
										'debug.print "update1" & source.name & " at:" & ShadowOpacity

										'currentMat = objBallShadow(b).material
										'UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-ShadowOpacity),RGB(0,0,0),0,0,False,True,0,0,0,0

									Elseif currentShadowCount(b) = 2 Then
																				'Same logic as 1 shadow, but twice
										currentMat = objrtx1(b).material
										set AnotherSource = Eval(sourcenames(b))
										objrtx1(b).visible = 1 : objrtx1(b).X = gBOT(b).X : objrtx1(b).Y = gBOT(b).Y + fovY
										objrtx1(b).rotz = AnglePP(AnotherSource.x, AnotherSource.y, gBOT(b).X, gBOT(b).Y) + 90
										ShadowOpacity = (falloff-(((gBOT(b).x-AnotherSource.x)^2+(gBOT(b).y-AnotherSource.y)^2)^0.5))/falloff
										objrtx1(b).size_y = Wideness*ShadowOpacity+Thinness
										UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

										currentMat = objrtx2(b).material
										objrtx2(b).visible = 1 : objrtx2(b).X = gBOT(b).X : objrtx2(b).Y = gBOT(b).Y + fovY
										objrtx2(b).rotz = AnglePP(Source.x, Source.y, gBOT(b).X, gBOT(b).Y) + 90
										ShadowOpacity2 = (falloff-LSd)/falloff
										objrtx2(b).size_y = Wideness*ShadowOpacity2+Thinness
										UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
										'debug.print "update2: " & source.name & " at:" & ShadowOpacity & " and "  & Eval(sourcenames(b)).name & " at:" & ShadowOpacity2

										'currentMat = objBallShadow(b).material
										'UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
									end if
								Else
									currentShadowCount(b) = 0
								End If
							Else
							'other rules here, like ramps and upper pf
								objrtx2(b).visible = 0 : objrtx1(b).visible = 0
							End If
						Next
					end If
				Elseif inlaneballs(b) = True Then
					objrtx2(b).visible = 0 : objrtx1(b).visible = 0
				end If
				

			ElseIf gBOT(b).z > 40 Then

            '***Balls Rolling on Plastic Ramps*** - By knowing the ball is NOT inside co-ordinate defined rectangles covering the wire ramp area
				If not InRect(gBOT(b).x, gBOT(b).y, 10,755,885,1200,885,1785,10,1785) and Not InRect(gBOT(b).x, gBOT(b).y, 713,197,865,233,700,1106,350,928) Then
'					if (gBOT(b).z > 120 And gBOT(b).z < 157) And InRect(gBOT(b).x, gBOT(b).y, 195,223,592,242,613,739,96,715) then
					'front part of the disc may still play ramprolling???
					'debug.print "plastic: " & b & " at z: " & gBOT(b).z					'if IsObject(tball1) 
					'if tball1.ID = gBOT(b).ID Or tball2.ID = gBOT(b).ID Or tball3.ID = gBOT(b).ID Or TballWaiting.ID  = gBOT(b).ID then
					BallShadowA(b).X = gBOT(b).X			'Flasher shadows for ramps
					ballShadowA(b).Y = gBOT(b).Y + 12.5 + fovY
					BallShadowA(b).visible = 1
					BallShadowA(b).height=gBOT(b).z - 12.5
					if dwballs(b) = 1 then
							'debug.print "DW: " & b & " at z: " & gBOT(b).z
							If ramprolling(b) = True Then
								ramprolling(b) = False
								StopSound ("PlasticRamp_" & b):StopSound ("WireRamp_" & b)
							End If
'							objBallShadow(b).Z = gBOT(b).Z - 25 + s/1000 + 0.1
							'objBallShadow(b).RotX = 0
							'objBallShadow(b).size_x = 6.5 * ((gBOT(b).Z-(ballsize/2))/70)
							'objBallShadow(b).size_y = 5.0 * ((gBOT(b).Z-(ballsize/2))/70)
					else
'						debug.print "plastic: " & b & " at z: " & gBOT(b).z
						ramprolling(b) = True
						StopSound ("WireRamp_" & b):PlaySound ("PlasticRamp_" & b), -1, VolPlasticRampRoll(gBOT(b)) * 1.1 * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlasticRamp(gBOT(b)), 1, 0, AudioFade(gBOT(b))
'						objBallShadow(b).Z = gBOT(b).Z - 25 + s/1000 + 0.1
'						objBallShadow(b).RotX = -40
					end if

            '***Balls Rolling on Wire Ramps*** - By knowing the ball is inside co-ordinate defined rectangles covering the wire ramp area.
				Elseif InRect(gBOT(b).x, gBOT(b).y, 10,755,885,1200,885,1785,10,1785) or InRect(gBOT(b).x, gBOT(b).y, 713,197,865,233,700,1106,350,928) Then
					'debug.print "wire: " & b & " at z: " & gBOT(b).z
					ramprolling(b) = True
					StopSound ("PlasticRamp_" & b):PlaySound ("WireRamp_" & b), -1, VolPlasticRampRoll(gBOT(b)) * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlasticRamp(gBOT(b)), 1, 0, AudioFade(gBOT(b))
'					objBallShadow(b).Z = b/1000 + 0.14
'					objBallShadow(b).RotX = 0
					BallShadowA(b).visible = 0
					objBallShadow(b).size_x = 6 * ((gBOT(b).Z-(ballsize/2))/70)
					objBallShadow(b).size_y = 5 * ((gBOT(b).Z-(ballsize/2))/70)
				Else
'					debug.print "some other place o.O: " & b & " at z: " & gBOT(b).z
					BallShadowA(b).visible = 0
					If ramprolling(b) = True Then
                        ramprolling(b) = False
						StopSound ("PlasticRamp_" & b):StopSound ("WireRamp_" & b)
					End If
				End If
			else
				If ramprolling(b) = True Then
					ramprolling(b) = False
					StopSound ("PlasticRamp_" & b):StopSound ("WireRamp_" & b)
				End If
			End If

		Else
			'if b = 3 then debug.print "under apron"

			If rolling(b) = True Then
				rolling(b) = False
				StopSound("BallRoll_" & b)
			End If
			If ramprolling(b) = True Then
				ramprolling(b) = False
				StopSound ("PlasticRamp_" & b)
				StopSound ("WireRamp_" & b)
			End If
		End If

		'***Ball Drop Sounds***
		If gBOT(b).VelZ < -1 and gBOT(b).z < 55 and gBOT(b).z > 27 Then 'height adjust for ball drop sounds
			'debug.print "ball drop" & gBOT(b).velz
			If DropCount(b) >= 2 Then
				DropCount(b) = 0
				If gBOT(b).velz > -7 Then
					'debug.print "random sound soft"
					RandomSoundBallBouncePlayfieldSoft gBOT(b)
				Else
					'debug.print "random sound hard"
					RandomSoundBallBouncePlayfieldHard gBOT(b)
				End If				
			End If
'		Else
'			If gBOT(b).VelZ < -1 then
'				debug.print "ball z: " & gBOT(b).z & " ball velz: " & gBOT(b).VelZ
'			end if
		End If
		If DropCount(b) < 2 Then
			DropCount(b) = DropCount(b) + 1
		End If

		'brightness
		'if ballbrightness <> -1 then 
		'if gi2lvl <> gi2prevlvl then
			'gBOT(b).color = ballbrightness + (ballbrightness * 256) + (ballbrightness * 256 * 256)
			'if b = UBound(gBOT) then 'until last ball brightness is set, then reset to -1
				'if ballbrightness = ballbrightMax Or ballbrightness = ballbrightMin then ballbrightness = -1
			'end if
		'end If

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

'Detect when balls are in the inlanes to manage shadows there
Sub TriggerLaneShL_Hit : 	 inlaneballs(activeball.id)=True : End Sub
Sub TriggerLaneShL_UnHit : inlaneballs(activeball.id)=False : End Sub
Sub TriggerLaneShR_Hit : 	 inlaneballs(activeball.id)=True : End Sub
Sub TriggerLaneShR_UnHit : inlaneballs(activeball.id)=False : End Sub

'****************************************************************
'		End Fleep Audio
'****************************************************************

'****************************************************************
'		Crane Mod - cyberpez
'****************************************************************

Sub SetCraneMod ()

	If CraneMod = 0 Then
		Crane.Visible = True
		Crane_Mod_a.Visible = False
		Crane_Mod_b.Visible = False
		Crane_Mod_LEDs.Visible = False
		Crane_Mod3d_a.Visible = False
		Crane_Mod3d_b.Visible = False
		Crane_Mod3d_LEDs.Visible = False
		Crane_Arm.Visible = False
	End If

	If CraneMod = 1 Then
		Crane_Mod_a.Visible = False
		Crane_Mod_b.Visible = False
		Crane_Mod_LEDs.Visible = False
		Crane_Mod3d_a.Visible = True
		Crane_Mod3d_b.Visible = True
		Crane_Mod3d_LEDs.Visible = True
		Crane.Visible = false
		Crane_Arm.Visible = True
	End If

	If CraneMod = 2 Then
		Crane_Mod_a.Visible = True
		Crane_Mod_b.Visible = True
		Crane_Mod_LEDs.Visible = True
		Crane_Mod3d_a.Visible = False
		Crane_Mod3d_b.Visible = False
		Crane_Mod3d_LEDs.Visible = False
		Crane.Visible = false
		Crane_Arm.Visible = True
		Crane_Mod_a.Material = "Metal_Black_Powdercoat"
	End If

	If CraneMod = 3 Then
		Crane_Mod_a.Visible = True
		Crane_Mod_b.Visible = True
		Crane_Mod_LEDs.Visible = True
		Crane_Mod3d_a.Visible = False
		Crane_Mod3d_b.Visible = False
		Crane_Mod3d_LEDs.Visible = False
		Crane.Visible = false
		Crane_Arm.Visible = True
		Crane_Mod_a.Material = "MetalShiny"
	End If

End Sub




'****************************************************************
'		Cabinet Mode
'****************************************************************

If CabinetMode Then
	G03_Rails.visible = 0
	Else
	G03_Rails.visible = 1
End If


'****************************************************************
'		VR Mode
'****************************************************************
'0 - VR Room Off, 1 - Minimal Room, 2 - 360 Room, 3 - Ultra Minimal

DIM VRThings
If VRRoom > 0 Then
	ScoreText.visible = 0
	DMD.visible = 1
	G03_Rails.visible = 1
	Table1.PlayfieldReflectionStrength = 40
	If VRRoom = 1 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 1:Next
		for each VRThings in VR_360:VRThings.visible = 0:Next
		for each VRThings in CoindoorStuff:VRThings.visible = 1:Next
	End If
	If VRRoom = 2 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		for each VRThings in VR_360:VRThings.visible = 1:Next
		for each VRThings in CoindoorStuff:VRThings.visible = 1:Next
	End If
	If VRRoom = 3 Then
		for each VRThings in VR_Cab:VRThings.visible = 0:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		for each VRThings in VR_360:VRThings.visible = 0:Next
		for each VRThings in CoindoorStuff:VRThings.visible = 0:Next
		PinCab_Backbox.visible = 1
	End If
Else
	for each VRThings in VR_Cab:VRThings.visible = 0:Next
	for each VRThings in VR_Min:VRThings.visible = 0:Next
	for each VRThings in VR_360:VRThings.visible = 0:Next
	for each VRThings in CoindoorStuff:VRThings.visible = 0:Next
End if

'****************************************************************
'		Preload Flashers and Textures
'****************************************************************

dim preloadCounter
sub preloader_timer
	preloadCounter = preloadCounter + 1
	if preloadCounter = 1 then
'        BothPrimsVisible
'        OffPrimSwap 1,1,False
	Elseif preloadCounter = 2 then
'        OffPrimSwap 1,0,False
	Elseif preloadCounter = 3 then
'        OffPrimSwap 2,1,False
	Elseif preloadCounter = 4 then
'        OffPrimSwap 2,0,False
	Elseif preloadCounter = 5 then
'        OffPrimSwap 1,1,True
'        OffPrimsVisible False
	Elseif preloadCounter = 6 then
'		sol4r true
	Elseif preloadCounter = 7 then
'		sol2r true
	Elseif preloadCounter = 8 then
'		lampz.state(111) = 5
	Elseif preloadCounter = 12 then
		me.enabled = false
	end if
	gBOT = GetBalls
	'msgbox preloadCounter
end sub

'****************************************************************
'		Wobble's!
'****************************************************************

Dim WobbleValue

tWobblePR.interval = 25 ' Controls the speed of the wobble
tWobblePL.interval = 25 ' Controls the speed of the wobble

Sub PursuitRT_hit()
	If Activeball.velx > 1 Then
		tWobblePR.enabled=true
		WobbleValue = 3
	End If
End Sub

Sub PursuitLT_Hit()
	If Activeball.velx > 1 Then
		tWobblePL.enabled=true
		WobbleValue = 3
	End If
End Sub

Sub tWobblePR_timer()

	Pursuit_RSticker.z=WobbleValue:Pursuit_RLights.z=WobbleValue:Pursuit_RSticker1.z=WobbleValue:Pursuit_RLights1.z=WobbleValue
	Pursuit_RSticker.x=WobbleValue:Pursuit_RLights.x=WobbleValue:Pursuit_RSticker1.x=WobbleValue:Pursuit_RLights1.x=WobbleValue

	if WobbleValue < 0 then
		WobbleValue = abs(WobbleValue) * 0.9 - 0.1
	Else
		WobbleValue = -abs(WobbleValue) * 0.9 + 0.1
	end if

'	debug.print WobbleValue

	if abs(WobbleValue) < 0.1 Then
		Pursuit_RSticker.z=0:Pursuit_RLights.z=0:Pursuit_RSticker1.z=0:Pursuit_RLights1.z=0
		Pursuit_RSticker.x=0:Pursuit_RLights.x=0:Pursuit_RSticker1.x=0:Pursuit_RLights1.x=0
		tWobblePR.Enabled = false
	end If
end Sub

Sub tWobblePL_timer()

	Pursuit_LSticker.z=WobbleValue:Pursuit_LLights.z=WobbleValue:Pursuit_LSticker1.z=WobbleValue:Pursuit_LLights1.z=WobbleValue
	Pursuit_LSticker.x=WobbleValue:Pursuit_LLights.x=WobbleValue:Pursuit_LSticker1.x=WobbleValue:Pursuit_LLights1.x=WobbleValue

	if WobbleValue < 0 then
		WobbleValue = abs(WobbleValue) * 0.9 - 0.1
	Else
		WobbleValue = -abs(WobbleValue) * 0.9 + 0.1
	end if

'	debug.print WobbleValue

	if abs(WobbleValue) < 0.1 Then
		Pursuit_LSticker.z=0:Pursuit_LLights.z=0:Pursuit_LSticker1.z=0:Pursuit_LLights1.z=0
		Pursuit_LSticker.x=0:Pursuit_LLights.x=0:Pursuit_LSticker1.x=0:Pursuit_LLights1.x=0
		tWobblePL.Enabled = false
	end If
end Sub

'VR Coin In Slot Animation.

Dim MoveCoin:MoveCoin=6
Sub VRCoinTimer_timer()
		VR_Coin.Y=VR_Coin.Y-MoveCoin	  
		VR_Coin.Z=VR_Coin.Z-MoveCoin/2
		VR_Coin.RotX=VR_Coin.RotX+MoveCoin
	If VR_Coin.y<=2250 then 
		VR_Coin.visible = false
		VR_Coin.Y=2385
		VR_Coin.Z = -20
		VRCointimer.enabled = false
    End if
End Sub

Sub VRCoinTimer2_timer()
		VR_Coin2.Y=VR_Coin2.Y-MoveCoin	  
		VR_Coin2.Z=VR_Coin2.Z-MoveCoin/2
		VR_Coin2.RotX=VR_Coin2.RotX+MoveCoin
	If VR_Coin2.y<=2250 then 
		VR_Coin2.visible = false
		VR_Coin2.Y=2385
		VR_Coin2.Z = -20
		VRCoinTimer2.enabled = false
    End if
End Sub

'VR Coindoor and Manual Animation.

Sub CoindoorTimer() 
	If DCO = True Then
		For Each CDA in CoindoorStuff:CDA.RotY = CDA.RotY + DoorMove:Next	' This is the main coin door animation command.  It moves all parts together in an ungrouped collection.
		If CoinDoor.RotY <= -130 then DCO = False:DoorMove=1 :MA = true		' -130 is the door angle, set that here.
		If CoinDoor.RotY >= 0 then DCO = False:DoorMove=-1: DoorReady = true
	End If
	If MA = True Then
		VR_Instructions.objrotx = VR_Instructions.objrotx + ManualMove
		VR_Binder.objrotx = VR_Binder.objrotx + ManualMove
		if VR_Instructions.objrotx > 164 Then MA = false: ManualMove = -1: DoorReady = true ' 164 is the instruction manual angle, set that here.
		if VR_Instructions.objrotx < 1 Then MA = false: ManualMove = 1 :PlaySound "CoindoorClose":PlaySound "CoindoorClose":PlaySound "CoindoorClose": DCO = true
	End If
End Sub


'******************************************************
' 					LUT
'******************************************************


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
		Case 0: LUTBox.text = "Fleep Natural Dark 1"
		Case 1: LUTBox.text = "Fleep Natural Dark 2"
		Case 2: LUTBox.text = "Fleep Warm Dark"
		Case 3: LUTBox.text = "Fleep Warm Bright"
		Case 4: LUTBox.text = "Fleep Warm Vivid Soft"
		Case 5: LUTBox.text = "Fleep Warm Vivid Hard"
		Case 6: LUTBox.text = "Skitso Natural and Balanced"
		Case 7: LUTBox.text = "Skitso Natural High Contrast"
		Case 8: LUTBox.text = "3rdaxis Referenced THX Standard"
		Case 9: LUTBox.text = "CalleV Punchy Brightness and Contrast"
		Case 10: LUTBox.text = "HauntFreaks Desaturated"
  		Case 11: LUTBox.text = "Tomate washed out"
        Case 12: LUTBox.text = "VPW original 1on1"
        Case 13: LUTBox.text = "bassgeige"
        Case 14: LUTBox.text = "blacklight"
        Case 15: LUTBox.text = "B&W Comic Book"
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

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "JDLUT.txt",True)
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
	If Not FileObj.FileExists(UserDirectory & "JDLUT.txt") then
		LUTset=12
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "JDLUT.txt")
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
