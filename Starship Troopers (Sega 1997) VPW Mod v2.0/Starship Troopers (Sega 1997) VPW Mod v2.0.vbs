'   _____ __                  __    _                        
'  / ___// /_____ ___________/ /_  (_)___                    
'  \__ \/ __/ __ `/ ___/ ___/ __ \/ / __ \                   
' ___/ / /_/ /_/ / /  (__  ) / / / / /_/ /                   
'/____/\__/\__,_/_/__/____/_/ /_/_/ .___/                    
'                /_  __/________ /_/__  ____  ___  __________
'                 / / / ___/ __ \/ __ \/ __ \/ _ \/ ___/ ___/
'                / / / /  / /_/ / /_/ / /_/ /  __/ /  (__  ) 
'               /_/ /_/   \____/\____/ .___/\___/_/  /____/  
'                                   /_/                                  
'*******************************************************
'SEGA PINBALL INC
'STARSHIP TROOPERS 1997
'*******************************************************
' Design by:	Joe Balcer, Joe Kaminkow
' Art by:		Morgan Weistling
' Music by:		Brian Schmidt
' Sound by:		Brian Schmidt
' Software by:	Neil Falconer, Orin Day
'*******************************************************

'*******************************************************
' Recreated for Visual Pinball by Knorr and Clark Kent
'
' Updated by VPW
'   Eighties8: Complete rescale and nFozzy physics rebuild, Fleep sounds
'   TastyWasps: VR room integration
'	iaakki: Physics, lighting, and animation support
'   Aubrel: Lighting adjustments
'   Sixtoe: Table level and target animation fixes
'   apophis: Dynamic shadows, some visual, sound, and physics support
'   leojreimroc and HauntFreaks: VR backglass
'*******************************************************

'*******************************************************
' Special Thanks to:
' JPSalas, Arngrim, Vinthar
' Toxie, Fuzzel and the VPX Dev Team
'*******************************************************


'*******************************************************
' V1.0		First Release
'*******************************************************
' V1.1 		fixed left kicker
'			added missing post
'			slowed down orbit
'*******************************************************
' V2.0		VPW Mod
'			See changelog below
'*******************************************************
' 001 e8 - Rescaled the whole project to match the current understanding of physics
' 002 iaakki - Rescaled ramp heights, brainbug position and animations fixed, pf physics reworked, light bulb ball reflections fixed (sub addBallReflections), GI bulbs fixed
' 003 e8 - Refactored scale from 2.0 to 1.9
' 004 e8 - Added nfozzy flipper physics
' 005 e8 - Added nfozzy dampening physics code. Applied new physics material settings and added rubbers and primatives to playfield
' 006 e8 - Refactored scale to 20.25 and rescaled graphics to match, added latest slings implementation w correction functions
' 007 e8 - Updated slings to nfozzy and added correction scripts.Added zCol mat to ramps, Adjusted pops settings.
' 008 e8 - Fixed: slingshot sounds, autoplunger, sling geometries, Logo ball, subway drop height
' 009 e8 - Fixed: Brain bug position, flipper strength, pf friction, fixed drop targets Z, aligned ramps
' 010/14 e8 - Updated WarriorBug positions in script. Fixed ball block from subway wall under target area
' 015 e8 - Updated WB script values to match new scale base 222 steps. Synced WB origins to same x/y base. Realigned left ramp inner post/sleeve to match overlay. 
' 016 e8 - Replaced Wall022 at corner of WB target with rubber post.
' 018 e8 - Fixed wall heights around ramps and warrior bug
' 019 Aubrel - Lights_BlurU, General_Illumination, FlasherCaps, Lights, Lights_Blur, flasherside layers
' 020 e8 - Fixed rubber post sizes broken since scaled-7 
' 021 e8 - Fixed sleeve positions and sizes around right side drops.
' 022 e8 - Fixed PFGI alignment, lowered flipper strength from 3000 to 2850 helps flipper tricks?
' 023 e8 - Fixed PFGI alignment again, rescaled playfield mesh to further precision 1.8485 (18.485) to fix PFGI alignment. Rescaled GIFL1 to match PF mesh
' 024 e8 - Added n/r physics to small flipper
' 025 e8 - Fixed subway floors and wall heights, added block above tanker beetle scoop, lowered flipper strength to 2500
' 026 e8 - More tweaks to subway to tighten geometries, removed * from layer names, fixed WP imageswap, replaced post with sleeve at left side DT bank
' 027-29 Aubrel - lighting adjustments to account for scaling
' 030 Aubrel - fixed side flashers hard edges on sidewalls. Added scripting to scale flasher effects
' 031 e8 - Added sub to enable blue "bugball" Const BugBall = 1
' 032 e8 - Reset glass height back to 400 to try fix cabinent POV
' 033 e8 - Added LockBarKey to control right mini flipper
' 034 e8 - Added Fleep Sounds, fixed position of PFGI bulbs!
' 035 e8 - Added Shooter Groove to help with autoplunge alignment (Thx Uncle_Paulie). Adjusted plunger release speed. Replaced more stock sounds with fleep.
' 036 e8 - Fixed spinner Z values! added Fleep SoundSpinner for ramp spinners.Added small flipper fleep sound and collide
' 037 e8 - Added WB & BB switches to Fleep Targets. Added all zCol Posts & Sleeves to Fleep Rubbers 
' 038 e8 - Changed PF friction from .25 to .15 (thanks Apophis)
' 039 e8 - Increased difficulty 20->50, flipper strength 2250->3125, flipper length 146.25->147.5, mini flip strength++ Reset target falloffs to 0, adjusted kicker actions
' 041 e8 - Added Toys Visibility option and Hopper bug alignment option (need to remove bolt from prim)
' 042 e8 - Removed the hex_bolt that sits atop the hopper bug (vpx export->blender import->blender export->vpx insert prim
' 043 Sixtoe - Fixed target hit threshold and prim position, fixed right outlane issue, fixed metal fixture prim wrong position, fixed plunger lane guide size, fixed broken target animation and timers
' 044 e8 - Enabled plastic ramp sounds (no fleep equivalent). Fixed brainbug wall height to prevent ball locks
' 045 TastyWasps - Added hybrid minimal VR Room integration.
' 046 iaakki - VR ramp material tweaks and z-fighting fixes. All changes are around UpdateGi sub..
' 047 TastyWasps - Added Sphere VR Room as default.  Thanks RajoJoey!
' 048 apophis - Added dynamic shadows. Added kick to ball if on top of brainbug when it raises. Updated ball image an ball scratches. Updated environmental image and emission scale. Adjusted flipper DL with GI state. 
' 049 apophis - Added flipper mod (thanks tomate!)
' RC1 apophis - Reworked ramp rolling sounds. Sound effect file cleanup. Locked all objects.
' RC2 TastyWasps - VR Backglass material and disable lighting settings tweaked. BrainBugCage normal map removed.  AO disabled by default.
' RC3 TastyWasps - VR flippers animated. VR Cabinet shiny factor reduced. Platform for sphere adjusted.
' RC4 apophis -  minor sleeve position adjustment near scoop. Set Wall014 physics material to MetalFO. Reduced WireRamp_Stop volume. Fixed top two bumpers height scale. Top comment block cleanup.
' RC5 apophis - Added inlane slowdown code. Reduced BumperSoundFactor to 3 and VolumeDial to 0.5. Updated desktop background default image (thanks PSD). Updated kicker angles (thanks tomate). Added some kicker randomness.
' v2.0 Release


Option Explicit
Randomize


' --------------------- USER OPTIONS ---------------------

'///////////////////////-----LUT CONTROL-----///////////////////////
' To change the brightness of the table, you can adjust the LUT by holding down
' the left magnasave button and then pressing the right magnasave button.

'///////////////////////-----General Sound Options-----///////////////////////
'// VolumeDial:
'// VolumeDial is the actual global volume multiplier for the mechanical sounds.
'// Values smaller than 1 will decrease mechanical sounds volume.
'// Recommended values should be no greater than 1.
Const VolumeDial = 0.5
Const BallRollVolume = 0.5		    'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 0.4		    'Level of ramp rolling volume. Value between 0 and 1

'///////////////////////-----Toys Visibility-----///////////////////////
'// Toys Visibility:
'// Can help with visibility of targets on the playfield
Const HideToys = 0 					'Set to 1 to hide ALL toys over playfield
Const HideHopper = 0 				'Set to 1 to hide the left outlane green Hopper
Const PapaHopper = 0 				'Set to 1 to rotate the Hopper to the Papa Pinball Position to make the left bank target area more visible


'///////////////////////-----Shadow Options-----///////////////////////
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's

'///////////////////////-----Other Options-----///////////////////////
Const BugBall = 0 					'0=Normal Ball, 1=Enable the Blue Bug Ball
Const FlipperMod = 0 				'0=Normal Flippers, 1=Flippers with blue rubbers

' ------------------ END OF USER OPTIONS --------------------



If FlipperMod=1 Then
	leftflipperP.image = "flipperL_mod_off"
	rightflipperP.image = "flipperR_mod_off"
Else
	leftflipperP.image = "flipperL_off"
	rightflipperP.image = "flipperR_off"
End If

If HideToys = 1 then 
	toys_hopper001.visible = 0
	toys_hopper.visible = 0
	canon_L.visible = 0
	canon_R.visible = 0
	Tanker.visible = 0
	Ship_center.visible = 0
	Ship_side.visible = 0
	canon_baseL.visible = 0
	canon_baseR.visible = 0
	Hopper_bracket.visible = 0
end if
if HideHopper = 1 then
	toys_hopper001.visible = 0
	toys_hopper.visible = 0
	Hopper_bracket.visible = 0
end if
if PapaHopper = 1 and HideToys = 0 then
	toys_hopper001.visible = 1
	toys_hopper.visible = 0
end if

'----- VR Room Auto-Detect -----
Dim VR_Room_Choice, VR_Obj			

VR_Room_Choice = 1		' 1 = Star Sphere Room, 2 = Minimal Room			  

If RenderingMode = 2 Then
	If VR_Room_Choice = 1 Then
		For Each VR_Obj in Sphere: VR_Obj.Visible = 1 : Next
		For Each VR_Obj in VR_Minimal : VR_Obj.Visible = 0 : Next
	Else
		For Each VR_Obj in Sphere: VR_Obj.Visible = 0 : Next
		For Each VR_Obj in VR_Minimal : VR_Obj.Visible = 1 : Next
	End If
Else
	For Each VR_Obj in Sphere: VR_Obj.Visible = 0 : Next
	For Each VR_Obj in VR_Minimal : VR_Obj.Visible = 0 : Next
End If

Dim VarHidden, UseVPMColoredDMD
If Table1.ShowDT = true then
    UseVPMColoredDMD = true
    VarHidden = 1
else
    UseVPMColoredDMD = False
    VarHidden = 0
end if

If BugBall=1 Then Table1.BallDecalMode=True else Table1.BallDecalMode=False


Const BallMass = 1
Const BallSize = 50


On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the Controller.vbs file in order to run this table (installed with the VPX package in the scripts folder)"
On Error Goto 0

LoadVPM "01560000", "sega.vbs", 3.57

Const UseSolenoids = 15
Const UseLamps = 0
Const UseSync = 0
Const HandleMech = 0

'' special fastflips
'Const cSingleLFlip = 0
'Const cSingleRFlip = 0

' Standard Sounds
Const SSolenoidOn = "fx_solenoidon"
Const SSolenoidOff = "fx_solenoidoff"
Const SCoin = ""



'***ROM***ROM***ROM***ROM***

Const cGameName = "startrp2"
'***************************

Dim bsTrough, mBug, bsSuperVUK, bsLeftVUK, FlippersEnabled, mLeftMagnet, mRightMagnet


Set GICallback = GetRef("UpdateGI")


'************
' Table Init 
'************

Sub Table1_Init
    vpmInit Me
    With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
        .SplashInfoLine = "StarShip Troopers - Sega 1997" & vbNewLine & "VPX by Knorr and Clark Kent"
        .Games(cGameName).Settings.Value("rol") = 1 'set it to 1 to rotate the DMD to the left
        .HandleKeyboard = 0
        .ShowTitle = 0
        .ShowDMDOnly = 1
        .ShowFrame = 0
        .HandleMechanics = 0
        .Hidden = VarHidden
        '.SetDisplayPosition 0, 0, GetPlayerHWnd 'uncomment this line if you don't see the DMD
        On Error Resume Next
        .Run GetPlayerHWnd
        If Err Then MsgBox Err.Description
        On Error Goto 0
'        .Switch(22) = 1 'close coin door
'        .Switch(24) = 1 'and keep it close
		' Main Timer init
		PinMAMETimer.Interval = PinMAMEInterval
		PinMAMETimer.Enabled = 1
		RealTime.Enabled = 1
		' Nudging
		vpmNudge.TiltSwitch = 56
		vpmNudge.Sensitivity = 2
		vpmNudge.TiltObj = Array(bumper1, bumper2, bumper3, Leftslingshot, Rightslingshot)
	End With

    Set bsTrough = New cvpmBallStack
    With bsTrough
        .InitSw 0, 15, 14, 13, 12, 0, 0, 0
        .InitKick BallRelease, 80, 11
        '.InitExitSnd SoundFX("fx_ballrelease", DOFContactors), SoundFX("fx_solenoid", DOFContactors)
        .Balls = 4
        .IsTrough = 1
    End With


    Set bsSuperVUK = New cvpmBallStack
    With bsSuperVUK
        .InitSw 0, 46, 0, 0, 0, 0, 0, 0
        .InitKick sw46, 182, 72 '42
        '.InitExitSnd SoundFX("fx_supervuk", DOFContactors), SoundFX("fx_solenoid", DOFContactors)
        .KickZ = 1.15
		.KickAngleVar = 5
		.KickForceVar = 3
    End With


    Set bsLeftVUK = New cvpmSaucer
    With bsLeftVUK
        .InitKicker sw45, 45, 131, 15, 15
        .InitExitVariance 2, 1
        '.InitSounds "fx_safehousehit", "fx_solenoid", SoundFX("fx_leftvuk",DOFContactors)
        .CreateEvents "bsLeftVUK", sw45
    End With


    Set mLeftMagnet = New cvpmMagnet
    With mLeftMagnet
        .InitMagnet LeftMagnet, 30 
        .GrabCenter = 1
		.Solenoid = 5
        .CreateEvents "mLeftMagnet"
    End With

    Set mRightMagnet = New cvpmMagnet
    With mRightMagnet
        .InitMagnet RightMagnet, 30
        .GrabCenter = 1
		.Solenoid = 6
        .CreateEvents "mRightMagnet"
    End With

'e8
    Set mBug = New cvpmMyMech
	With mBug
	' In VPX 10.7+ should use "vpmMechFourStepSol" which happens to be the same as setting both StepSol and TwoDirSol
		'.MType=vpmMechFourStepSol+vpmMechStopEnd+vpmMechLinear+vpmMechFast
		.MType=vpmMechStepSol+vpmMechTwoDirSol+vpmMechStopEnd+vpmMechLinear+vpmMechFast
		.Sol1=17
		.Length=600 '150 'Time duration for mech trip
		.Steps=222 '222 '120 'primitive trans -110 to +10 'scaled prim trans -110 to +93.5 + 18.5 = 222
		.AddSw 34,0,1 'Add switch 34, start 0 end 1
		.AddSw 33,221,222 '33,119,120
		.Callback=GetRef("UpdateWarriorBug")
		.Start
	End With

 	sw38.IsDropped=1
 	sw38a.IsDropped=1


	for each xx in BWalls:xx.isDropped = 1:Next


'**********************
'***DesktopMode Init***
'**********************

If Table1.ShowDT = False then
'Cabinet mode
		KorpusFs_inside.visible = true
		f125.rotx = -22 : f125.height = 425
		f126.rotx = -22 : f126.height = 425
		f127.rotx = -22 : f127.height = 425
		f128.rotx = -22 : f128.height = 425
	Else
'Desktop mode
		KorpusFS_inside.visible = false
		f125.rotx = -7.45 : f125.height = 296
		f126.rotx = -7.45 : f126.height = 296
		f127.rotx = -7.45 : f127.height = 296
		f128.rotx = -7.45 : f128.height = 296
	End if
 
End Sub


'**********************
'***Ball Release***
'**********************

Sub BallRelease_UnHit(): MakeBugBalls : RandomSoundBallRelease BallRelease : End Sub


'**********************
'***Bugballs***
'**********************

Sub MakeBugBalls()
	If BugBall = 1 Then
		dim b
		for each b in getballs 
			b.Image="brainbug_nrm"
			b.FrontDecal = "Scratches"
		next
	End If
End Sub


Sub table1_Paused:Controller.Pause = 1:End Sub
Sub table1_unPaused:Controller.Pause = 0:End Sub
Sub table1_exit():Controller.Pause = False:Controller.Stop:End Sub


Sub TextboxTimer_Timer()
'	TextBox003.Text = WarriorBugAssembly004.transZ
'	TextBox003.Text = materialamount
'	TextBox001.Text = GIFL1.opacity
'	TextBox003.Text = plunger.position
'	TextBox003.Text = fadestepb

'	MassboxL.text = Leftflipper.Mass
'	MassboxR.text = Rightflipper.Mass
End Sub




'************************************
' Game timer for real time updates
'************************************

Sub RealTime_Timer
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows
    RollingUpdate
    For each BrainBugStuff in BrainBugGroup: BrainBugStuff.TransY = BrainBugFlipper.CurrentAngle * 2:next

    GateP_plungerlane.rotx = Gate_plungerlane.currentangle +25
    GateP_RightRamp.rotx = Gate_rightramp.currentangle +25
    sw25p.rotx = sw25spinner.currentangle
    sw52p.rotx = sw52spinner.currentangle
    sw53p.rotx = sw53spinner.currentangle


    LeftFlipperP.objrotz = LeftFlipper.CurrentAngle
    RightFlipperP.objrotz = RightFlipper.CurrentAngle
    RightFlipperPS.objrotz = RightFlipperS.CurrentAngle
    FlipperLShadow.RotZ = LeftFlipper.CurrentAngle
    FlipperRShadow.RotZ = RightFlipper.CurrentAngle
    FlipperRsShadow.RotZ = RightFlipperS.CurrentAngle

	PlungerP.TransZ = Plunger.position-((plunger.position-4)*3)

'	*****FlipperScript****

	if fadeb = 0 and fadestepB > 0 then fadestepB = fadestepB -fadespeeddown
	if fadeb = 1 And FadeStepB < 999 then fadestepB = fadestepB +fadespeedup
	MaterialStepB = FadestepB/1000
	UpdateMaterial "FlasherCapsLitBlue",0,1,1,1,materialstepB,materialstepB,materialstepB,RGB(255,255,255),0,0,False,True,0,0,0,0

	if fader = 0 and fadestepR > 0 then fadestepR = fadestepR -fadespeeddown
	if fader = 1 And FadeStepR < 999 then fadestepR = fadestepR +fadespeedup
	MaterialStepR = FadestepR/1000
	UpdateMaterial "FlasherCapsLitRed",0,1,1,1,materialstepR,materialstepR,materialstepR,RGB(255,255,255),0,0,False,True,0,0,0,0

	if fadey = 0 and fadestepY > 0 then fadestepY = fadestepY -fadespeeddown
	if fadey = 1 And FadeStepY < 999 then fadestepY = fadestepY +fadespeedup
	MaterialStepY = FadestepY/1000
	UpdateMaterial "FlasherCapsLitYellow",0,1,1,1,materialstepY,materialstepY,materialstepY,RGB(255,255,255),0,0,False,True,0,0,0,0

	if fadeG = 0 and fadestepG > 0 then fadestepG = fadestepG -fadespeeddown
	if fadeG = 1 And FadeStepG < 999 then fadestepG = fadestepG +fadespeedup
	MaterialStepG = FadestepG/1000
	UpdateMaterial "FlasherCapsLitGreen",0,1,1,1,materialstepG,materialstepG,materialstepG,RGB(255,255,255),0,0,False,True,0,0,0,0
End Sub



'**********
' Keys
'**********
Dim BIPL : BIPL=0

Sub table1_KeyDown(ByVal Keycode)

    If KeyCode = MechanicalTilt Then vpmTimer.PulseSw vpmNudge.TiltSwitch: Exit Sub: End if

	If KeyCode = PlungerKey Then
		Plunger.Pullback
		SoundPlungerPull()
		TimerPlunger.Enabled = True
		TimerPlunger2.Enabled = False
	End If

    If keycode = LeftMagnaSave Then bLutActive = True

    If KeyCode = RightMagnaSave OR KeyCode = LockbarKey Then 
		Controller.Switch(88) = 1
		If FlippersEnabled Then SolRFlipperS True
		If bLutActive Then NextLUT:End If
        FlipperActivate RightFlipperS, RFPress2 'small flipper
		RightFlipperButtonMagna.X = RightFlipperButtonMagna.X - 10
	End if

	If keycode = LeftFlipperKey Then
		FlipperActivate LeftFlipper, LFPress
		LeftFlipperButton.X = LeftFlipperButton.X + 10
	End If

	If keycode = RightFlipperKey Then
		FlipperActivate RightFlipper, RFPress
		RightFlipperButton.X = RightFlipperButton.X - 10
	End If

	'*********************************
	'Keydown Sounds
	'*********************************
	If keycode = LeftTiltKey Then Nudge 90, 5:SoundNudgeLeft()
	If keycode = RightTiltKey Then Nudge 270, 5:SoundNudgeRight()
	If keycode = CenterTiltKey Then Nudge 0, 3:SoundNudgeCenter()

	If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
	End If

	if keycode=StartGameKey then soundStartButton()

	If vpmKeyDown(keycode) Then Exit Sub

End Sub

Sub table1_KeyUp(ByVal Keycode)

    If keycode = LeftMagnaSave Then bLutActive = False

    If KeyCode = RightMagnaSave OR KeyCode = LockbarKey Then 
		SolRFlipperS False
		Controller.Switch(88) = 0
        FlipperDeActivate RightFlipperS, RFPress2
		RightFlipperButtonMagna.X = RightFlipperButtonMagna.X + 10
	End If

	If keycode = LeftFlipperKey Then 
		FlipperDeActivate LeftFlipper, LFPress
		LeftFlipperButton.X = LeftFlipperButton.X - 10
	End If

	If keycode = RightFlipperKey Then
		FlipperDeActivate RightFlipper, RFPress
		RightFlipperButton.X = RightFlipperButton.X + 10
	End If

	'*********************************
	'KeyUp Sounds
	'*********************************
	If KeyCode = PlungerKey Then
		Plunger.Fire
		TimerPlunger.Enabled = False
		TimerPlunger2.Enabled = True
		plungerP.Y = 0
		If BIPL = 1 Then
				SoundPlungerReleaseBall() 'Plunger release sound when there is a ball in shooter lane
		Else
				SoundPlungerReleaseNoBall() 'Plunger release sound when there is no ball in shooter lane
		End If
	End If

	If vpmKeyUp(keycode) Then Exit Sub

End Sub

'****************************
'   Plunger Animations for VR
'****************************
Sub TimerPlunger_Timer
  If plungerP.Y < 100 then
  		plungerP.Y = plungerP.Y + 5
  End If
End Sub

Sub TimerPlunger2_Timer
  plungerP.Y = 0 + (5* Plunger.Position) -20
End Sub

'*********
'   LUT
'*********

Dim bLutActive, LUTImage
Sub LoadLUT
    bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "")Then LUTImage = x Else LUTImage = 0
    UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

'Sub NextLUT:LUTImage = (LUTImage + 1)MOD 9:UpdateLUT:SaveLUT:End Sub
Sub NextLUT:LUTImage = (LUTImage + 1)MOD 5:UpdateLUT:SaveLUT:End Sub

Sub UpdateLUT
    Select Case LutImage
        Case 0:table1.ColorGradeImage = "LUT0":FlashLut004.visible = 0:FlashLut000.visible = 1:FlashLut000.timerenabled = 1
        Case 1:table1.ColorGradeImage = "LUT1":FlashLut000.visible = 0:FlashLut001.visible = 1:FlashLut001.timerenabled = 1
        Case 2:table1.ColorGradeImage = "LUT2":FlashLut001.visible = 0:FlashLut002.visible = 1:FlashLut002.timerenabled = 1
        Case 3:table1.ColorGradeImage = "LUT3":FlashLut002.visible = 0:FlashLut003.visible = 1:FlashLut003.timerenabled = 1
        Case 4:table1.ColorGradeImage = "LUT4":FlashLut003.visible = 0:FlashLut004.visible = 1:FlashLut004.timerenabled = 1
    End Select
End Sub

Sub FlashLut000_Timer(): me.visible = 0:me.timerenabled = 0:End Sub
Sub FlashLut001_Timer(): me.visible = 0:me.timerenabled = 0:End Sub
Sub FlashLut002_Timer(): me.visible = 0:me.timerenabled = 0:End Sub
Sub FlashLut003_Timer(): me.visible = 0:me.timerenabled = 0:End Sub
Sub FlashLut004_Timer(): me.visible = 0:me.timerenabled = 0:End Sub


''*********
''Solenoids
''*********
'
SolCallback(1) = "SolRelease"
SolCallback(2) = "SolAutolaunch"
'SolCallback(3) = "bsLeftvuk.SolOut"
SolCallback(3) = "SolLeftVuk"
SolCallback(4) = "bsSupervuk.SolOut"
SolCallback(5) = "SolLeftMagnet"
SolCallback(6) = "SolRightMagnet"
SolCallBack(7) = "SolBrainBug"
SolCallBack(8)= "SolKnocker"
SolCallBack(15)= "FlippersEnabled="
SolCallBack(23) = "SetLamp 123," 	'flash brainbug
SolCallBack(25) = "Sol25" 	'Flash Red
SolCallBack(26) = "Sol26"	'Flash Yellow
SolCallBack(27) = "Sol27"	'Flash Green
SolCallBack(28) = "Sol28"	'Flash Blue
SolCallBack(29) = "SetLamp 129," 'F5 WarBugSled Flasher X4
SolCallBack(30) = "SetLamp 130," 'F6 Left Ramp Flasher X4
SolCallBack(31) = "SetLamp 131," 'F7 Right Ramp Flasher X4
SolCallBack(32) = "SetLamp 132," 'F8 Pop Bumpers Flasher X2


Sub sol25(enabled): If enabled then FlashBlinkingRed:Else FlashoffRed:End if:End Sub
Sub sol26(enabled): If enabled then FlashBlinkingYellow:Else FlashoffYellow:End if:End Sub
Sub sol27(enabled): If enabled then FlashBlinkingGreen:Else FlashoffGreen:End if:End Sub
Sub sol28(enabled): If enabled then FlashBlinkingBlue:Else FlashoffBlue:End if:End Sub


Sub SolKnocker(Enabled) : If enabled Then KnockerSolenoid : End If : End Sub 'Add knocker position object




'*************
'BALL RELEASE
'*************
Sub SolRelease(Enabled)
    If Enabled Then
        If bsTrough.Balls > 0 Then bsTrough.ExitSol_On
    End If
End Sub


Sub SolAutoLaunch(Enabled)
	If Enabled Then
		Plunger.Autoplunger = True
		Plunger.Fire
		PlaySound SoundFX("fx_AutoPlunger", DOFContactors)
		Else
		Plunger.Autoplunger = False
	End If
 End Sub


Sub Drain_Hit:bsTrough.AddBall Me : RandomSoundDrain drain : End Sub


'leftVUK
dim KB

Sub SolLeftVuk(Enabled)
	if enabled then
		KB = True
		bsleftvuk.exitSol_on
		vuk_switch.RotX = 90
		sw45.TimerEnabled = True
	End if
End Sub

Sub sw45a_Hit(): Vuk_Switch.RotX = 80: SoundSaucerLock: End Sub
Sub sw45a_UnHit(): SoundSaucerKick 1, sw45 : End Sub

Sub sw45_Timer()
	if KB = True and vuk_kicker.TransZ < -25 then vuk_kicker.TransZ = vuk_kicker.TransZ +5
	if KB = False and vuk_kicker.TransZ > -50 then vuk_kicker.TransZ = vuk_kicker.TransZ -5
	if vuk_kicker.TransZ >= -25 then KB = False
	if KB = False And vuk_kicker.TransZ = -50 then me.TimerEnabled = False
End Sub


Sub sw46_Hit: SoundSaucerLock:  bsSuperVUK.AddBall Me : End Sub
Sub sw46_UnHit(): SoundSaucerKick 1, sw46 : MakeBugBalls : End Sub


'**********
'BRAIN BUG
'**********

Dim BrainBugStuff

Sub SolBrainBug(Enabled)
	if enabled then
		dim BOT: BOT=getballs
		dim b: for each b in BOT  'kick ball out of way if on top of brainbug
			if InRect(b.x,b.y,545,871,472,1054,633,1126,714,944) then
				b.velz = 20
				b.vely = b.vely + 5
			end if
		next
		BrainBug.Material = "Toys"
		BrainBugflipper.rotatetoend
		BrainBug.image = "brainbugup_on"
		sw38.isDropped = 0
		sw38a.isDropped = 0
		PlaySoundAT SoundFX("fx_trapdoorhigh",DOFContactors), sw46
		Controller.Switch(37) = True
		Controller.Switch(39) = True
		ShakeBrainBug
	Else
		BrainBugFlipper.rotatetostart
		BrainBug.image = "brainbugup_off"
		sw38.isDropped = 1
		sw38a.isDropped = 1
		PlaySoundAT SoundFX("fx_trapdoorlow",DOFContactors), sw46
		Controller.Switch(37) = False
		Controller.Switch(39) = False
		BrainBug.Material = "Toys_alpha"
	end if
End Sub


Sub sw38_Hit:ShakeBrainBug2:PlaySoundAtBall "fx_brainbughit":Me.TimerEnabled = 1:End Sub
Sub sw38_timer: vpmTimer.PulseSw 38:Me.TimerEnabled = 0:End Sub

'BRAINBUG_SHAKE

Dim BrainBugPos

Sub ShakeBrainBug
    BrainBugPos = 8
    ShakeBrainBugTimer.Enabled = 1
End Sub

Sub ShakeBrainBugTimer_Timer()
    For each BrainBugStuff in BrainBugGroup:BrainBugStuff.RotAndTra4 = BrainBugPos:Next
    If BrainBugPos = 0 Then ShakeBrainBugTimer.Enabled = False:Exit Sub
    If BrainBugPos < 0 Then
        BrainBugPos = ABS(BrainBugPos) - 1
    Else
        BrainBugPos = - BrainBugPos + 1
    End If
End Sub

Sub ShakeBrainBug2
    BrainBugPos = 2
    ShakeBrainBugTimer2.Enabled = 1
End Sub

Sub ShakeBrainBugTimer2_Timer()
    For each BrainBugStuff in BrainBugGroup:BrainBugStuff.RotAndTra2 = BrainBugPos:Next
    If BrainBugPos = 0 Then ShakeBrainBugTimer2.Enabled = False:Exit Sub
    If BrainBugPos < 0 Then
        BrainBugPos = ABS(BrainBugPos) - 1
    Else
        BrainBugPos = - BrainBugPos + 1
    End If
End Sub





'**********************
'Warrior Bug Animation
'**********************

Dim BugWalls, xx
BugWalls = Array (BWall1,BWall2,BWall3,BWall4,BWall5,BWall6,BWall7,BWall8,BWall9,BWall10,BWall11,BWall12,BWall13,BWall14,BWall15,BWall16,BWall17,BWall18,BWall19,BWall20,BWall21,BWall22,BWall23,BWall24)

'e8
Sub UpdateWarriorBug(NewPos, aSpeed, LastPos)
	dim Position, LastPosition, WarriorBugStuff
	For each WarriorBugStuff in WarriorBugGroup: WarriorBugStuff.TransZ = NewPos-111:next '-110 -1 difference to starting point

	'Position = NewPos * 23 / 120 ' There are 23 targets + this one
	'LastPosition = LastPos * 23 / 120 'If this is distance -110 to +10, new distance -110 to +93.5 + 18.5 = 222
    Position = NewPos * 23 / 222 '120 steps in original (from -110 to 10)
	LastPosition = LastPos * 23 / 222

	BugWalls(LastPosition).IsDropped=1
	BugWalls(LastPosition).collidable = 0
	BugWalls(Position).IsDropped=0
	BugWalls(Position).collidable = 1
End Sub


Sub BWalls_Hit(idx):vpmTimer.PulseSw 35:ShakeTarget:End Sub 'fx_switch

'e8
Sub ShakeTarget
    'TargetPos = 4	'8
    TargetPos = 7.36 ' 1.85 x 4
    ShakeTargetTimer.Enabled = 1
End Sub

Dim TargetPos

'e8
Sub ShakeTargetTimer_Timer()
    WarriorBugAssembly003.RotAndTra4 = TargetPos
    If TargetPos = 0 Then ShakeTargetTimer.Enabled = False:Exit Sub
    If TargetPos < 0 Then
        TargetPos = ABS(TargetPos) - 1
    Else
        TargetPos = - TargetPos + 1
    End If
End Sub

Sub sw10_Hit:vpmTimer.PulseSw 10: vpmTimer.AddTimer 1800, "SuperVukAddBall'":Me.Enabled = 0:Me.TimerEnabled = 1: End Sub

Sub sw10_Timer: Me.DestroyBall:Me.TimerEnabled = 0:Me.Enabled = 1:End Sub

Sub SuperVukAddBall()
	bsSuperVuk.AddBall 1
End Sub





'********************************************
'DJRobX's Warrior Bug Stepper Supporting Code
'********************************************

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



Sub SolLeftMagnet(Enabled):if enabled then PlaySoundAt "fx_magnet", Leftmagnet:Else:StopSound "fx_magnet":End if:End Sub
Sub SolRightMagnet(Enabled):if enabled then PlaySoundAt "fx_magnet", Rightmagnet:Else:StopSound "fx_magnet":End if:End Sub


'**************
' Flipper Subs
'**************


SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"
'SolCallback(14) = "SolRFlipperS" ' mini flipper


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

Sub SolRFlipperS(Enabled)
    If Enabled Then
		RandomSoundReflipUpRight RightFlipperS
        RFs.fire 'RightFlipper.RotateToEnd
    Else
		RandomSoundFlipperDownRight RightFlipperS
        RightFlipperS.RotateToStart
    End If
End Sub

'************
' SlingShots
'************


Dim RStep, Lstep

Sub Rightslingshot_Slingshot
	RS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotRight Sling1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransY = -17
    RStep = 0
    Rightslingshot.TimerEnabled = 1
    vpmTimer.PulseSw 59
End Sub

Sub Rightslingshot_Timer
    Select Case RStep
        Case 1:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransY = -5
        Case 2:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransY = 0:Rightslingshot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub Leftslingshot_Slingshot
	LS.VelocityCorrect(ActiveBall)
    RandomSoundSlingshotLeft Sling2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransY = -12
    LStep = 0
    Leftslingshot.TimerEnabled = 1
    vpmTimer.PulseSw 62
End Sub

Sub Leftslingshot_Timer
    Select Case LStep
        Case 1:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransY = -5
        Case 2:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransY = 0:Leftslingshot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub




'*********
' Switches
'*********

' Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 49 : RandomSoundBumperTop Bumper1 : End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 51 : RandomSoundBumperMiddle Bumper2 : End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 50 : RandomSoundBumperBottom Bumper3 : End Sub



'rollover 'was all fx_switch
Sub sw16_Hit:Controller.Switch(16) = 1:sw16p.RotX = 70: BIPL=1 : End Sub
Sub sw16_UnHit:Controller.Switch(16) = 0:sw16p.RotX = 90: BIPL=0 : End Sub

Sub sw41_Hit:Controller.Switch(41) = 1:sw41p.RotX = 70:End Sub
Sub sw41_UnHit:Controller.Switch(41) = 0:sw41p.RotX = 90:End Sub

Sub sw42_Hit:Controller.Switch(42) = 1:sw42p.RotX = 70:End Sub
Sub sw42_UnHit:Controller.Switch(42) = 0:sw42p.RotX = 90:End Sub

Sub sw43_Hit:Controller.Switch(43) = 1:sw43p.RotX = 70:End Sub
Sub sw43_UnHit:Controller.Switch(43) = 0:sw43p.RotX = 90:End Sub

Sub sw57_Hit:Controller.Switch(57) = 1:sw57p.RotX = 70:End Sub
Sub sw57_UnHit:Controller.Switch(57) = 0:sw57p.RotX = 90:End Sub

Sub sw58_Hit:Controller.Switch(58) = 1:sw58p.RotX = 70: activeball.vely=activeball.vely*0.8: activeball.angmomz=0: End Sub
Sub sw58_UnHit:Controller.Switch(58) = 0:sw58p.RotX = 90:End Sub

Sub sw61_Hit:Controller.Switch(61) = 1:sw61p.RotX = 70: activeball.vely=activeball.vely*0.8: activeball.angmomz=0: End Sub
Sub sw61_UnHit:Controller.Switch(61) = 0:sw61p.RotX = 90:End Sub

Sub sw60_Hit:Controller.Switch(60) = 1:sw60p.RotX = 70:End Sub
Sub sw60_UnHit:Controller.Switch(60) = 0:sw60p.RotX = 90:End Sub

'orbit/optos
Sub sw47_Hit:Controller.Switch(47)= 1:End Sub
Sub sw47_UnHit:Controller.Switch(47) = 0:End Sub

Sub sw48_Hit:Controller.Switch(48)= 1:End Sub
Sub sw48_UnHit:Controller.Switch(48) = 0:End Sub


'undertrough

'skillshot
Sub sw9_Hit:Controller.Switch(9)= 1:RandomSoundHole:WireRampOff:End Sub
Sub sw9_UnHit:Controller.Switch(9) = 0:End Sub

'bugexit
'Sub sw10a_Hit:RandomSoundHole:End Sub 'Use fleep? RandomSoundDelayedBallDropOnPlayfield
Sub sw10a_Hit:RandomSoundDelayedBallDropOnPlayfield(ActiveBall):End Sub 'Use fleep? RandomSoundDelayedBallDropOnPlayfield


'bumperexit
Sub sw40_Hit:Controller.Switch(40)= 1:PlaySoundAtBall "fx_tedhit":End Sub
Sub sw40_UnHit:Controller.Switch(40) = 0:End Sub


'targets 'was all fx_switch sounds. Now using targets collection sounds
Sub T17_Hit:vpmTimer.pulseSw 17: T17p.RotX = T17p.RotX +4:Me.TimerEnabled = 1:End Sub
Sub T17_Timer:T17p.RotX = 90:Me.TimerEnabled = 0:End Sub

Sub T18_Hit:vpmTimer.pulseSw 18: T18p.RotX = T18p.RotX +4:Me.TimerEnabled = 1:End Sub
Sub T18_Timer:T18p.RotX = 90:Me.TimerEnabled = 0:End Sub

Sub T19_Hit:vpmTimer.pulseSw 19: T19p.RotX = T19p.RotX +4:Me.TimerEnabled = 1:End Sub
Sub T19_Timer:T19p.RotX = 90:Me.TimerEnabled = 0:End Sub

Sub T20_Hit:vpmTimer.pulseSw 20: T20p.RotX = T20p.RotX +4:Me.TimerEnabled = 1:End Sub
Sub T20_Timer:T20p.RotX = 90:Me.TimerEnabled = 0:End Sub

Sub T21_Hit:vpmTimer.pulseSw 21: T21p.RotX = T21p.RotX +4:Me.TimerEnabled = 1:End Sub
Sub T21_Timer:T21p.RotX = 90:Me.TimerEnabled = 0:End Sub

Sub T22_Hit:vpmTimer.pulseSw 22: T22p.RotX = T22p.RotX +4:Me.TimerEnabled = 1:End Sub
Sub T22_Timer:T22p.RotX = 90:Me.TimerEnabled = 0:End Sub

Sub T23_Hit:vpmTimer.pulseSw 23: T23p.RotX = T23p.RotX +4:Me.TimerEnabled = 1:End Sub
Sub T23_Timer:T23p.RotX = 90:Me.TimerEnabled = 0:End Sub

Sub T24_Hit:vpmTimer.pulseSw 24: T24p.RotX = T24p.RotX +4:Me.TimerEnabled = 1:End Sub
Sub T24_Timer:T24p.RotX = 90:Me.TimerEnabled = 0:End Sub

Sub T27_Hit:vpmTimer.pulseSw 27: T27p.RotX = T27p.RotX +4:Me.TimerEnabled = 1:End Sub
Sub T27_Timer:T27p.RotX = 90:Me.TimerEnabled = 0:End Sub

Sub T28_Hit:vpmTimer.pulseSw 28: T28p.RotX = T28p.RotX +4:Me.TimerEnabled = 1:End Sub
Sub T28_Timer:T28p.RotX = 90:Me.TimerEnabled = 0:End Sub

Sub T29_Hit:vpmTimer.pulseSw 29: T29p.RotX = T29p.RotX +4:Me.TimerEnabled = 1:End Sub
Sub T29_Timer:T29p.RotX = 90:Me.TimerEnabled = 0:End Sub

Sub T30_Hit:vpmTimer.pulseSw 30: T30p.RotX = T30p.RotX +4:Me.TimerEnabled = 1:End Sub
Sub T30_Timer:T30p.RotX = 90:Me.TimerEnabled = 0:End Sub

Sub T31_Hit:vpmTimer.pulseSw 31: T31p.RotX = T31p.RotX +4:Me.TimerEnabled = 1:End Sub
Sub T31_Timer:T31p.RotX = 90:Me.TimerEnabled = 0:End Sub

Sub T32_Hit:vpmTimer.pulseSw 32: T32p.RotX = T32p.RotX +4:Me.TimerEnabled = 1:End Sub
Sub T32_Timer:T32p.RotX = 90:Me.TimerEnabled = 0:End Sub


Sub sw25_Hit:vpmTimer.PulseSW 25: End Sub 
Sub sw52_Hit:vpmTimer.PulseSW 52: End Sub
Sub sw53_Hit:vpmTimer.PulseSW 53: End Sub

Sub sw26_Hit:vpmTimer.PulseSW 26: WireRampOff: End Sub
Sub sw26_UnHit:WireRampOn False: End Sub




'***********
' Update GI
'***********
If RenderingMode = 2 Then	'VR
	ramp_off.material = "ramps_off_vr"
	ramp_on.material = "ramps_vr"
	ramp_off.size_x = 18.5005
	ramp_off.size_y = 18.5001
	ramp_off.size_z = 18.5001
	ramp_off.x = -0.015

	plasticsslingshots_on.size_x = 18.5001
	plasticsslingshots_on.size_y = 18.5001
	plasticsslingshots_on.size_z = 18.5001

	plasticsrest_on.size_x = 18.5001
	plasticsrest_on.size_y = 18.5001
	plasticsrest_on.size_z = 18.5001
	plasticsrest_on.y = 0.01

	plasticsset2_on.size_x = 18.5001
	plasticsset2_on.size_y = 18.5001
	plasticsset2_on.size_z = 18.5001

	plastics_trans_on.size_x = 18.5001
	plastics_trans_on.size_y = 18.5001
	plastics_trans_on.size_z = 18.5001

	plastics_bumpercaps_on.size_x = 18.5001
	plastics_bumpercaps_on.size_y = 18.5001
	plastics_bumpercaps_on.size_z = 18.5001

end if

Dim MaterialAmount, x, StepM

Sub UpdateGi(no, enabled)
	If No = False Then
		Select Case no
			Case 0
				SetLamp 200, enabled
			end select
	End if

		Select Case no
			Case 0
			if enabled then
				for StepM  =  1 to 998 Step +1:next
				for each x in targetgroup: x.image = "targets_on":next
				for each x in imageswap: x.image = Replace(x.image,"_off","_on"):next
				LeftFlipperP.blenddisablelighting = 1.5
				RightFlipperP.blenddisablelighting = 1.5
				RightFlipperPS.blenddisablelighting = 1.0
			else
				for StepM  =  998 to 1 Step -1:next
				for each x in targetgroup: x.image = "targets_off":next
				for each x in imageswap: x.image = Replace(x.image,"_on","_off"):next
				   LeftFlipperP.blenddisablelighting = 0.5
				RightFlipperP.blenddisablelighting = 0.5
				RightFlipperPS.blenddisablelighting = 0.5
			End if
	End Select
	MaterialAmount = stepm / 1000




	UpdateMaterial "plastics_textured" ,0,0,1,1,1,1,MaterialAmount,RGB(255,255,255),0,0,False,True,0,0,0,0
	If RenderingMode = 2 Then	'VR
		UpdateMaterial "ramps_vr" 			   ,0,0,1,0.7,MaterialAmount,MaterialAmount * 0.5,MaterialAmount * 0.2 ,RGB(200,200,200),0,0,False,True,0,0,0,0
	Else
		UpdateMaterial "ramps" 			   ,0,0,1,1,MaterialAmount,MaterialAmount,MaterialAmount,RGB(255,255,255),0,0,False,True,0,0,0,0
	end if
	UpdateMaterial "plastics_trans" ,0,1,1,1,MaterialAmount,MaterialAmount,MaterialAmount,RGB(255,255,255),0,0,False,True,0,0,0,0

'Wrap/Shininess/use_image/thickness/edge_brightness/edgeopacity/alpha/BaseColor/
End Sub



'*******************************************************************************************************************************************
'*******************************************************************************************************************************************
'Flasher_Strobing
'*******************************************************************************************************************************************
'The command is called from the solenoid with "FlashonRed" or "FlashblinkingRed" or "FlashOffRed" similar to the "states" of a single light
'Flashtimer 1+2 controls the blinking speed of the red flashers Lights
'Flashtimer 3+4 controls the blinking speed of the green flashers Lights
'Flashtimer 5+6 controls the blinking speed of the yellow flashers Lights
'Flashtimer 7+8 controls the blinking speed of the blue flashers Lights
'Fadespeedup and Fadespeeddown only controls the fading of the second primitive with the "lit" texture#
'FadeTimer updates the "lit" material of the flasherdomes
'*******************************************************************************************************************************************
'*******************************************************************************************************************************************

Dim bulb, fbulb, FlasherOnR, FlasherOffR, FlasherOnG, FlasherOffG, FlasherOnY, FlasherOffY, FlasherOnB, FlasherOffB
Dim FadeStepR, FadeStepG, FadeStepY, FadeStepB, materialstepR, MaterialstepG, MaterialStepY, MaterialStepB, fadespeedup, fadespeeddown
Dim FadeR, FadeG, FadeY, FadeB
FadestepR = 0
FadestepG = 0
FadestepY = 0
FadestepB = 0
Fadespeedup = 33
Fadespeeddown = 18		'high is fast, lower is slower



Sub FlashOffRed()
	FlasherOffR = True
	for each bulb in StrobeFlashersLRed
	bulb.state = 0
	for each fbulb in StrobeFlashersFRed
	fbulb.visible = False
	next
	next
End Sub

Sub FlashOffGreen()
	FlasherOffG = True
	for each bulb in StrobeFlashersLGreen
	bulb.state = 0
	for each fbulb in StrobeFlashersFGreen
	fbulb.visible = False
	next
	next
End Sub

Sub FlashOffYellow()
	FlasherOffY = True
	for each bulb in StrobeFlashersLYellow
	bulb.state = 0
	for each fbulb in StrobeFlashersFYellow
	fbulb.visible = False
	next
	next
End Sub

Sub FlashOffBlue()
	FlasherOffB = True
	for each bulb in StrobeFlashersLBlue
	bulb.state = 0
	for each fbulb in StrobeFlashersFBlue
	fbulb.visible = False
	next
	next
End Sub

'Sub FlashOn()
'	FlasherOn = True
'	for each bulb in StrobeFlashersL
'	bulb.state = 1
'	for each fbulb in StrobeFlashersF
'	fbulb.visible = True
'	next
'	next
'End Sub

Sub FlashBlinkingRed()
	FlasherOnR = False
	FlasherOffR = False
	FlasherTimer2.Enabled = True
End Sub

Sub FlashBlinkingGreen()
	FlasherOnG = False
	FlasherOffG = False
	FlasherTimer4.Enabled = True
End Sub


Sub FlashBlinkingYellow()
	FlasherOnY = False
	FlasherOffY = False
	FlasherTimer6.Enabled = True
End Sub


Sub FlashBlinkingBlue()
	FlasherOnB = False
	FlasherOffB = False
	FlasherTimer8.Enabled = True
End Sub



'FlasherStrobing off/on...

Sub FlasherTimer1_Timer()

	FadeR = 0
'	capsred.image = "CapsRed_On"
	for each fbulb in StrobeFlashersFRed
	fbulb.visible = 0
	for each bulb in StrobeFlashersLRed
	bulb.state = 0
	FlasherTimer1.Enabled = 0
	If FlasherOffR = False then FlasherTimer2.Enabled = 1
	next
	next
End Sub

Sub FlasherTimer2_Timer()

	FadeR = 1
'	CapsRed.image = "CapsRed_lit"
	for each fbulb in StrobeFlashersFRed
	fbulb.visible = 1
	for each bulb in StrobeFlashersLRed
	bulb.state = 1
	If FlasherOnR = False then FlasherTimer1.Enabled = 1
	FlasherTimer2.Enabled = 0
	next
	next
End Sub




Sub FlasherTimer3_Timer()

	FadeG = 0
'	capsgreen.image = "CapsGreen_on"
	for each fbulb in StrobeFlashersFGreen
	fbulb.visible = 0
	for each bulb in StrobeFlashersLGreen
	bulb.state = 0
	FlasherTimer3.Enabled = 0
	If FlasherOffG = False then FlasherTimer4.Enabled = 1
	next
	next
End Sub

Sub FlasherTimer4_Timer()

	FadeG = 1
'	capsgreen.image = "CapsGreen_Lit"
	for each fbulb in StrobeFlashersFGreen
	fbulb.visible = 1
	for each bulb in StrobeFlashersLGreen
	bulb.state = 1
	If FlasherOnG = False then FlasherTimer3.Enabled = 1 
	FlasherTimer4.Enabled = 0
	next
	next
End Sub




Sub FlasherTimer5_Timer()

	FadeY = 0
'	CapsYellow.image = "CapsYellow_On"
	for each fbulb in StrobeFlashersFYellow
	fbulb.visible = 0
	for each bulb in StrobeFlashersLYellow
	bulb.state = 0
	FlasherTimer5.Enabled = 0
	If FlasherOffY = False then FlasherTimer6.Enabled = 1
	next
	next
End Sub

Sub FlasherTimer6_Timer()

	FadeY = 1
'	Capsyellow.image = "CapsYellow_Lit"
	for each fbulb in StrobeFlashersFYellow
	fbulb.visible = 1
	for each bulb in StrobeFlashersLYellow
	bulb.state = 1
	If FlasherOnY = False then FlasherTimer5.Enabled = 1 
	FlasherTimer6.Enabled = 0
	next
	next
End Sub



Sub FlasherTimer7_Timer()

	FadeB = 0
'	CapsBlue.image = "capsblue_on"
	for each fbulb in StrobeFlashersFBlue
	fbulb.visible = 0
	for each bulb in StrobeFlashersLBlue
	bulb.state = 0
	FlasherTimer7.Enabled = 0
	If FlasherOffB = False then FlasherTimer8.Enabled = 1
	next
	next
End Sub

Sub FlasherTimer8_Timer()

	FadeB = 1
'	CapsBlue.image = "capsblue_lit"
	for each fbulb in StrobeFlashersFBlue
	fbulb.visible = 1
	for each bulb in StrobeFlashersLBlue
	bulb.state = 1
	If FlasherOnB = False then FlasherTimer7.Enabled = 1 
	FlasherTimer8.Enabled = 0
	next
	next
End Sub


'**********************************************
'*******FadeTimer Moved to the Gametimer*******
'**********************************************

'Sub FadeTimer_Timer()
'	if fadeb = 0 and fadestepB > 0 then fadestepB = fadestepB -fadespeeddown
'	if fadeb = 1 And FadeStepB < 999 then fadestepB = fadestepB +fadespeedup
'	MaterialStepB = FadestepB/1000
'	UpdateMaterial "flashercapslitblue",0,1,1,1,materialstepB,materialstepB,materialstepB,RGB(255,255,255),0,0,False,True,0,0,0,0
'
'	if fader = 0 and fadestepR > 0 then fadestepR = fadestepR -fadespeeddown
'	if fader = 1 And FadeStepR < 999 then fadestepR = fadestepR +fadespeedup
'	MaterialStepR = FadestepR/1000
'	UpdateMaterial "flashercapslitred",0,1,1,1,materialstepR,materialstepR,materialstepR,RGB(255,255,255),0,0,False,True,0,0,0,0
'
'	if fadey = 0 and fadestepY > 0 then fadestepY = fadestepY -fadespeeddown
'	if fadey = 1 And FadeStepY < 999 then fadestepY = fadestepY +fadespeedup
'	MaterialStepY = FadestepY/1000
'	UpdateMaterial "flashercapslityellow",0,1,1,1,materialstepY,materialstepY,materialstepY,RGB(255,255,255),0,0,False,True,0,0,0,0
'
'	if fadeG = 0 and fadestepG > 0 then fadestepG = fadestepG -fadespeeddown
'	if fadeG = 1 And FadeStepG < 999 then fadestepG = fadestepG +fadespeedup
'	MaterialStepG = FadestepG/1000
'	UpdateMaterial "flashercapslitgreen",0,1,1,1,materialstepG,materialstepG,materialstepG,RGB(255,255,255),0,0,False,True,0,0,0,0
'End Sub



'Ball reflections - iaakki
addBallReflections


sub addBallReflections
	dim rr,gg,bb
	dim origColori

	for each xx in GILamps
		origColori = round(xx.colorfull,0)

		rr = (origColori Mod 256)
		gg = (origColori \ 256) Mod 256
		bb = (origColori \ 65536) Mod 256

		xx.color = rgb(rr/4,gg/4,bb/4)
	Next

	for each xx in BallRefl
		origColori = round(xx.colorfull,0)

		rr = (origColori Mod 256)
		gg = (origColori \ 256) Mod 256
		bb = (origColori \ 65536) Mod 256

'		msgbox rr & "." & gg & "." & bb
		xx.color = rgb(rr,gg,bb)
	Next

end sub

sub removeBallReflections
	for each xx in GILamps:xx.color = RGB(0,0,0):Next
	for each xx in BallRefl:xx.color = RGB(0,0,0):Next
end sub


'**********************************************************
'     JP's Flasher Fading for VPX and Vpinmame
'       (Based on Pacdude's Fading Light System)
' This is a fast fading for the Flashers in vpinmame tables
'  just 4 steps, like in Pacdude's original script.
' Included the new Modulated flashers & Lights for WPC
'**********************************************************

Dim LampState(200), FadingState(200), FlashLevel(200)

InitLamps() ' turn off the lights and flashers and reset them to the default parameters

' vpinmame Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp)Then
        For ii = 0 To UBound(chgLamp)
			LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
            FadingState(chgLamp(ii, 0)) = chgLamp(ii, 1) + 3  'fading step
        Next
    End If
    UpdateLamps
End Sub

Sub UpdateLamps

    lampm 1, l1
    lampm 1, l1b
    flash 1, lf1
    lampm 2, l2
    lampm 2, l2b
    flash 2, lf2
    lampm 3, l3
    lampm 3, l3b
    flash 3, lf3
    lampm 4, l4
    lampm 4, l4b
    flash 4, lf4
    lampm 5, l5
    lampm 5, l5b
    flash 5, lf5
    lampm 6, l6
    lampm 6, l6b
    flash 6, lf6
    lampm 7, l7
    lampm 7, l7b
    flash 7, lf7
    lampm 8, l8
    lampm 8, l8b
    flash 8, lf8
    lampm 9, l9
    lampm 9, l9b
    flash 9, lf9
    lampm 10, l10
    lampm 10, l10b
    flash 10, lf10

	lampm 11, l11
	lampm 11, l11b
	flash 11, lf11
    lampm 12, l12
    lampm 12, l12b
    flash 12, lf12
    lampm 13, l13a
    lampm 13, l13b
    flash 13, lf13
    lampm 14, l14a
    lampm 14, l14b
    flash 14, lf14
    lampm 15, l15a
    lampm 15, l15b
    flash 15, lf15
    lampm 16, l16a
    lampm 16, l16b
    flash 16, lf16
    lampm 17, l17
    flash 17, lf17
    lampm 18, l18
    lampm 18, l18b
    flash 18, lf18
    lampm 19, l19
    flash 19, lf19
    lampm 20, l20
    lampm 20, l20b
    flash 20, lf20
    lampm 21, l21
    flash 21, lf21
    lampm 22, l22
    flash 22, lf22
    lampm 23, l23
    flash 23, lf23
    lampm 24, l24
    flash 24, lf24
    lampm 25, l25
    lampm 25, l25b
    flash 25, lf25
    lampm 26, l26
    lampm 26, l26b
    flash 26, lf26
    lampm 27, l27
    lampm 27, l27b
    flash 27, lf27

    lampm 28, l28
    lampm 28, l28b
    lamp 28, l28a
    lampm 29, l29
    lampm 29, l29b
    lamp 29, l29a
    lampm 30, l30
    lampm 30, l30b
    lamp 30, l30a

    lampm 31, l31
    flash 31, lf31
    lampm 32, l32
    lampm 32, l32b
    flash 32, lf32

    lampm 35, l35
    flashm 35, fl35
    flash 35, fl35a
    lampm 36, l36
    flashm 36, fl36
    flash 36, fl36a
    lampm 37, l37
    lampm 37, l37b
    flashm 37, lf37s
    flash 37, lf37
    lampm 38, l38
    lampm 38, l38b
    flashm 38, lf38s
    flash 38, lf38
    lampm 39, l39
    flashm 39, fl39
    flash 39, fl39a


    lampm 40, l40
    lampm 40, l40a
    lampm 40, l40b
    flashm 40, lf40
    flash 40, lf40a

    lampm 41, l41
    lampm 41, l41b
    flash 41, lf41
    lampm 42, l42
    lampm 42, l42b
    flash 42, lf42
    lampm 43, l43
    flash 43, lf43
    lampm 44, l44
    lampm 44, l44b
    flash 44, lf44
    lampm 45, l45
    lampm 45, l45b
    flash 45, lf45
    lampm 46, l46
    lampm 46, l46b
    flash 46, lf46

    lampm 47, l47
    lampm 47, l47b
    flash 47, fl47
    lampm 48, l48
    lampm 48, l48b
    flash 48, fl48
    flash 49, l49
    flash 50, l50
    flash 51, l51
    flash 52, l52
    flash 53, l53
    flash 54, l54
    flash 55, l55

    flash 57, l57
    flash 58, l58
    flash 59, l59
    flash 60, l60
    flash 61, l61
    flash 62, l62
    flash 63, l63

    flash 65, l65
    flash 66, l66
    flash 67, l67
    flash 68, l68
    flash 69, l69
    flash 70, l70
    flash 71, l71

    flash 73, l73
    flash 74, l74
    flash 75, l75
    flash 76, l76
    flash 77, l77
    flash 78, l78
    flash 79, l79

    lampm 56, l56
    lampm 56, l56b
    flash 56, lf56

    lampm 64, l64
    lampm 64, l64b
    flash 64, lf64

    lampm 72, l72
    lampm 72, l72b
    flash 72, lf72

    lampm 80, l80
    lampm 80, l80b
    flash 80, lf80

'	BrainBug_Flash
'	flash 123, f123
	lampm 123, l123a
	lampm 123, l123b

'	WarriorBug_Flash
	lampm 129, l129
	lampm 129, l129a
	lampm 129, l129b
	flashm 129, f129s
	flashm 129, lf129
	flash 129, lf129b


'	LT_Ramp_Flash
	lampm 130, l130b
	lampm 130, l130c
	lampm 130, l130d
	lampm 130, l130e
	flashm 130, f130a
	flashm 130, f130b
	flashm 130, f130c
	flash 130, f130d

'	RT_Ramp_Flash
	lampm 131, l131b
	lampm 131, l131c
	lampm 131, l131d
	lampm 131, l131e
	flashm 131, f131a
	flashm 131, f131b
	flashm 131, f131c
	flash 131, f131d

	lampm 132, l132
	flash 132, lf132

'General_Illumination
	lampm 200, Light001
	lampm 200, Light002
	lampm 200, Light003
	lampm 200, Light004
	lampm 200, Light005
	lampm 200, Light006
	lampm 200, Light007
	lampm 200, Light008
	lampm 200, Light009
	lampm 200, Light010
	lampm 200, Light011
	lampm 200, Light012
	lampm 200, Light013
	lampm 200, Light014
	lampm 200, Light015
	lampm 200, Light016
	lampm 200, Light017
	lampm 200, Light018
	lampm 200, Light019
	lampm 200, Light020
	lampm 200, Light021
	lampm 200, Light022
	lampm 200, Light023
	lampm 200, Light024
	lampm 200, Light025
	lampm 200, Light026
	lampm 200, Light027
	lampm 200, Light028
	flashm 200, GIFL1
	flashm 200, lfgi001
	flashm 200, lfgi002
	flashm 200, lfgi003
	flash 200, lfgi004

End Sub

' div lamp subs

' Normal Lamp & Flasher subs

Sub InitLamps()
    Dim x
'    LampTimer.Interval = 25 ' flasher fading speed
    LampTimer.Interval = 10 ' flasher fading speed
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

'***************************************************************
'             Supporting Ball & Sound Functions v3.0
'  includes random pitch in PlaySoundAt and PlaySoundAtBall
'***************************************************************

Dim TableWidth, TableHeight

TableWidth = Table1.width
TableHeight = Table1.height

'Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
'    Vol = Csng(BallVel(ball) ^2 / 1500)
'End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / TableWidth-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

'Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
'    Pitch = BallVel(ball) * 20
'End Function

'Function BallVel(ball) 'Calculates the ball speed
'    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2)))
'End Function

' Duplicated function in fleep
'Function AudioFade(ball) 'only on VPX 10.4 and newer
'    Dim tmp
'    tmp = ball.y * 2 / TableHeight-1
'    If tmp > 0 Then
'        AudioFade = Csng(tmp ^10)
'    Else
'        AudioFade = Csng(-((- tmp) ^10))
'    End If
'End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.06, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub


'******************************************************
'                BALL ROLLING AND DROP SOUNDS
'******************************************************

Const tnob = 4 ' total number of balls
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

'Sub RollingTimer_Timer()
Sub RollingUpdate()
	Dim BOT, b
	BOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(BOT) + 1 to tnob
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
			PlaySound ("BallRoll_" & b), - 1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))

		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
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

		' "Static" Ball Shadows
		If AmbientBallShadowOn = 0 Then
			If BOT(b).Z > 30 Then
				BallShadowA(b).height=BOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
			Else
				BallShadowA(b).height=BOT(b).z - BallSize/2 + 5
			End If
			BallShadowA(b).Y = BOT(b).Y + Ballsize/5 + offsetY
			BallShadowA(b).X = BOT(b).X + offsetX
			BallShadowA(b).visible = 1
		End If
	Next
End Sub




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
Dim RampBalls(5,2)
'x,0 = ball x,1 = ID, 2 = Protection against ending early (minimum amount of updates)

'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
' Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
' Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
Dim RampType(5)

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
			Debug.print "WireRampOn error, ball queue is full: " & vbNewLine & _
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


'Ramp triggers

Sub RampLFxstart_Hit
	WireRampOn True
End Sub

Sub RampLFxstart_UnHit
	If activeball.vely>0 Then WireRampOff
End Sub

Sub RampRFXstart_Hit
	WireRampOn True
End Sub

Sub RampRFXstart_UnHit
	If activeball.vely>0 Then WireRampOff
End Sub

Sub StopWireSoundTrigger_Hit
	WireRampOff
End Sub

Sub StopWireSoundTrigger1_Hit
	WireRampOff
End Sub

Sub StopWireSoundTrigger2_Hit
	WireRampOff
End Sub

Sub LeftRampEnd_Hit
	PlaySoundAtLevelActiveBall ("WireRamp_Stop"), Vol(ActiveBall) * 0.001
End Sub


'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************


Sub RandomSoundHole()
    Select Case Int(Rnd * 6) + 1
        Case 1:PlaySound "fx_fallinramp1", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 2:PlaySound "fx_fallinramp2", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 3:PlaySound "fx_fallinramp3", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 4:PlaySound "fx_fallinramp4", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 5:PlaySound "fx_fallinramp5", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 6:PlaySound "fx_fallinramp6", 0, 1, Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End Select
End Sub




'******************************************************
'****  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity
dim RFs : Set RFs = New FlipperPolarity

InitPolarity

Sub InitPolarity()
        dim x, a : a = Array(LF, RF, RFs)
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
        RFs.Object = RightFlipperS
        RFs.EndPoint = EndPointRpS
End Sub

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub
Sub TriggerRFs_Hit() 	: RFs.Addball activeball : End Sub
Sub TriggerRFs_UnHit() 	: RFs.PolarityCorrect activeball : End Sub


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
        
        Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : if IsEmpty(balls(x)) then set balls(x) = aBall : MakeBugBalls : exit sub :end if : Next :  End Sub

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
'                FLIPPER POLARITY AND RUBBER DAMPENER
'                        SUPPORTING FUNCTIONS 
'******************************************************

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
        FlipperTricks RightFlipperS, RFPress2, RFCount2, RFEndAngle2, RFState2
end sub

Dim LFEOSNudge, RFEOSNudge

'e8 updated to latest
Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
    Dim BOT, b

    If abs(Flipper1.currentangle) < abs(Endangle1) + 3 and EOSNudge1 <> 1 Then
        EOSNudge1 = 1
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
                    'debug.print "flippernudge!!"
                    BOT(b).velx = BOT(b).velx /1.3
                    BOT(b).vely = BOT(b).vely - 0.5
                end If
            Next
        End If
    Else 
        If abs(Flipper1.currentangle) > abs(Endangle1) + 30 then EOSNudge1 = 0
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

' Used for drop targets and stand up targets
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

'*************************************************
' End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, RFPress2, LFCount, RFCount, RFCount2
dim LFState, RFState
dim RFState2 'small flipper
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle
dim RFEndAngle2 'small flipper

EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 0.8 
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
SOSRampup = 2.5
Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.025

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle
RFEndAngle2 = RightFlipperS.endangle

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
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
        End If
End Sub


'**************************************************
'        Flipper Collision Subs
'NOTE: COpy and overwrite collision sound from original collision subs over
'RandomSoundFlipper()' below
'**************************************************'

Sub LeftFlipper_Collide(parm)
    CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
    LeftFlipperCollide parm   'This is the Fleep code
End Sub

Sub RightFlipper_Collide(parm)
    CheckLiveCatch Activeball, RightFlipper, RFCount, parm
    RightFlipperCollide parm  'This is the Fleep code
End Sub

Sub RightFlipperS_Collide(parm)
    CheckLiveCatch Activeball, RightFlipperS, RFCount2, parm
    RightFlipperCollide parm  'This is the Fleep code
End Sub


'****************************************************************************
'PHYSICS DAMPENERS
'****************************************************************************

'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
'Requires tracking ballspeed to calculate COR


Sub dPosts_Hit(idx) 
        RubbersD.dampen Activeball
End Sub

Sub dSleeves_Hit(idx) 
        SleevesD.Dampen Activeball
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
Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function
Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

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



'e8 Fleep Sounds


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
BumperSoundFactor = 3                                                                                                'volume multiplier; must not be zero
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
SpinnerSoundLevel = 0.5                                                                      'volume level; range [0, 1]

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

Sub Apron1_Hit (idx)
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

Const RelayFlashSoundLevel = 0.315                                                                        'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05                                                                        'volume level; range [0, 1];

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
'                                        End Mechanical Sounds
'/////////////////////////////////////////////////////////////////






'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

'****** INSTRUCTIONS please read ******

'****** Part A:  Table Elements ******
'
' Import the "bsrtx7" and "ballshadow" images
' Import the shadow materials file (3 sets included) (you can also export the 3 sets from this table to create the same file)
' Copy in the BallShadowA flasher set and the sets of primitives named BallShadow#, RtxBallShadow#, and RtxBall2Shadow#
'	* Count from 0 up, with at least as many objects each as there can be balls, including locked balls.  You'll get an "eval" warning if tnob is higher
'	* Warning:  If merging with another system (JP's ballrolling), you may need to check tnob math and add an extra BallShadowA# flasher (out of range error)
' Ensure you have a timer with a -1 interval that is always running
' Set plastic ramps DB to *less* than the ambient shadows (-10000) if you want to see the pf shadow through the ramp

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
'Const tnob = 4 ' total number of balls
Const lob = 0	'locked balls on start; might need some fiddling depending on how your locked balls are done
'Dim tablewidth: tablewidth = Table1.width
'Dim tableheight: tableheight = Table1.height

' *** User Options - Uncomment here or move to top for easy access by players
'----- Shadow Options -----
'Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
'Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
'									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
'									'2 = flasher image shadow, but it moves like ninuzzu's

' *** This segment goes within the RollingUpdate sub, so that if Ambient...=0 and Dynamic...=0 the entire DynamicBSUpdate sub can be skipped for max performance
' *** Change gBOT to BOT if using existing getballs code
' *** Includes lines commonly found there, for reference:
'	' stop the sound of deleted balls
'	For b = UBound(gBOT) + 1 to tnob
'		If AmbientBallShadowOn = 0 Then BallShadowA(b).visible = 0
'		...rolling(b) = False
'		...StopSound("BallRoll_" & b)
'	Next
'
' ...rolling and drop sounds...

'		If DropCount(b) < 5 Then
'			DropCount(b) = DropCount(b) + 1
'		End If
'
'		' "Static" Ball Shadows
'		If AmbientBallShadowOn = 0 Then
'			If gBOT(b).Z > 30 Then
'				BallShadowA(b).height=gBOT(b).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
'			Else
'				BallShadowA(b).height=gBOT(b).z - BallSize/2 + 5
'			End If
'			BallShadowA(b).Y = gBOT(b).Y + Ballsize/5 + offsetY
'			BallShadowA(b).X = gBOT(b).X + offsetX
'			BallShadowA(b).visible = 1
'		End If

' *** Required Functions, enable these if they are not already present elswhere in your table
Function max(a,b)
	if a > b then 
		max = a
	Else
		max = b
	end if
end Function

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
Const AmbientBSFactor 		= 0.9	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const offsetX				= 0		'Offset x position under ball	(These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY				= 0		'Offset y position under ball	 (for example 5,5 if the light is in the back left corner)

'Dynamic (Table light sources)
Const DynamicBSFactor 		= 0.95	'0 to 1, higher is darker
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness is technically most accurate for lights at z ~25 hitting a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source

' ***														***

' *** Trim or extend these to *match* the number of balls/primitives/flashers on the table!
dim objrtx1(4), objrtx2(4)
dim objBallShadow(4)
Dim OnPF(5)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4)
Dim DSSources(30), numberofsources', DSGISide(30) 'Adapted for TZ with GI left / GI right

Dim ClearSurface:ClearSurface = True		'Variable for hiding flasher shadow on wire and clear plastic ramps
									'Intention is to set this either globally or in a similar manner to RampRolling sounds

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
'		If Instr(Source.name , "Left") > 0 Then DSGISide(iii) = 0 Else DSGISide(iii) = 1	'Adapted for TZ with GI left / GI right
		iii = iii + 1
	Next
	numberofsources = iii
end sub


Sub BallOnPlayfieldNow(yeh, num)		'Only update certain things once, save some cycles
	If yeh Then
		OnPF(num) = True
'		debug.print "Back on PF"
		UpdateMaterial objBallShadow(num).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(num).size_x = 5
		objBallShadow(num).size_y = 4.5
		objBallShadow(num).visible = 1
		BallShadowA(num).visible = 0
	Else
		OnPF(num) = False
'		debug.print "Leaving PF"
		If Not ClearSurface Then
			BallShadowA(num).visible = 1
			objBallShadow(num).visible = 0
		Else
			objBallShadow(num).visible = 1
		End If
	End If
End Sub

Sub DynamicBSUpdate
	Dim falloff: falloff = 150 'Max distance to light sources, can be changed dynamically if you have a reason
	Dim ShadowOpacity1, ShadowOpacity2 
	Dim s, LSd, iii
	Dim dist1, dist2, src1, src2
	Dim gBOT: gBOT=getballs	'Uncomment if you're deleting balls - Don't do it! #SaveTheBalls

	'Hide shadow of deleted balls
	For s = UBound(gBOT) + 1 to tnob - 1
		objrtx1(s).visible = 0
		objrtx2(s).visible = 0
		objBallShadow(s).visible = 0
		BallShadowA(s).visible = 0
	Next

	If UBound(gBOT) < lob Then Exit Sub		'No balls in play, exit

'The Magic happens now
	For s = lob to UBound(gBOT)

' *** Normal "ambient light" ball shadow
	'Layered from top to bottom. If you had an upper pf at for example 80 units and ramps even above that, your segments would be z>110; z<=110 And z>100; z<=100 And z>30; z<=30 And z>20; Else invisible

		If AmbientBallShadowOn = 1 Then			'Primitive shadow on playfield, flasher shadow in ramps
			If gBOT(s).Z > 30 Then							'The flasher follows the ball up ramps while the primitive is on the pf
				If OnPF(s) Then BallOnPlayfieldNow False, s		'One-time update

				If Not ClearSurface Then							'Don't show this shadow on plastic or wire ramps (table-wide variable, for now)
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + BallSize/5
					BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				Else
					If gBOT(s).X < tablewidth/2 Then
						objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
					Else
						objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
					End If
					objBallShadow(s).Y = gBOT(s).Y + BallSize/10 + offsetY
					objBallShadow(s).size_x = 5 * ((gBOT(s).Z+BallSize)/80)			'Shadow gets larger and more diffuse as it moves up
					objBallShadow(s).size_y = 4.5 * ((gBOT(s).Z+BallSize)/80)
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(30/(gBOT(s).Z)),RGB(0,0,0),0,0,False,True,0,0,0,0
				End If

			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf, primitive only
				If Not OnPF(s) Then BallOnPlayfieldNow True, s

				If gBOT(s).X < tablewidth/2 Then
					objBallShadow(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					objBallShadow(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				objBallShadow(s).Y = gBOT(s).Y + offsetY
'				objBallShadow(s).Z = gBOT(s).Z + s/1000 + 0.04		'Uncomment (and adjust If/Elseif height logic) if you want the primitive shadow on an upper/split pf

			Else												'Under pf, no shadows
				objBallShadow(s).visible = 0
				BallShadowA(s).visible = 0
			end if

		Elseif AmbientBallShadowOn = 2 Then		'Flasher shadow everywhere
			If gBOT(s).Z > 30 Then							'In a ramp
				If Not ClearSurface Then							'Don't show this shadow on plastic or wire ramps (table-wide variable, for now)
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + BallSize/5
					BallShadowA(s).height=gBOT(s).z - BallSize/4		'This is technically 1/4 of the ball "above" the ramp, but it keeps it from clipping the ramp
				Else
					BallShadowA(s).X = gBOT(s).X + offsetX
					BallShadowA(s).Y = gBOT(s).Y + offsetY
				End If
			Elseif gBOT(s).Z <= 30 And gBOT(s).Z > 20 Then	'On pf
				BallShadowA(s).visible = 1
				If gBOT(s).X < tablewidth/2 Then
					BallShadowA(s).X = ((gBOT(s).X) - (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX + 5
				Else
					BallShadowA(s).X = ((gBOT(s).X) + (Ballsize/10) + ((gBOT(s).X - (tablewidth/2))/(Ballsize/AmbientMovement))) + offsetX - 5
				End If
				BallShadowA(s).Y = gBOT(s).Y + offsetY
				BallShadowA(s).height=0.1
			Else											'Under pf
				BallShadowA(s).visible = 0
			End If
		End If

' *** Dynamic shadows
		If DynamicBallShadowsOn Then
			If gBOT(s).Z < 30 And gBOT(s).X < 850 Then	'Parameters for where the shadows can show, here they are not visible above the table (no upper pf) or in the plunger lane
				dist1 = falloff:
				dist2 = falloff
				For iii = 0 to numberofsources - 1 ' Search the 2 nearest influencing lights
					LSd = Distance(gBOT(s).x, gBOT(s).y, DSSources(iii)(0), DSSources(iii)(1)) 'Calculating the Linear distance to the Source
					If LSd < falloff Then
'					If LSd < dist2 And ((DSGISide(iii) = 0 And Lampz.State(100)>0) Or (DSGISide(iii) = 1 And Lampz.State(104)>0)) Then	'Adapted for TZ with GI left / GI right
						dist2 = dist1
						dist1 = LSd
						src2 = src1
						src1 = iii
					End If
				Next
				ShadowOpacity1 = 0
				If dist1 < falloff Then
					objrtx1(s).visible = 1 : objrtx1(s).X = gBOT(s).X : objrtx1(s).Y = gBOT(s).Y
					'objrtx1(s).Z = gBOT(s).Z - 25 + s/1000 + 0.01 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx1(s).rotz = AnglePP(DSSources(src1)(0), DSSources(src1)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity1 = 1 - dist1 / falloff
					objrtx1(s).size_y = Wideness * ShadowOpacity1 + Thinness
					UpdateMaterial objrtx1(s).material,1,0,0,0,0,0,ShadowOpacity1*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx1(s).visible = 0
				End If
				ShadowOpacity2 = 0
				If dist2 < falloff Then
					objrtx2(s).visible = 1 : objrtx2(s).X = gBOT(s).X : objrtx2(s).Y = gBOT(s).Y + offsetY
					'objrtx2(s).Z = gBOT(s).Z - 25 + s/1000 + 0.02 'Uncomment if you want to add shadows to an upper/lower pf
					objrtx2(s).rotz = AnglePP(DSSources(src2)(0), DSSources(src2)(1), gBOT(s).X, gBOT(s).Y) + 90
					ShadowOpacity2 = 1 - dist2 / falloff
					objrtx2(s).size_y = Wideness * ShadowOpacity2 + Thinness
					UpdateMaterial objrtx2(s).material,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					objrtx2(s).visible = 0
				End If
				If AmbientBallShadowOn = 1 Then
					'Fades the ambient shadow (primitive only) when it's close to a light
					UpdateMaterial objBallShadow(s).material,1,0,0,0,0,0,AmbientBSFactor*(1 - max(ShadowOpacity1, ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
				Else
					BallShadowA(s).Opacity = 100 * AmbientBSFactor * (1 - max(ShadowOpacity1, ShadowOpacity2))
				End If
			Else 'Hide dynamic shadows everywhere else, just in case
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub

'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************
