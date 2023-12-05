Option Explicit
Randomize
Dim OptionReset
'OptionReset = 1  'Uncomment to reset to default options in case of error OR keep all changes temporary

' Version 1.1.0: initial release
' Version 1.0.1: fixed DOF light for shooterlane
' Version 2.0.0: rebuild from scratch

'***********************************************************************************
'****            		 Constants and global variables						****
'***********************************************************************************
const UseSolenoids	= True
const UseLamps		= False
const UseGI			= False 								'Only WPC games have special GI circuit.
Const SCoin        = "Coin"

Dim OutlaneLPos: OutlaneLPos = 0

Dim Controller, cController, cGameName, bsTrough, bsTopKicker, bsRightKicker, bsHoleKicker, dtU
Dim Tilted, FlipperEnabled, GI_On, GameOn, Hidden, i
Dim DesktopMode: DesktopMode = Victory.ShowDT
Dim DebugSwitch: DebugSwitch = False						'Don't even think about it, only Sindbad is allowed to do that ...
cGameName="victory"


On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

LoadVPM"01001100","sys80.VBS",3.42
OptionsLoad


Sub Victory_Init
    vpmInit Me
    With Controller
		.GameName=cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine="Victory by Sindbad"
		.HandleKeyboard=0
		.ShowTitle=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.HandleMechanics=0
		.Hidden=Hidden
		.SolMask(0) = 0
	On Error Resume Next
		.Run GetPlayerHWnd
	End With
	If Err Then MsgBox Err.Description
	On Error Goto 0
	
	vpmTimer.AddTimer 2000, "Controller.SolMask(0)=&Hffffffff'"
	PinMAMETimer.Interval=PinMAMEInterval
    PinMAMETimer.Enabled=1

	' Trough handler
	Set bsTrough=New cvpmBallStack
	With bsTrough
		.InitSw swOuthole,swTrough,0,0,0,0,0,0
		.InitKick BallRelease, 90, 7
		.Balls=3
	End With

	' Top Kicker
	Set bsTopKicker=new cvpmBallStack
	With bsTopKicker
		.InitSaucer TopKicker, swTopKicker, 0, 40
	End With
		
	' Right Kicker
	Set bsRightKicker=new cvpmBallStack
	With bsRightKicker
		.InitSaucer RightKicker, swRightKicker, 0, 25
		bsRightKicker.KickAngleVar = 1.5
		bsRightKicker.KickForceVar = 8
	End With

	' Hole Kicker
	Set bsHoleKicker=new cvpmBallStack
	With bsHoleKicker
		.InitSaucer HoleKicker, swHoleKicker, 155, 12
	End With

	vpmNudge.TiltSwitch = swTiltMe
	vpmNudge.Sensitivity = 5
	vpmNudge.TiltObj = Array(SlingL, SlingR)

	Tilted = False
	GI_On = False
	GameOn = False
	FlipperEnabled = False
	Flippers_Init
	Backdrop_Init
	Slingshots_Init
	Outlanes_Init
	Kickers_Init
'	Rolling_Init
	Illumination_Init
	SetGIOff
End Sub

Sub Victory_Paused:Controller.Pause = True:End Sub
Sub Victory_UnPaused:Controller.Pause = False:End Sub
Sub Victory_Exit:Controller.Pause = False:Controller.Stop:End Sub 



'***********************************************************************************
'****               	  Keyboard (Input) Handling								****
'***********************************************************************************
Sub Victory_KeyDown(ByVal keycode)
	' If keycode = 38 Then Toggle_Debug_Lights
	' If keycode = 33 Then Debug_Flashers_Flag = 1
	If keycode = PlungerKey Then Plunger.Pullback: PlaySoundAt "PlungerPull", Plunger
	If keycode = LeftTiltKey Then LeftNudge 80, 1.2, 20:PlaySound SoundFX("NudgeL",0)
    If keycode = RightTiltKey Then RightNudge 280, 1.2, 20:PlaySound SoundFX("NudgeR",0)
	If keycode = CenterTiltKey Then CenterNudge 0, 1.6, 25:PlaySound SoundFX("NudgeC",0)
   	If vpmKeyDown(keycode) Then Exit Sub 
End Sub

Sub Victory_KeyUp(ByVal keycode)
	If keycode = PlungerKey Then Plunger.Fire: PlaySoundAt "Plunger", Plunger
 	If vpmKeyUp(keycode) Then Exit Sub
End Sub



'***********************************************************************************
'****				nudging based on Noah's nudge test table					****
'*********************************************************************************** 
Dim LeftNudgeEffect, RightNudgeEffect, CenterNudgeEffect
 
Sub LeftNudge(angle, strength, delay)
    vpmNudge.DoNudge angle, (strength * (delay-LeftNudgeEffect) / delay) + RightNudgeEffect / delay
    LeftNudgeEffect = delay
    RightNudgeEffect = 0
    RightNudgeTimer.Enabled = 0
    LeftNudgeTimer.Interval = delay
    LeftNudgeTimer.Enabled = 1
	If Controller.Switch(swTiltMe) = True Then Tilted = True
End Sub
 
Sub RightNudge(angle, strength, delay)
    vpmNudge.DoNudge angle, (strength * (delay-RightNudgeEffect) / delay) + LeftNudgeEffect / delay
    RightNudgeEffect = delay
    LeftNudgeEffect = 0
    LeftNudgeTimer.Enabled = 0
    RightNudgeTimer.Interval = delay
    RightNudgeTimer.Enabled = 1
	If Controller.Switch(swTiltMe) = True Then Tilted = True
End Sub
 
Sub CenterNudge(angle, strength, delay)
    vpmNudge.DoNudge angle, strength * (delay-CenterNudgeEffect) / delay
    CenterNudgeEffect = delay
    CenterNudgeTimer.Interval = delay
    CenterNudgeTimer.Enabled = 1
	If Controller.Switch(swTiltMe) = True Then Tilted = True
End Sub
 
 Sub LeftNudgeTimer_Timer()
     LeftNudgeEffect = LeftNudgeEffect-1
     If LeftNudgeEffect = 0 then Me.Enabled = 0
 End Sub
 
 Sub RightNudgeTimer_Timer()
     RightNudgeEffect = RightNudgeEffect-1
     If RightNudgeEffect = 0 then Me.Enabled = 0
 End Sub
 
 Sub CenterNudgeTimer_Timer()
     CenterNudgeEffect = CenterNudgeEffect-1
     If CenterNudgeEffect = 0 then Me.Enabled = 0
 End Sub


 
'***********************************************************************************
'****				        	Lamp reference      				  		  	****
'***********************************************************************************
Const lBallRelease					= 2
Const lShootAgain					= 3
Const lLeft50k1						= 5
Const lLeft50k2						= 6
Const lLeft50k3						= 7
Const lLeft50k4						= 8
Const lExtraBall					= 9
Const lLeftOutlane					= 10
Const lRightOutlane					= 11
Const lTopKicker					= 12
Const l100k							= 13
Const lCenterLamps1					= 14
Const lTopRamp						= 15
Const lCenterLamps2					= 16
Const lCar1							= 17
Const lCar2							= 18
Const lCar3							= 19
Const lCar4							= 20
Const lCar5							= 21
Const lCar6							= 22
Const lCar7							= 23
Const lCar8							= 24
Const lSpotTarget1					= 25
Const lSpotTarget2					= 26
Const lSpotTarget3					= 27
Const lSpotTarget4					= 28
Const lSpotTarget5					= 29
Const lSpotTarget6					= 30
Const lMultiplier1X					= 31
Const lMultiplier2X					= 32
Const lMultiplier4X					= 33
Const lMultiplier8X					= 34
Const lLeftSpinnerDouble			= 35
Const lDropTarget1					= 36
Const lDropTarget2					= 37
Const lDropTarget3					= 38
Const lDropTarget4					= 39
Const lDropTarget50k				= 40
Const lDropTarget100k				= 41
Const lDropTarget200k				= 42
Const lDropTargetSpecial			= 43
Const lRightExtraBall				= 44
Const lTopRace						= 45
Const lRightBottomRace				= 46
Const lRightSpinner					= 47
Const lLeftSpinner					= 51
' start of virtual lamps
Const lLightshowL1					= 60
Const lLightshowL2					= 61
Const lLightshowL3					= 62
Const lLightshowL4					= 63
Const lLightshowL5					= 64
Const lLightshowR1					= 65
Const lLightshowR2					= 66
Const lLightshowR3					= 67
Const lLightshowR4					= 68
Const lLightshowR5					= 69
Const lLeft100k						= 70
Const lRight100k					= 71
Const lFlashersTop					= 196 '(virtual lamp for top flashers)
Const lFlashersLeft					= 197 '(virtual lamp for left flashers)
Const lFlashersRight				= 198 '(virtual lamp for right flashers)
Const lGI							= 199 '(virtual lamp for GI_Flasher)

'***********************************************************************************
'****				        	Switch reference      				  		  	****
'***********************************************************************************
Const swTopRightRampRollunder		= 30
Const swSpotTarget1					= 40
Const swSpotTarget4					= 41
Const swDropTarget1					= 42
Const swTopLeftRampRollunder		= 43
Const swTopRollover					= 44
Const swLeftOutlane					= 45
Const swSling						= 46
Const swSpotTarget2					= 50
Const swSpotTarget5					= 51
Const swDropTarget2					= 52
Const swLeftSpinner					= 53
Const swRightSpinner				= 54
Const swLeftReturnlane				= 55
Const swTrough						= 56
Const swTiltMe						= 57
Const swSpotTarget3					= 60
Const swSpotTarget6					= 61
Const swDropTarget3					= 62
Const swLeftRollUnder				= 63
Const swTopKicker					= 64
Const swRightReturnlane				= 65
Const swOuthole						= 66
Const swLeftSpotTarget				= 70
Const swRightSpotTarget				= 71
Const swDropTarget4					= 72
Const swLeftTrackExitRollunder		= 73
Const swRightKicker					= 74
Const swRightOutlane				= 75
Const swHoleKicker					= 76


'***********************************************************************************
'****				        	Solenoid reference      				    	****
'***********************************************************************************
const sHoleKicker					= 1
const sDropTargetsReset				= 2
const sFlashersTop	 				= 3
const sFlashersRight 				= 4
const sRightKicker	 				= 5
const sFlashersLeft	 				= 7
const sKnocker		 				= 8
const sOuthole		 				= 9
const sFlippersEnable				= 10


'***********************************************************************************
'****				       		DOF reference	 	     				    	****
'***********************************************************************************
const dShooterLane	 				= 201	' Shooterlane
const dLeftSlingshot 				= 211	' Left Slingshot
const dRightSlingshot 				= 212	' Right Slingshot



'***********************************************************************************
'****								Knocker               						****
'*********************************************************************************** 
SolCallback(sKnocker) = "DoKnocker"

Sub DoKnocker(enabled)
	If enabled Then PlaySound SoundFX("Knocker",DOFKnocker)
End Sub



'***********************************************************************************
'****						  Drains and Kickers           						****
'***********************************************************************************
Dim CurrentBall
SolCallback(sOuthole)= "DoOuthole"
SolCallback(sRightKicker)= "DoRightKicker"
SolCallback(sHoleKicker)= "DoHoleKicker"

Sub DoOuthole(enabled)
	If enabled = True Then
		bsTrough.EntrySol_On
		If Tilted = True then SetGIon: Tilted = False
	End If
End Sub

Sub DoBallRelease
	If Tilted = True then SetGIon: Tilted = False
	PlaySoundAt SoundFX("BallRelease",DOFContactors), BallRelease
	BallRelease.TimerEnabled = True
End Sub
 
Sub BallRelease_Timer()
 	Me.TimerEnabled = False
	bsTrough.ExitSol_On
End Sub

Sub TopKicker_Hit(): PlaySoundAtVol "KickerEnter", TopKicker, 0.15: bsTopKicker.AddBall Me: End Sub
Sub RightKicker_Hit(): PlaySoundAtVol "KickerEnter", RightKicker, 0.15: bsRightKicker.AddBall Me: End Sub
Sub HoleKicker_Hit(): PlaySoundAtVol "KickerEnter", HoleKicker, 0.15: bsHoleKicker.AddBall Me: End Sub

Sub DoTopKicker
	PlaySoundAt SoundFX("PopperBall",DOFContactors), TopKicker
	bsTopKicker.ExitSol_On
End Sub

Sub DoRightKicker(enabled)
	If enabled = True Then 
		PlaySoundAt SoundFX("PopperBall",DOFContactors), RightKicker
		bsRightKicker.ExitSol_On
	End If
End Sub

Sub DoHoleKicker(enabled)
	If enabled = True Then 
		PlaySoundAt SoundFX("PopperBall",DOFContactors), HoleKicker
		bsHoleKicker.ExitSol_On
	End If
End Sub

Sub Drain_Hit()
	PlaySoundAt "DrainLong", Drain
	bsTrough.AddBall Me
 	If Tilted = True then SetGIon: Tilted = False
End Sub

Sub Kickers_Init
End Sub



'***********************************************************************************
'****							Flippers                 						****
'***********************************************************************************

SolCallback(sLRFlipper) = "SolFlip FlipperLR, FlipperUR," ' Right Flipper
SolCallback(sLLFlipper) = "SolFlip FlipperLL, FlipperUL," ' Left Flipper
SolCallback(sFlippersEnable) = "DoEnable"

Sub SolFlip(aFlip1, aFlip2, aEnabled) 
	If aEnabled Then 
		PlaySoundAt SoundFX("FlipperUp",DOFFlippers), FlipperLL
		aFlip1.RotateToEnd: aFlip2.RotateToEnd
	Else
		PlaySoundAt SoundFX("FlipperDown",DOFFlippers), FlipperLR
		aFlip1.RotateToStart: aFlip2.RotateToStart
	End If
End Sub

Sub DoEnable(enabled)
 	If enabled = True Then 
		vpmNudge.SolGameOn enabled	
		If GI_On = False Then SetGIOn
		SLingL.Disabled = 0
		SLingR.Disabled = 0
		FlipperEnabled = True
	Else
		SLingL.Disabled = 1
		SLingR.Disabled = 1
		FlipperEnabled = False
		If Tilted = True Then SetGIOff
	End If
End Sub

Sub Flippers_Init
	FlipperLL.Visible = 0:FlipperLL.Enabled = 1:FlipperLLP.Visible = 1
	FlipperLR.Visible = 0:FlipperLR.Enabled = 1:FlipperLRP.Visible = 1
	FlipperUL.Visible = 0:FlipperUL.Enabled = 1:FlipperULP.Visible = 1
	FlipperUR.Visible = 0:FlipperUR.Enabled = 1:FlipperURP.Visible = 1
End Sub

'***********************************************************************************
'****							Slingshot and walls        						****
'*********************************************************************************** 
Dim SLPos,SRPos

Sub SlingL_Slingshot()
	If FlipperEnabled = False Then Exit Sub
	LSling1.Visible = 0:LSling2.Visible = 0:LSling3.Visible = 0: LSling4.Visible = 1: LSling.TransZ = -27
	vpmTimer.PulseSw(swSling): PlaySoundAt SoundFX ("SlingshotLeft",DOFContactors), LSling:DOF dLeftSlingshot, 2
	SLPos = 0: Me.TimerEnabled = 1
	LightshowChangeSide
End Sub 

Sub SlingL_Timer
    Select Case SLPos
        Case 2:	LSling1.Visible = 0:LSling2.Visible = 0:LSling3.Visible = 1: LSling4.Visible = 0: LSling.TransZ = -17
		Case 3:	LSling1.Visible = 0:LSling2.Visible = 1:LSling3.Visible = 0: LSling4.Visible = 0: LSling.TransZ = -8
		Case 4:	LSling1.Visible = 1:LSling2.Visible = 0:LSling3.Visible = 0: LSling4.Visible = 0: LSling.TransZ = 0 :Me.TimerEnabled = 0
    End Select
    SLPos = SLPos + 1
End Sub

Sub SlingR_Slingshot()
	If FlipperEnabled = False Then Exit Sub
	RSling1.Visible = 0:RSling2.Visible = 0:RSling3.Visible = 0: RSling4.Visible = 1: RSling.TransZ = -27
	vpmTimer.PulseSw(swSling): PlaySoundAt SoundFX ("SlingshotRight",DOFContactors), RSling:DOF dRightSlingshot, 2
	SRPos = 0: Me.TimerEnabled = 1
	LightshowChangeSide
End Sub 

Sub SlingR_Timer
    Select Case SRPos
        Case 2:	RSling1.Visible = 0:RSling2.Visible = 0:RSling3.Visible = 1: RSling4.Visible = 0: RSling.TransZ = -17
		Case 3:	RSling1.Visible = 0:RSling2.Visible = 1:RSling3.Visible = 0: RSling4.Visible = 0: RSling.TransZ = -8
		Case 4:	RSling1.Visible = 1:RSling2.Visible = 0:RSling3.Visible = 0: RSling4.Visible = 0: RSling.TransZ = 0 :Me.TimerEnabled = 0
    End Select
    SRPos = SRPos + 1
End Sub

Sub Slingshots_Init
	LSling1.Visible = 1:LSling2.Visible = 0:LSling3.Visible = 0: LSling4.Visible = 0: LSling.TransZ = 0
	RSling1.Visible = 1:RSling2.Visible = 0:RSling3.Visible = 0: RSling4.Visible = 0: RSling.TransZ = 0
End Sub

Sub Outlanes_Init
	Dim OutlaneLPosArray: OutlaneLPosArray = Array(1144, 1157.5, 1169)
	OutlaneLP1.Y = OutlaneLPosArray(OutlaneLPos): OutlaneLP2.Y = OutlaneLPosArray(OutlaneLPos): OutlaneLP3.Y = OutlaneLPosArray(OutlaneLPos)
	Select Case OutlaneLPos
		Case 0:RubberOutlaneL1.Collidable = 1: RubberOutlaneL2.Collidable = 0: RubberOutlaneL3.Collidable = 0
		Case 1:RubberOutlaneL1.Collidable = 0: RubberOutlaneL2.Collidable = 1: RubberOutlaneL3.Collidable = 0
		Case 2:RubberOutlaneL1.Collidable = 0: RubberOutlaneL2.Collidable = 0: RubberOutlaneL3.Collidable = 1
	End Select
End Sub



'***********************************************************************************
'****								  Drop Targets								****
'*********************************************************************************** 
Set dtU=New cvpmDropTarget
	dtU.InitDrop Array(sw42,sw52,sw62,sw72),Array(swDropTarget1,swDropTarget2,swDropTarget3,swDropTarget4)
	dtU.InitSnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

SolCallBack(sDropTargetsReset) = "dtU.SolDropUp"

Sub sw42_Dropped:dtU.Hit 1 :End Sub  
Sub sw52_Dropped:dtU.Hit 2 :End Sub  
Sub sw62_Dropped:dtU.Hit 3 :End Sub  
Sub sw72_Dropped:dtU.Hit 4 :End Sub  



'***********************************************************************************
'****						      Targets                 						****
'*********************************************************************************** 
Sub sw70_Hit:vpmTimer.PulseSw(swLeftSpotTarget):PlaySound SoundFX("",DOFTargets):End Sub
Sub sw71_Hit:vpmTimer.PulseSw(swRightSpotTarget):PlaySound SoundFX("",DOFTargets):End Sub
Sub sw40_Hit:vpmTimer.PulseSw(swSpotTarget1):PlaySound SoundFX("",DOFTargets):End Sub
Sub sw41_Hit:vpmTimer.PulseSw(swSpotTarget4):PlaySound SoundFX("",DOFTargets):End Sub
Sub sw50_Hit:vpmTimer.PulseSw(swSpotTarget2):PlaySound SoundFX("",DOFTargets):End Sub
Sub sw51_Hit:vpmTimer.PulseSw(swSpotTarget5):PlaySound SoundFX("",DOFTargets):End Sub
Sub sw60_Hit:vpmTimer.PulseSw(swSpotTarget3):PlaySound SoundFX("",DOFTargets):End Sub
Sub sw61_Hit:vpmTimer.PulseSw(swSpotTarget6):PlaySound SoundFX("",DOFTargets):End Sub


'***********************************************************************************
'****						   Rollovers and triggers      						****
'*********************************************************************************** 
Sub sw44_Hit():Controller.Switch(swTopRollover) = 1:sw44p.Visible = 0:End Sub
Sub sw44_UnHit():Controller.Switch(swTopRollover) = 0:sw44p.Visible = 1:End Sub
Sub sw45_Hit():Controller.Switch(swLeftOutlane) = 1:sw45p.Visible = 0:End Sub
Sub sw45_UnHit():Controller.Switch(swLeftOutlane) = 0:sw45p.Visible = 1:End Sub
Sub sw55_Hit():Controller.Switch(swLeftReturnlane) = 1:sw55p.Visible = 0:End Sub
Sub sw55_UnHit():Controller.Switch(swLeftReturnlane) = 0:sw55p.Visible = 1:End Sub
Sub sw65_Hit():Controller.Switch(swRightReturnlane) = 1:sw65p.Visible = 0:End Sub
Sub sw65_UnHit():Controller.Switch(swRightReturnlane) = 0:sw65p.Visible = 1:End Sub
Sub sw75_Hit():Controller.Switch(swRightOutlane) = 1:sw75p.Visible = 0:End Sub
Sub sw75_UnHit():Controller.Switch(swRightOutlane) = 0:sw75p.Visible = 1:End Sub
Sub TopKickerTrigger_Hit():TopKickerTriggerP.Visible = 0:End Sub
Sub TopKickerTrigger_UnHit():TopKickerTriggerP.Visible = 1:End Sub
Sub RightKickerTrigger_Hit():RightKickerTriggerP.Visible = 0:End Sub
Sub RightKickerTrigger_UnHit():RightKickerTriggerP.Visible = 1:End Sub
Sub swShooterLane_Hit():Set CurrentBall = ActiveBall:DOF dShooterLane, 1:End Sub
Sub swShooterLane_UnHit():DOF dShooterLane, 0:End Sub



'***********************************************************************************
'****						   Spinners and gates	      						****
'*********************************************************************************** 
Sub sw30_Hit():vpmTimer.PulseSw (swTopRightRampRollunder):End Sub
Sub sw43_Hit():vpmTimer.PulseSw (swTopLeftRampRollunder):End Sub
Sub sw53_Spin():PlaySoundAtVol "fx_Spinner", sw53, 0.15:vpmTimer.PulseSw (swLeftSpinner):End Sub
Sub sw54_Spin():PlaySoundAtVol "fx_Spinner", sw54, 0.15:vpmTimer.PulseSw (swRightSpinner):End Sub
Sub sw63_Hit():vpmTimer.PulseSw (swLeftRollUnder):End Sub
Sub sw73_Hit():vpmTimer.PulseSw (swLeftTrackExitRollunder):End Sub


'***********************************************************************************
'****				Init siderails and stuff for desktop mode 					****
'***********************************************************************************
Sub Backdrop_Init
	Dim iw
	If DesktopMode = True Then
		RampLT.Visible = 1:RampRT.Visible = 1
		For each iw in cBackdropLights: iw.Visible = True:Next
		LedsTimer.Interval = 33
		LedsTimer.Enabled = 1
	Else
		RampLT.Visible = 0:RampRT.Visible = 0
		For each iw in cBackdropLights: iw.Visible = False:Next
	End If
End Sub



'***********************************************************************************
'**** 							 Light routines						        	****
'***********************************************************************************
Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200), Debug_Lights_Flag, Debug_Flashers_Flag
Debug_Lights_Flag = 0: Debug_Flashers_Flag = 0

SolCallback(sFlashersTop) = "SolFlash lFlashersTop," ' Top Flashers
SolCallback(sFlashersRight) = "SolFlash lFlashersRight," ' Right Flashers
SolCallback(sFlashersLeft) = "SolFlash lFlashersLeft," ' Left Flashers

Sub SolFlash(nr, enabled)
	If enabled Then
		LampState(nr) = 1:FadingLevel(nr) = 5
	Else
		LampState(nr) = 0:FadingLevel(nr) = 4
	End If
End Sub

Sub Illumination_Init()
    Dim x
    For x = 0 to 200
        LampState(x) = 0        ' current light state, independent of the fading level. 0 is off and 1 is on
        FadingLevel(x) = 4      ' used to track the fading state
        FlashSpeedUp(x) = 0.4   ' faster speed when turning on the flasher
        FlashSpeedDown(x) = 0.2 ' slower speed when turning off the flasher
        FlashMax(x) = 1         ' the maximum value when on, usually 1
        FlashMin(x) = 0         ' the minimum value when off, usually 0
        FlashLevel(x) = 0       ' the intensity of the flashers, usually from 0 to 1
    Next
	FlashSpeedUp(lGI) = 0.05: FlashSpeedDown(lGI) = 0.075 ' set fading speed for GI Flashers
	LampTimer.Interval = 5 		'lamp fading speed
	LampTimer.Enabled = 1
End Sub

Sub Toggle_Debug_Lights
    Dim x
	If Debug_Lights_Flag = 0 Then
		Debug_Lights_Flag = 1: Debug_Flashers_Flag = 0
		LightshowClr 0: LightshowClr 1: LightshowTimer.Enabled = False
		For x = 0 to 200
			LampState(x) = 1        ' current light state, independent of the fading level. 0 is off and 1 is on
			FadingLevel(x) = 5      ' used to track the fading state
			FlashSpeedUp(x) = 0.4   ' faster speed when turning on the flasher
			FlashSpeedDown(x) = 0.2 ' slower speed when turning off the flasher
			FlashMax(x) = 1         ' the maximum value when on, usually 1
			FlashMin(x) = 0         ' the minimum value when off, usually 0
			FlashLevel(x) = 0       ' the intensity of the flashers, usually from 0 to 1
		Next
	Else
		Debug_Lights_Flag = 0: Debug_Flashers_Flag = 0
		Illumination_Init
		Lightshow_Init
	End If
    UpdateLamps
End Sub

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
	If Debug_Lights_Flag = 0 Then
		If Not IsEmpty(chgLamp) Then
			If GameOn = False Then GameOn = True: SetGIOn
			For ii = 0 To UBound(chgLamp)
				LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
				FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
			Next
		End If
	End If
    UpdateLamps
End Sub

Sub UpdateLamps
	If GameOn = False Then Exit Sub
	If Debug_Lights_Flag = 0 Then
		If LampState(lBallRelease) = 1 Then DoBallRelease:LampState(lBallRelease) = 0
		If LampState(lTopKicker) = 1 Then DoTopKicker:LampState(lTopKicker) = 0
	End If
	If Controller.Lamp(1) = True Then 
		SetGIOff
	Else
		SetGIOn
	End If
	NFadeLm LShootAgain, l3G
	NFadeL LShootAgain, l3
	NFadeL lLeft50k1, l5
	NFadeL lLeft50k2, l6
	NFadeL lLeft50k3, l7
	NFadeL lLeft50k4, l8
	NFadeLm lExtraBall, l9G
	NFadeL lExtraBall, l9
	NFadeL lLeftOutlane, l10
	NFadeL lRightOutlane, l11
	NFadeLm lCenterLamps1, l14aG
	NFadeLm lCenterLamps1, l14a
	NFadeLm lCenterLamps1, l14bG
	NFadeL lCenterLamps1, l14b
	NFadeLm lCenterLamps2, l16aG
	NFadeLm lCenterLamps2, l16a
	NFadeLm lCenterLamps2, l16bG
	NFadeL lCenterLamps2, l16b
	NFadeLm lCar1, l17a
	NFadeLm lCar1, l17aG
	NFadeL lCar1, l17b
	NFadeLm lCar2, l18a
	NFadeLm lCar2, l18aG
	NFadeL lCar2, l18b
	NFadeLm lCar3, l19a
	NFadeLm lCar3, l19aG
	NFadeL lCar3, l19b
	NFadeLm lCar4, l20a
	NFadeLm lCar4, l20aG
	NFadeL lCar4, l20b
	NFadeLm lCar5, l21a
	NFadeLm lCar5, l21aG
	NFadeL lCar5, l21b
	NFadeLm lCar6, l22a
	NFadeLm lCar6, l22aG
	NFadeL lCar6, l22b
	NFadeLm lCar7, l23a
	NFadeLm lCar7, l23aG
	NFadeL lCar7, l23b
	NFadeLm lCar8, l24a
	NFadeLm lCar8, l24aG
	NFadeL lCar8, l24b
	NFadeLm lSpotTarget1, l25aG
	NFadeLm lSpotTarget1, l25a
	NFadeL lSpotTarget1, l25b
	NFadeLm lSpotTarget2, l26aG
	NFadeLm lSpotTarget2, l26a
	NFadeL lSpotTarget2, l26b
	NFadeLm lSpotTarget3, l27aG
	NFadeLm lSpotTarget3, l27a
	NFadeL lSpotTarget3, l27b
	NFadeLm lSpotTarget4, l28aG
	NFadeLm lSpotTarget4, l28a
	NFadeL lSpotTarget4, l28b
	NFadeLm lSpotTarget5, l29aG
	NFadeLm lSpotTarget5, l29a
	NFadeL lSpotTarget5, l29b
	NFadeLm lSpotTarget6, l30aG
	NFadeLm lSpotTarget6, l30a
	NFadeL lSpotTarget6, l30b
	NFadeLm lTopRamp, l15a
	NFadeL lTopRamp, l15b
	NFadeLm lMultiplier1X, l31G
	NFadeL lMultiplier1X, l31
	NFadeL lMultiplier2X, l32
	NFadeL lMultiplier4X, l33
	NFadeL lMultiplier8X, l34
	NFadeLm lLeftSpinnerDouble, l35G
	NFadeL lLeftSpinnerDouble, l35
	NFadeLm lDropTarget1, l36G
	NFadeL lDropTarget1, l36
	NFadeLm lDropTarget2, l37G
	NFadeL lDropTarget2, l37
	NFadeLm lDropTarget3, l38G
	NFadeL lDropTarget3, l38
	NFadeLm lDropTarget4, l39G
	NFadeL lDropTarget4, l39
	NFadeL lDropTarget50k, l40
	NFadeLm lDropTarget100k, l41G
	NFadeL lDropTarget100k, l41
	NFadeLm lDropTarget200k, l42G
	NFadeL lDropTarget200k, l42
	NFadeL lDropTargetSpecial, l43
	NFadeL lRightExtraBall, l44
	NFadeLm lTopRace, l45aG
	NFadeLm lTopRace, l45bG
	NFadeLm lTopRace, l45a
	NFadeL lTopRace, l45b
	NFadeLm lRightBottomRace, l46G
	NFadeL lRightBottomRace, l46
	NFadeLm lRightSpinner, l47aG
	NFadeLm lRightSpinner, l47a
	NFadeL lRightSpinner, l47b
	NFadeLm lLeftSpinner, l51aG
	NFadeLm lLeftSpinner, l51a
	NFadeL lLeftSpinner, l51b	
	NFadeLm lLeft100k, l70G
	NFadeL lLeft100k, l70
	NFadeLm lRight100k, l71G
	NFadeL lRight100k, l71
	nFadeLm lLightshowL1, FL1LG
	nFadeLm lLightshowL1, FL6LG
	Flashm lLightshowL1, FL1L
	Flash lLightshowL1, FL6L
	nFadeLm lLightshowL2, FL2LG
	nFadeLm lLightshowL2, FL7LG
	Flashm lLightshowL2, FL2L
	Flash lLightshowL2, FL7L
	nFadeLm lLightshowL3, FL3LG
	nFadeLm lLightshowL3, FL8LG	
	Flashm lLightshowL3, FL3L
	Flash lLightshowL3, FL8L
	nFadeLm lLightshowL4, FL4LG
	nFadeLm lLightshowL4, FL9LG
	Flashm lLightshowL4, FL4L
	Flash lLightshowL4, FL9L
	nFadeLm lLightshowL5, FL5LG
	nFadeLm lLightshowL5, FL10LG
	Flashm lLightshowL5, FL5L
	Flash lLightshowL5, FL10L
	nFadeLm lLightshowR1, FL1RG
	nFadeLm lLightshowR1, FL6RG
	Flashm lLightshowR1, FL1R
	Flash lLightshowR1, FL6R
	nFadeLm lLightshowR2, FL2RG
	nFadeLm lLightshowR2, FL7RG
	Flashm lLightshowR2, FL2R
	Flash lLightshowR2, FL7R
	nFadeLm lLightshowR3, FL3RG
	nFadeLm lLightshowR3, FL8RG
	Flashm lLightshowR3, FL3R
	Flash lLightshowR3, FL8R
	nFadeLm lLightshowR4, FL4RG
	nFadeLm lLightshowR4, FL9RG
	Flashm lLightshowR4, FL4R
	Flash lLightshowR4, FL9R
	nFadeLm lLightshowR5, FL5RG
	nFadeLm lLightshowR5, FL10RG
	Flashm lLightshowR5, FL5R
	Flash lLightshowR5, FL10R
	Flash lGI, FlasherGI5
	FlashDomesRight: FlashDomesLeft: FlashDomesTop
End Sub

' Lights: used for VP10 standard lights, the fading is handled by VP itself

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

'Lights, Ramps & Primitives used as 4 step fading lights
'a,b,c,d are the images used from on to off

Sub FadeObj(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:object.DisableLighting = 1:FadingLevel(nr) = 6		'fading to off...
        Case 5:object.image = a:FadingLevel(nr) = 1									'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             				'wait
        Case 9:object.image = c:FadingLevel(nr) = FadingLevel(nr) + 1 				'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1         				'wait
        Case 13:object.image = d:object.DisableLighting = 0:FadingLevel(nr) = 0		'Off
    End Select
End Sub

Sub FadeObjm(nr, object, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
        Case 9:object.image = c
        Case 13:object.image = d
    End Select
End Sub

Sub NFadeObj(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b:FadingLevel(nr) = 0 'off
        Case 5:object.image = a:FadingLevel(nr) = 1 'on
    End Select
End Sub

Sub NFadeObjm(nr, object, a, b)
    Select Case FadingLevel(nr)
        Case 4:object.image = b
        Case 5:object.image = a
    End Select
End Sub

' Flasher objects

Sub Flash(nr, object)
    Select Case FadingLevel(nr)
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
                FadingLevel(nr) = 0 'completely off
            End if
            Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 1 'completely on
            End if
            Object.IntensityScale = FlashLevel(nr)
    End Select
End Sub

Sub Flashm(nr, object) 'multiple flashers, it just sets the flashlevel
	Object.IntensityScale = FlashLevel(nr)
End Sub

Sub FlashDomesRight
	If ((Debug_Lights_Flag = 1) AND (Debug_Flashers_Flag = 0)) Then Exit Sub
    Select Case FadingLevel(lFlashersRight)
        Case 4 'off
			FlashDomex3 lFlashersRight, DomeRightRedB, DomeRightRedL, FlasherRightRed, LightRightRed
			FlashDomex3 lFlashersRight, DomeRightOrangeB, DomeRightOrangeL, FlasherRightOrange, LightRightOrange
            FlashLevel(lFlashersRight) = FlashLevel(lFlashersRight) * 0.9 - 0.01
			If FlashLevel(lFlashersRight) < 0.15 Then
				DomeRightRedL.visible = 0: DomeRightOrangeL.visible = 0
			End If
			If FlashLevel(lFlashersRight) < 0 Then
				PlaySoundAtVol "RelayOff", DomeRightOrangeB, 0.15
				FlasherRightRed.visible = 0: FlasherRightOrange.visible = 0
				FadingLevel(lFlashersRight) = 0 'completely off
			End If
        Case 5 ' on
			PlaySoundAtVol "RelayOn", DomeRightOrangeB, 0.25
            FlashLevel(lFlashersRight) = 1
			FlasherRightRed.visible = 1: FlasherRightOrange.visible = 1
			DomeRightRedL.visible = 1: DomeRightOrangeL.visible = 1
			FlashDomex3 lFlashersRight, DomeRightRedB, DomeRightRedL, FlasherRightRed, LightRightRed
			FlashDomex3 lFlashersRight, DomeRightOrangeB, DomeRightOrangeL, FlasherRightOrange, LightRightOrange
            FadingLevel(lFlashersRight) = 1 'completely on
    End Select
End Sub

Sub FlashDomesLeft
	If ((Debug_Lights_Flag = 1) And (Debug_Flashers_Flag = 0)) Then Exit Sub
    Select Case FadingLevel(lFlashersLeft)
        Case 4 'off
			FlashDomex3 lFlashersLeft, DomeLeftRedB, DomeLeftRedL, FlasherLeftRed, LightLeftRed
			FlashDomex3 lFlashersLeft, DomeLeftOrangeB, DomeLeftOrangeL, FlasherLeftOrange, LightLeftOrange
            FlashLevel(lFlashersLeft) = FlashLevel(lFlashersLeft) * 0.9 - 0.01
			If FlashLevel(lFlashersLeft) < 0.15 Then
				DomeLeftRedL.visible = 0: DomeLeftOrangeL.visible = 0
			End If
			If FlashLevel(lFlashersLeft) < 0 Then
				PlaySoundAtVol "RelayOff", DomeLeftOrangeB, 0.15
				FlasherLeftRed.visible = 0: FlasherLeftOrange.visible = 0
				FadingLevel(lFlashersLeft) = 0 'completely off
			End If
        Case 5 ' on
			PlaySoundAtVol "RelayOn", DomeLeftOrangeB, 0.25
            FlashLevel(lFlashersLeft) = 1
			FlasherLeftRed.visible = 1: FlasherLeftOrange.visible = 1
			DomeLeftRedL.visible = 1: DomeLeftOrangeL.visible = 1
			FlashDomex3 lFlashersLeft, DomeLeftRedB, DomeLeftRedL, FlasherLeftRed, LightLeftRed
			FlashDomex3 lFlashersLeft, DomeLeftOrangeB, DomeLeftOrangeL, FlasherLeftOrange, LightLeftOrange
            FadingLevel(lFlashersLeft) = 1 'completely on
    End Select
End Sub

Sub FlashDomesTop
	If ((Debug_Lights_Flag = 1) And (Debug_Flashers_Flag = 0)) Then Exit Sub
    Select Case FadingLevel(lFlashersTop)
        Case 4 'off
			FlashDomex3 lFlashersTop, DomeRightBlueB, DomeRightBlueL, FlasherRightBlue, LightRightBlue
			FlashDomex3 lFlashersTop, DomeLeftBlueB, DomeLeftBlueL, FlasherLeftBlue, LightLeftBlue
            FlashLevel(lFlashersTop) = FlashLevel(lFlashersTop) * 0.9 - 0.01
			If FlashLevel(lFlashersTop) < 0.15 Then
				DomeRightBlueL.visible = 0: DomeLeftBlueL.visible = 0
			End If
			If FlashLevel(lFlashersTop) < 0 Then
				PlaySoundAtVol "RelayOff", DomeRightBlueL, 0.15
				FlasherRightBlue.visible = 0: FlasherLeftBlue.visible = 0
				FadingLevel(lFlashersTop) = 0 'completely off
			End If
        Case 5 ' on
			PlaySoundAtVol "RelayOn", DomeRightBlueL, 0.25
            FlashLevel(lFlashersTop) = 1
			FlasherRightBlue.visible = 1: FlasherLeftBlue.visible = 1
			DomeRightBlueL.visible = 1: DomeLeftBlueL.visible = 1
			FlashDomex3 lFlashersTop, DomeRightBlueB, DomeRightBlueL, FlasherRightBlue, LightRightBlue
			FlashDomex3 lFlashersTop, DomeLeftBlueB, DomeLeftBlueL, FlasherLeftBlue, LightLeftBlue
            FadingLevel(lFlashersTop) = 1 'completely on
    End Select
End Sub

Sub FlashDomex3(nr, fdomeb, fdomel, fflasher, flight) 
	dim flashx3, matdim
	flashx3 = FlashLevel(nr) * FlashLevel(nr) * FlashLevel(nr)
	fflasher.opacity = fflasher.UserValue * flashx3
	fdomel.BlendDisableLighting = 10 * flashx3
	fdomeb.BlendDisableLighting =  flashx3
	flight.IntensityScale = flashx3
	matdim = Round(10 * FlashLevel(nr))
	fdomel.material = "domelit" & matdim
End Sub


Sub SetGIOn
	If GI_On = True Then Exit Sub
	For each i in GI_Bulbs: i.State = LightStateOn: Next
	FadingLevel(lGI) = 5
	GI_On = True
End Sub

Sub SetGIOff
	If GI_On = False Then Exit Sub
	For each i in GI_Bulbs: i.State = LightStateOff: Next
	FadingLevel(lGI) = 4
	GI_On = False
End Sub


'***********************************************************************************
'****							Lightshow Handling	        					****
'***********************************************************************************
Dim LightshowCycle, LightshowSide: LightshowSide = 0
Dim Virtual_L13_Lamps: Virtual_L13_Lamps=Array(lLeft100k, lRight100k)
Dim LightshowStartBulbs: LightshowStartBulbs = Array(lLightshowL1, lLightshowR1)
Dim oldL13_level

Sub LightshowTimer_Timer
	FadingLevel(LightshowStartBulbs(LightshowSide)+LightshowCycle) = 4
	LightshowCycle = (LightshowCycle + 1) Mod 5
	FadingLevel(LightshowStartBulbs(LightshowSide)+LightshowCycle) = 5
	If FadingLevel(l100k) <> oldL13_level Then	
		FadingLevel(Virtual_L13_Lamps(LightshowSide)) = FadingLevel(l100k)
		oldL13_level = FadingLevel(l100k)
	End If
End Sub

Sub LightshowChangeSide
	Dim oldlampstate: oldlampstate = FadingLevel(Virtual_L13_Lamps(LightshowSide))
	FadingLevel(Virtual_L13_Lamps(LightshowSide)) = 4
	LightshowTimer.Enabled = False
	LightshowClr(LightshowSide)
	LightshowSide = LightshowSide XOR 1
	If ((oldlampstate = 5) OR (oldlampstate = 1)) Then FadingLevel(Virtual_L13_Lamps(LightshowSide)) = 5
	LightshowTimer.Enabled = True
End Sub

Sub LightshowClr(side)
	Dim li
	For li = 0 to 4:FadingLevel(LightshowStartBulbs(side)+li) = 4:Next
	LightshowCycle = 9
End Sub

Sub Lightshow_Init
	LightshowClr 0: LightshowClr 1: LightshowCycle = 9
	LightshowTimer.Interval = 150
	LightshowTimer.Enabled = True
End Sub

Dim Digits(40)
Digits(0) = Array(a00, a05, a0c, a0d, a08, a01, a06, a0f, a02, a03, a04, a07, a0b, a0a, a09, a0e)
Digits(1) = Array(a10, a15, a1c, a1d, a18, a11, a16, a1f, a12, a13, a14, a17, a1b, a1a, a19, a1e)
Digits(2) = Array(a20, a25, a2c, a2d, a28, a21, a26, a2f, a22, a23, a24, a27, a2b, a2a, a29, a2e)
Digits(3) = Array(a30, a35, a3c, a3d, a38, a31, a36, a3f, a32, a33, a34, a37, a3b, a3a, a39, a3e)
Digits(4) = Array(a40, a45, a4c, a4d, a48, a41, a46, a4f, a42, a43, a44, a47, a4b, a4a, a49, a4e)
Digits(5) = Array(a50, a55, a5c, a5d, a58, a51, a56, a5f, a52, a53, a54, a57, a5b, a5a, a59, a5e)
Digits(6) = Array(a60, a65, a6c, a6d, a68, a61, a66, a6f, a62, a63, a64, a67, a6b, a6a, a69, a6e)
Digits(7) = Array(a70, a75, a7c, a7d, a78, a71, a76, a7f, a72, a73, a74, a77, a7b, a7a, a79, a7e)
Digits(8) = Array(a80, a85, a8c, a8d, a88, a81, a86, a8f, a82, a83, a84, a87, a8b, a8a, a89, a8e)
Digits(9) = Array(a90, a95, a9c, a9d, a98, a91, a96, a9f, a92, a93, a94, a97, a9b, a9a, a99, a9e)
Digits(10) = Array(aa0, aa5, aac, aad, aa8, aa1, aa6, aaf, aa2, aa3, aa4, aa7, aab, aaa, aa9, aae)
Digits(11) = Array(ab0, ab5, abc, abd, ab8, ab1, ab6, abf, ab2, ab3, ab4, ab7, abb, aba, ab9, abe)
Digits(12) = Array(ac0, ac5, acc, acd, ac8, ac1, ac6, acf, ac2, ac3, ac4, ac7, acb, aca, ac9, ace)
Digits(13) = Array(ad0, ad5, adc, add, ad8, ad1, ad6, adf, ad2, ad3, ad4, ad7, adb, ada, ad9, ade)
Digits(14) = Array(ae0, ae5, aec, aed, ae8, ae1, ae6, aef, ae2, ae3, ae4, ae7, aeb, aea, ae9, aee)
Digits(15) = Array(af0, af5, afc, afd, af8, af1, af6, aff, af2, af3, af4, af7, afb, afa, af9, afe)
Digits(16) = Array(b00, b05, b0c, b0d, b08, b01, b06, b0f, b02, b03, b04, b07, b0b, b0a, b09, b0e)
Digits(17) = Array(b10, b15, b1c, b1d, b18, b11, b16, b1f, b12, b13, b14, b17, b1b, b1a, b19, b1e)
Digits(18) = Array(b20, b25, b2c, b2d, b28, b21, b26, b2f, b22, b23, b24, b27, b2b, b2a, b29, b2e)
Digits(19) = Array(b30, b35, b3c, b3d, b38, b31, b36, b3f, b32, b33, b34, b37, b3b, b3a, b39, b3e)
Digits(20) = Array(b40, b45, b4c, b4d, b48, b41, b46, b4f, b42, b43, b44, b47, b4b, b4a, b49, b4e)
Digits(21) = Array(b50, b55, b5c, b5d, b58, b51, b56, b5f, b52, b53, b54, b57, b5b, b5a, b59, b5e)
Digits(22) = Array(b60, b65, b6c, b6d, b68, b61, b66, b6f, b62, b63, b64, b67, b6b, b6a, b69, b6e)
Digits(23) = Array(b70, b75, b7c, b7d, b78, b71, b76, b7f, b72, b73, b74, b77, b7b, b7a, b79, b7e)
Digits(24) = Array(b80, b85, b8c, b8d, b88, b81, b86, b8f, b82, b83, b84, b87, b8b, b8a, b89, b8e)
Digits(25) = Array(b90, b95, b9c, b9d, b98, b91, b96, b9f, b92, b93, b94, b97, b9b, b9a, b99, b9e)
Digits(26) = Array(ba0, ba5, bac, bad, ba8, ba1, ba6, baf, ba2, ba3, ba4, ba7, bab, baa, ba9, bae)
Digits(27) = Array(bb0, bb5, bbc, bbd, bb8, bb1, bb6, bbf, bb2, bb3, bb4, bb7, bbb, bba, bb9, bbe)
Digits(28) = Array(bc0, bc5, bcc, bcd, bc8, bc1, bc6, bcf, bc2, bc3, bc4, bc7, bcb, bca, bc9, bce)
Digits(29) = Array(bd0, bd5, bdc, bdd, bd8, bd1, bd6, bdf, bd2, bd3, bd4, bd7, bdb, bda, bd9, bde)
Digits(30) = Array(be0, be5, bec, bed, be8, be1, be6, bef, be2, be3, be4, be7, beb, bea, be9, bee)
Digits(31) = Array(bf0, bf5, bfc, bfd, bf8, bf1, bf6, bff, bf2, bf3, bf4, bf7, bfb, bfa, bf9, bfe)
Digits(32) = Array(c00, c05, c0c, c0d, c08, c01, c06, c0f, c02, c03, c04, c07, c0b, c0a, c09, c0e)
Digits(33) = Array(c10, c15, c1c, c1d, c18, c11, c16, c1f, c12, c13, c14, c17, c1b, c1a, c19, c1e)
Digits(34) = Array(c20, c25, c2c, c2d, c28, c21, c26, c2f, c22, c23, c24, c27, c2b, c2a, c29, c2e)
Digits(35) = Array(c30, c35, c3c, c3d, c38, c31, c36, c3f, c32, c33, c34, c37, c3b, c3a, c39, c3e)
Digits(36) = Array(c40, c45, c4c, c4d, c48, c41, c46, c4f, c42, c43, c44, c47, c4b, c4a, c49, c4e)
Digits(37) = Array(c50, c55, c5c, c5d, c58, c51, c56, c5f, c52, c53, c54, c57, c5b, c5a, c59, c5e)
Digits(38) = Array(c60, c65, c6c, c6d, c68, c61, c66, c6f, c62, c63, c64, c67, c6b, c6a, c69, c6e)
Digits(39) = Array(c70, c75, c7c, c7d, c78, c71, c76, c7f, c72, c73, c74, c77, c7b, c7a, c79, c7e)

Sub LedsTimer_Timer()
	On Error Resume Next
	Dim ChgLED,ii,num,chg,stat,obj

	ChgLED = Controller.ChangedLEDs(&HFFFFFFFF, &HFFFFFFFF) 
	If Not IsEmpty(ChgLED) Then
		For ii = 0 To UBound(ChgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			For Each obj In Digits(num)
				If chg And 1 Then obj.State = stat And 1
				chg = chg\2 : stat = stat\2
			Next
		Next
	End IF
End Sub

Sub DOF(dofevent, dofstate)	
	If cController < 3 Then Exit Sub
	If dofstate = 2 Then
		Controller.B2SSetData dofevent, 1:Controller.B2SSetData dofevent, 0
	Else
		Controller.B2SSetData dofevent, dofstate
	End If
End Sub 



'***********************************************************************************
'****				DIP switch routines (parts by Luvthatapex) 					****
'***********************************************************************************
Dim TableOptions, TableName

Sub CustomizeTable
	Sys80ShowDips
	OptionsEdit
End Sub

Sub OptionsEdit
	Dim DT_offs: DT_offs = 0
	Dim vpmDips1: Set vpmDips1 = New cvpmDips
	With vpmDips1
	 	.AddForm 350, 380, "Victory - Table Options"
	 	.AddLabel 0,0 + DT_offs,180,15,"Play Options"
	 	.AddFrameExtra 15,15 + DT_offs,205,"Left Outlane*",&H00003000, Array("Liberal", 0, "Normal", &H00001000, "Difficult", &H00002000)
	 	.AddChkExtra 0,150 + DT_offs,155, Array("Disable Menu Next Start", &H00000001)
	End With
	TableOptions = vpmDips1.ViewDipsExtra(TableOptions)
	SaveValue TableName,"Options",TableOptions
	OptionsToVariables
End Sub

Sub OptionsLoad
	TableName="Victory"
	Set vpmShowDips = GetRef("CustomizeTable")
 	TableOptions = LoadValue(TableName,"Options")			
	If TableOptions = "" Or OptionReset Then 
		TableOptions = 1				
		'OptionsEdit							
	Else
		If TableOptions And 1 = 0 Then
			TableOptions = TableOptions OR 1		
			OptionsEdit							
		Else
			OptionsToVariables
		End If
	End If
End Sub

Sub OptionsToVariables
	Dim newOL
	If DesktopMode = False Then
		cController = ((TableOptions AND &H00000070) / &H00000010)
	Else
		cController = 1
	End If
	newOL = ((TableOptions AND &H00003000) / &H00001000)
	If newOL <> OutlaneLPos Then
		OutlaneLPos = newOL
		Outlanes_Init
	End If
End Sub



'***********************************************************************************
'****							RealTime Updates								****
'***********************************************************************************
Set MotorCallback = GetRef("GameTimer")

Sub GameTimer
	RollingSound
	UpdateSpinners
    UpdateFlippers
'	DebugShowAll
End Sub

Sub UpdateSpinners
	sw30p.RotZ = -(sw30s.currentangle)
	sw43p.RotZ = -(sw43s.currentangle)
	sw53p.RotZ = -(sw53.currentangle)
	sw54p.RotZ = -(sw54.currentangle)
	sw63p.RotZ = -(sw63.currentangle)
	sw73p.RotZ = -(sw73s.currentangle)
	GateBRP.RotZ = ABS(GateBR.currentangle)
End Sub

Sub UpdateFlippers
	FlipperLLP.RotY = FlipperLL.CurrentAngle
	FlipperLRP.RotY = FlipperLR.CurrentAngle
	FlipperULP.RotY = FlipperUL.CurrentAngle
	FlipperURP.RotY = FlipperUR.CurrentAngle
	FlipperLLSh.RotZ = FlipperLL.CurrentAngle
	FlipperLRSh.RotZ = FlipperLR.CurrentAngle
	FlipperULSh.RotZ = FlipperUL.CurrentAngle
	FlipperURSh.RotZ = FlipperUR.CurrentAngle
End Sub

'*****************************************
'	ninuzzu'smodified BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

Sub BallShadowUpdate_Timer()
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
        If BOT(b).X < Victory.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/40) + ((BOT(b).X - (Victory.Width/2))/40)) + 1
		Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/40) + ((BOT(b).X - (Victory.Width/2))/40)) - 1
        End If
        ballShadow(b).Y = BOT(b).Y + 30
		BallShadow(b).Z = BOT(b).Z + 1
        If (BOT(b).Z < 100) Then
            BallShadow(b).Image = "BallShadow1"
		Else
			BallShadow(b).Image = "BallShadow2"
		End If
		If (BOT(b).Z > 20) Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub

'***********************************************************************************
'****								Misc stuff									****
'***********************************************************************************
Dim Pi: Pi = 4 * Atn(1)
Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
	if ABS(dSin) < 0.000001 Then dSin = 0
	if ABS(dSin) > 0.999999 Then dSin = 1 * sgn(dSin)
End Function

Function dCos(degrees)
	dCos = cos(degrees * Pi/180)
	if ABS(dCos) < 0.000001 Then dCos = 0
	if ABS(dCos) > 0.999999 Then dCos = 1 * sgn(dCos)
End Function



'***********************************************************************************
'****				        		Debug Stuff		      				    	****
'***********************************************************************************
Sub DebugPulseSwitch
	Dim sw
	sw = InputBox("Enter a number")
	VPMTimer.PulseSw sw
End Sub

Sub DebugShowLamps
	Dim ii,tmp: tmp= ""
	For ii = 1 to 30:tmp = tmp & (Controller.Lamp(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 31 to 60:tmp = tmp & (Controller.Lamp(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 61 to 90:tmp = tmp & (Controller.Lamp(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 91 to 120:tmp = tmp & (Controller.Lamp(ii) AND 1):Next
	DebugText.Text = tmp
End Sub

Sub DebugShowSwitches
	Dim ii,tmp: tmp= ""
	For ii = 1 to 30:tmp = tmp & (Controller.Switch(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 31 to 60:tmp = tmp & (Controller.Switch(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 61 to 90:tmp = tmp & (Controller.Switch(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 91 to 120:tmp = tmp & (Controller.Switch(ii) AND 1):Next
	DebugText.Text = tmp
End Sub

Sub DebugShowSolenoids
	Dim ii,tmp: tmp= ""
	For ii = 1 to 30:tmp = tmp & (Controller.Solenoid(ii) AND 1):Next
	DebugText.Text = tmp
End Sub

Sub DebugPosBall
	CurrentBall.X = 245:CurrentBall.Y = 516:CurrentBall.Z = 25:CurrentBall.VelX = -3:CurrentBall.VelY = -40
End Sub

Sub DebugShowAll
	Dim ii,tmp: tmp= "Solenoid Matrix" & Chr(13)
	For ii = 1 to 30:tmp = tmp & (Controller.Solenoid(ii) AND 1):Next
	tmp= tmp & Chr(13) & "Switch Matrix" & Chr(13)
	For ii = 1 to 30:tmp = tmp & (Controller.Switch(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 31 to 60:tmp = tmp & (Controller.Switch(ii) AND 1):Next
	tmp= tmp & Chr(13) & "Lamp Matrix" & Chr(13)
	For ii = 1 to 30:tmp = tmp & (Controller.Lamp(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 31 to 60:tmp = tmp & (Controller.Lamp(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 61 to 90:tmp = tmp & (Controller.Lamp(ii) AND 1):Next: tmp = tmp & Chr(13)
	For ii = 91 to 120:tmp = tmp & (Controller.Lamp(ii) AND 1):Next
	DebugText.Text = tmp
	DebugText.Text = tmp
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

'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Vol)
		PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set a Looping sound at object.
Sub PlayLoopSoundAtVol(sound, tableobj, Vol)
	PlaySound sound, -1, Vol, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "Victory" is the name of the table
	Dim tmp
    tmp = tableobj.y * 2 / Victory.height-1
    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "Victory" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / Victory.width-1
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

Function RndNum(min,max)
 RndNum = Int(Rnd()*(max-min+1))+min     ' Sets a random number between min and max
End Function


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

Sub RollingSound()
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
        If BallVel(BOT(b) ) > 1 and not BOT(b).z Then
			rolling(b) = True
			if BOT(b).z < 30 Then ' Ball on playfield
						PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), audioPan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
			Else ' Ball on raised ramp
						PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) )*.5, audioPan(BOT(b) ), 0, Pitch(BOT(b) )+30000, 1, 0, AudioFade(BOT(b) )
				End If
		Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next

	on error resume next ' In case VP is too old..
		' Kill ball spin
	if mLockMagnet.MagnetOn then
		dim rampballs:rampballs = mLockMagnet.Balls
		dim obj
		for each obj in rampballs
			obj.AngMomZ= 0
			obj.AngVelZ= 0
			obj.AngMomY= 0
			obj.AngVelY= 0
			obj.AngMomX= 0
			obj.AngVelX= 0
			obj.velx = 0
			obj.vely = 0
			obj.velz = 0
		next 
	end if
	on error goto 0
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
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
	PlaySound "fx_PinHit", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Switches_Hit (idx)
	PlaySound "Sensor", 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall)/2, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
	PlaySound "GateWire", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
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

Sub RandomSoundHole()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "Hole1", 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "Hole2", 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "Hole4", 0, 1, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub FlipperLL_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub FlipperLR_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub FlipperUL_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub FlipperUR_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub


'************************RAMP STUFF************************
'Set invisible Trigger on surface of ramp. Add these commands for each one.

'*********************UPPER PLAYFIELD DROP*****************
Sub UPD_Hit: PlaySoundAt "BallBounce", UPD: End Sub

'***********************RAMP SOUNDS************************

Sub WRL_Hit: PlaySound "MetalRolling", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall): End Sub
Sub WRR_Hit: PlaySound "MetalRolling", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall): End Sub

Sub LRD_Hit()
Stopsound "MetalRolling"
PlaySoundAt "BallBounce", LRD
End Sub

Sub RRD_Hit()
Stopsound "MetalRolling"
PlaySoundAt "BallBounce", RRD
End Sub

'RAMP BUMPS

Sub RampBumps_Hit (idx)
	RandomSoundRamps
End Sub

Sub RandomSoundRamps()
	Select Case Int(Rnd*7)+1
		Case 1: Playsound "RB1", 0, Vol(ActiveBall)*2, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2: Playsound "RB2", 0, Vol(ActiveBall)*2, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3: Playsound "RB3", 0, Vol(ActiveBall)*2, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 4: Playsound "RB4", 0, Vol(ActiveBall)*2, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 5: Playsound "RB5", 0, Vol(ActiveBall)*2, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 6: Playsound "RB6", 0, Vol(ActiveBall)*2, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 7: Playsound "RB7", 0, Vol(ActiveBall)*2, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub
