Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="jb_10r",UseSolenoids=2,UseLamps=0,UseGI=1,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin"

' UseVPMModSol   When True this allows the ROM to control the intensity level of modulated solenoids 
' instead of just on/off.
Const UseVPMModSol=1



'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow, 1 = enable dynamic ball shadow



'///////////////////////-----VR Room Options-----///////////////////////

Const VRRoom = 0   		'0 = VR OFF, 1 = VR Room ON

Dim UseVPMDMD
If VRRoom = 1 then UseVPMDMD = true Else UseVPMDMD = DesktopMode

'///////////////////////////////////////////////////////////////////////


LoadVPM "01560000", "WPC.VBS", 3.50  'LoadVPM "01570000", "wpc.vbs", 3.26
Dim DesktopMode: DesktopMode = Table1.ShowDT


dim ballsize,ballmass

ballsize=50
ballmass=1


Dim NullFader : set NullFader = new NullFadingObject
Dim FadeLights : Set FadeLights = New LampFader
Dim FlasherInterval: FlasherInterval = 17




'///////////////////////-----General Sound Options-----///////////////////////
'// VolumeDial:
'// VolumeDial is the actual global volume multiplier for the mechanical sounds.
'// Values smaller than 1 will decrease mechanical sounds volume.
'// Recommended values should be no greater than 1.
Const VolumeDial = 0.8

'**********************************************************************************************************
'Initiate Table
'**********************************************************************************************************

Dim bsTrough, bsSaucer, bsREye, bsLEye, dtDTBank, mVisor

Sub Table1_Init
   SetLocale(1033)
   solRampDwn(True)

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
		.Switch(22) = 1			'close coin door
		.Switch(24) = 1			'always closed
     End With
 
    PinMAMETimer.Interval = PinMAMEInterval
    PinMAMETimer.Enabled = 1

	vpmNudge.TiltSwitch=14
  	vpmNudge.Sensitivity=1
  	vpmNudge.TiltObj=Array(sw61_Bumper1,sw62_Bumper2,sw63_Bumper3,LeftSlingshot,RightSlingshot)

          Set bsTrough = New cvpmBallStack
			  bstrough.InitSw 0, 32, 33, 34, 35, 0, 0, 0
      		  bsTrough.InitKick BallRelease, 160, 8
              bsTrough.Balls = 4
 
          Set bsSaucer = New cvpmBallStack
              bsSaucer.InitSaucer sw46,46, 155, 12
   		      bsSaucer.KickForceVar = 2
   		      bsSaucer.KickAngleVar = 2

          Set bsLEye = New cvpmBallStack
              bsLEye.InitSaucer sw47,47, 165, 12
   	 	      bsLEye.KickForceVar = 2
   	    	  bsLEye.KickAngleVar = 2
 
          Set bsREye = New cvpmBallStack
              bsREye.InitSaucer sw48,48, 195, 12
   		      bsREye.KickForceVar = 2
   		      bsREye.KickAngleVar = 2
 
 
	Set mVisor = New cvpmMech
	With mVisor
	 .MType = vpmMechOneSol + vpmMechReverse + vpmMechLinear
         .Sol1 = 28
         .Length = 120
         .Steps = 58
         .AddSw 115, 0, 0
         .AddSw 117, 58, 58
         .Callback = GetRef("UpdateVisor")
         .Start
     End With

	Flasherset15 0
	Flasherset16 0
	Flasherset17 0
	Flasherset18 0
	Flasherset19 0
	Flasherset20 0
	Flasherset21 0
	Flasherset22 0
	Flasherset23 0
	Flasherset24 0
	Flasherset25 0
	Flasherset26 0
	Flasherset27 0

	FlasherInit.enabled = True

End Sub

Sub Table1_exit()
	SaveLUT
	Controller.Stop
End sub

Dim FICount:FICount = 0

Sub FlasherInit_Timer()
	FICount = FICount + 1

	Select Case FICount
		Case 25: 
			UpdateGi 0,8
			UpdateGi 1,8
			UpdateGi 2,8
			UpdateGi 3,8
			SetFlashersOdd 255
		Case 30: 
			SetFlashersEven 255
		Case 35: 
			SetFlashersOdd 0
		Case 40: 
			SetFlashersEven 0
			UpdateGi 0,0
			UpdateGi 1,0
			UpdateGi 2,0
			UpdateGi 3,0
		Case 45: 
			SetFlashersOdd 255
		Case 50: 
			SetFlashersEven 255
		Case 55: 
			SetFlashersOdd 0
		Case 60: 
			SetFlashersEven 0
			UpdateGi 0,8
			UpdateGi 1,8
			UpdateGi 2,8
			UpdateGi 3,8
			solRampDwn 1
		Case 65: me.enabled = False
	End Select
End Sub

Sub SetFlashersOdd(value)
	Flasherset15 value
	Flasherset17 value
	Flasherset19 value
	Flasherset21 value
	Flasherset23 value
	Flasherset25 value
	Flasherset27 value
End Sub

Sub SetFlashersEven(value)
	Flasherset16 value
	Flasherset18 value
	Flasherset20 value
	Flasherset22 value
	Flasherset24 value
	Flasherset26 value
End Sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

dim BIPL : BIPL=0

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyDownHandler(keycode) Then Exit Sub
	If KeyCode = PlungerKey Then Plunger.Pullback:SoundPlungerPull()
	if KeyCode = LeftTiltKey Then Nudge 90, 5:SoundNudgeLeft()
	if KeyCode = RightTiltKey Then Nudge 270, 5:SoundNudgeRight()
	if KeyCode = CenterTiltKey Then Nudge 0, 4:SoundNudgeCenter()
	If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress
'    if keycode = keyFront Then Controller.Switch(23) = 1
    If KeyCode = RightMagnaSave Then Controller.Switch(23) = 1
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
	End If
		
    If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
            Select Case Int(rnd*3)
                    Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
                    Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
                    Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
             End Select
    End If

	If keycode = 203 Then BCleft = 1	' Left Arrow
	If keycode = 200 Then BCup = 1		' Up Arrow		
	If keycode = 208 Then BCdown = 1	' Down Arrow
	If keycode = 205 Then BCright = 1	' Right Arrow

	If keycode = LeftFlipperKey Then 
		Primary_flipper_button_left.X = 2117 + 8
	End If
	If keycode = RightFlipperKey Then
		Primary_flipper_button_right.X = 2082 - 8
	End If

	If keycode = PlungerKey Then  
		TimerVRPlunger.Enabled = True
        TimerVRPlunger2.Enabled = False
	End If
	IF keycode = RightMagnaSave Then 
		Extra_Ball_button.Y = 332 - 4
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

End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyUpHandler(keycode) Then Exit Sub
'    If keycode = keyFront Then Controller.Switch(23) = 0
	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress
    If KeyCode = RightMagnaSave Then Controller.Switch(23) = 0

    If KeyCode = PlungerKey Then
            Plunger.Fire
            If BIPL = 1 Then
                    SoundPlungerReleaseBall()                        'Plunger release sound when there is a ball in shooter lane
            Else
                    SoundPlungerReleaseNoBall()                        'Plunger release sound when there is no ball in shooter lane
            End If
    End If
'Manual Ball Control
	If EnableBallControl = 1 Then
		If keycode = 203 Then BCleft = 0	' Left Arrow
		If keycode = 200 Then BCup = 0		' Up Arrow
		If keycode = 208 Then BCdown = 0	' Down Arrow
		If keycode = 205 Then BCright = 0	' Right Arrow
	End If

	If keycode = LeftFlipperKey Then 
		Primary_flipper_button_left.X = 2117
	End If
	If keycode = RightFlipperKey Then
		Primary_flipper_button_right.X = 2082
	End If

	If keycode = PlungerKey Then  
		TimerVRPlunger.Enabled = False
        TimerVRPlunger2.Enabled = True
	End If
	IF keycode = RightMagnaSave Then 
		Extra_Ball_button.Y = 332
	End If

End Sub

'******************* VR Plunger **********************

Sub TimerVRPlunger_Timer

  If PinCab_Shooter.Y < -64 then
       PinCab_Shooter.Y = PinCab_Shooter.Y + 5
  End If
End Sub

Sub TimerVRPlunger2_Timer
	PinCab_Shooter.Y = -180 + (5* Plunger.Position) -20
End Sub


'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************
SolCallback(1) = "bsTrough.SolOut"
'2 not used
SolCallback(3) = "bsSaucer.SolOut"
SolCallback(4) = "ResetDrops" 'Drop Targets
SolCallback(5) = "bsREye.SolOut"
SolCallback(6) = "solRampUp"
SolCallback(7) =  "SolKnocker"
SolCallback(8) = "bsLEye.SolOut"
'9 left slingshot
'10 right slingshot
'11 lower bumper
'12 left bumper
'13 upper bumper
SolCallback(14) = "solRampDwn"

SolModCallback(15) = "Flasherset15" 'RightEye Flash
SolModCallback(16) = "Flasherset16" 'LeftEye Flash
SolModCallback(17) = "Flasherset17" 'Center Visor Flash
SolModCallback(18) = "Flasherset18" 'Pinbot face flasher
SolModCallback(19) = "Flasherset19" 'Bumper Flasher
SolModCallback(20) = "Flasherset20" 'Flasher Lower Left
SolModCallback(21) = "Flasherset21" 'Flashers Center Left
SolModCallback(22) = "Flasherset22" 'Flasher Lower Right
SolModCallback(23) = "Flasherset23" 'Flashers 1 (L)
SolModCallback(24) = "Flasherset24" 'Flashers 2
SolModCallback(25) = "Flasherset25" 'Flashers 3
SolModCallback(26) = "Flasherset26" 'Flashers 4
SolModCallback(27) = "Flasherset27" 'Flashers 5 (R)



SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

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

'
dim Gi0IsOff,Gi1IsOff,Gi2IsOff,Gi3IsOff

'*****************************************
'      		NFOZZY FLIPPERS
'*****************************************


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


'*****************************************
'      		Modulated Flashers
'*****************************************

Dim FlashFadeUp, FlashFadeDown

FlashFadeUp = 80
FlashFadeDown = 60

Dim FlashLevel15, FlashLevel16, FlashLevel17, FlashLevel18, FlashLevel19, FlashLevel20, FlashLevel21, FlashLevel22
Dim FlashLevel23, FlashLevel24, FlashLevel25, FlashLevel26,FlashLevel27

Dim Flash2Level15, Flash2Level16, Flash2Level17, Flash2Level18, Flash2Level19, Flash2Level20, Flash2Level21, Flash2Level22
Dim Flash2Level23, Flash2Level24, Flash2Level25, Flash2Level26,Flash2Level27

Dim FlashLevel15dir, FlashLevel16dir, FlashLevel17dir, FlashLevel18dir, FlashLevel19dir, FlashLevel20dir, FlashLevel21dir, FlashLevel22dir
Dim FlashLevel23dir, FlashLevel24dir, FlashLevel25dir, FlashLevel26dir,FlashLevel27dir

Sub FlasherClick(oldvalue, newvalue) : If oldvalue <= 0 and newvalue > 0.2 Then PlaySound "fx_relay",0,0.1: End If : End Sub

' ********* Right Eye Flasher **********

'	FadeLights.obj(101) = array(flash101) 'right eye flash

Sub Flasherset15(value)
	if value < FlashLevel15 Then
		FlashLevel15dir =  -1
	elseif value > FlashLevel15 Then
		FlashLevel15dir = 1
	end If

	FlashLevel15 = value 
	Flash101.Timerenabled = true 

End Sub 

Sub Flash101_timer()
	me.timerinterval = FlasherInterval

	If FlashLevel15dir > 0 Then
		If Flash2Level15 < FlashLevel15 Then
			Flash2Level15 = Flash2Level15 + FlashFadeUp
		Else
			Flash2Level15 = FlashLevel15
			me.timerenabled = False
		End If
	Else
		If Flash2Level15 > FlashLevel15 Then
			Flash2Level15 = Flash2Level15 - FlashFadeDown
		Else
			Flash2Level15 = FlashLevel15
			me.timerenabled = False
		End If
	End If

	if Flash2Level15 >  0 Then
		flash101.visible = 1
		flash101.IntensityScale = Flash2Level15 / 255
	else
		flash101.visible = 0
	end if

	if Flash2Level15 > 0 Then
		if Flash2Level16 > 0 Then
			mask_prim.image="maskon"
		else
			mask_prim.image="maskrighton" '101 is on, 104 is off
		end if
	else
		if Flash2Level16 > 0 Then
			mask_prim.image="masklefton"
		else
			mask_prim.image="maskoff"
		end if
	end if
End Sub

' ********* Left Eye Flasher **********

'	FadeLights.obj(104) = array(flash104) 'left eye flash
Sub Flasherset16(value)
	if value < FlashLevel16 Then
		FlashLevel16dir =  -1
	elseif value > FlashLevel16 Then
		FlashLevel16dir = 1
	end If

	FlashLevel16 = value 
	Flash104.Timerenabled = true 

End Sub 

Sub Flash104_timer()
	me.timerinterval = FlasherInterval

	If FlashLevel16dir > 0 Then
		If Flash2Level16 < FlashLevel16 Then
			Flash2Level16 = Flash2Level16 + FlashFadeUp
		Else
			Flash2Level16 = FlashLevel16
			me.timerenabled = False
		End If
	Else
		If Flash2Level16 > FlashLevel16 Then
			Flash2Level16 = Flash2Level16 - FlashFadeDown
		Else
			Flash2Level16 = FlashLevel16
			me.timerenabled = False
		End If
	End If

	if Flash2Level16 >  0 Then
		flash104.visible = 1
		flash104.IntensityScale = Flash2Level16 / 255
	else
		flash104.visible = 0
	end if

	if Flash2Level16 > 0 Then
		if Flash2Level16 > 0 Then
			mask_prim.image="maskon"
		else
			mask_prim.image="masklefton" '104 is on, 101 is off
		end if
	else
		if Flash2Level15 > 0 Then
			mask_prim.image="maskrighton"
		else
			mask_prim.image="maskoff"
		end if
	end if
End Sub


' ********* Center Visor Flasher **********

'	FadeLights.obj(102) = array(flash102) 'center visor flash
Sub Flasherset17(value)
	if value < FlashLevel17 Then
		FlashLevel17dir =  -1
	elseif value > FlashLevel17 Then
		FlashLevel17dir = 1
	end If

	FlashLevel17 = value 
	Flash102.Timerenabled = true 

End Sub 

Sub Flash102_timer()
	me.timerinterval = FlasherInterval

	If FlashLevel17dir > 0 Then
		If Flash2Level17 < FlashLevel17 Then
			Flash2Level17 = Flash2Level17 + FlashFadeUp
		Else
			Flash2Level17 = FlashLevel17
			me.timerenabled = False
		End If
	Else
		If Flash2Level17 > FlashLevel17 Then
			Flash2Level17 = Flash2Level17 - FlashFadeDown
		Else
			Flash2Level17 = FlashLevel17
			me.timerenabled = False
		End If
	End If

	if Flash2Level17 >  0 Then
		flash102.visible = 1
		flash102.IntensityScale = Flash2Level17 / 255
	else
		flash102.visible = 0
	end if

	if Flash2Level17 > 0 Then
		mask_prim.image="maskon"
	else
		mask_prim.image="maskoff"
	end if
End Sub

' ********* Pinbot Face Flasher **********

'	FadeLights.obj(109) = array(l109, l109a, l109z, f109)
Sub Flasherset18(value)
	if value < FlashLevel18 Then
		FlashLevel18dir =  -1
	elseif value > FlashLevel18 Then
		FlashLevel18dir = 1
	end If

	FlashLevel18 = value 
	F109.Timerenabled = true 
End Sub 

Sub F109_timer()
	me.timerinterval = FlasherInterval

	If FlashLevel18dir > 0 Then
		If Flash2Level18 < FlashLevel18 Then
			Flash2Level18 = Flash2Level18 + FlashFadeUp
		Else
			Flash2Level18 = FlashLevel18
			me.timerenabled = False
		End If
	Else
		If Flash2Level18 > FlashLevel18 Then
			Flash2Level18 = Flash2Level18 - FlashFadeDown
		Else
			Flash2Level18 = FlashLevel18
			me.timerenabled = False
		End If
	End If

	if Flash2Level18 >  0 Then
		l109.state = 1
		l109a.state = 1
		l109z.state = 1
		f109.visible = 1
		l109.IntensityScale = Flash2Level18 / 255
		l109a.IntensityScale = Flash2Level18 / 255
		l109z.IntensityScale = Flash2Level18 / 255
		f109.IntensityScale = Flash2Level18 / 255
	else
		l109.state = 0
		l109a.state = 0
		l109z.state = 0
		f109.visible = 0
	end if
End Sub

' ********* Bumper Flasher **********

'	FadeLights.obj(107) = array(l107, l107a, l107z,BumpFlash,FlasherB,Flash107wall)
Sub Flasherset19(value)
	if value < FlashLevel19 Then
		FlashLevel19dir =  -1
	elseif value > FlashLevel19 Then
		FlashLevel19dir = 1
	end If

	FlashLevel19 = value 
	l107.Timerenabled = true 
End Sub 

Sub l107_timer()
	me.timerinterval = FlasherInterval

	If FlashLevel19dir > 0 Then
		If Flash2Level19 < FlashLevel19 Then
			Flash2Level19 = Flash2Level19 + FlashFadeUp
		Else
			Flash2Level19 = FlashLevel19
			me.timerenabled = False
		End If
	Else
		If Flash2Level19 > FlashLevel19 Then
			Flash2Level19 = Flash2Level19 - FlashFadeDown
		Else
			Flash2Level19 = FlashLevel19
			me.timerenabled = False
		End If
	End If

	if Flash2Level19 >  0 Then
		l107.state = 1
		l107a.state = 1
		l107z.state = 1
		BumpFlash.visible = 1
		FlasherB.visible = 1
		Flash107wall.visible = 1
		l109.IntensityScale = Flash2Level19 / 255
		l109a.IntensityScale = Flash2Level19 / 255
		l109z.IntensityScale = Flash2Level19 / 255
		BumpFlash.IntensityScale = Flash2Level19 / 255
		FlasherB.IntensityScale = Flash2Level19 / 255
		Flash107wall.IntensityScale = Flash2Level19 / 255
	else
		l107.state = 0
		l107a.state = 0
		l107z.state = 0
		BumpFlash.visible = 0
		FlasherB.visible = 0
		Flash107wall.visible = 0
	end if
End Sub


' ********* Lower Left Flasher **********

'	FadeLights.obj(106) = array(Flasher1L,Flasher1L1,FLasher1L2,Flash106wall) 'lower left flasher
Sub Flasherset20(value)
	if value < FlashLevel20 Then
		FlashLevel20dir =  -1
	elseif value > FlashLevel20 Then
		FlashLevel20dir = 1
	end If

	FlashLevel20 = value 
	Flasher1L.Timerenabled = true 
End Sub 

Sub Flasher1L_timer()
	me.timerinterval = FlasherInterval

	If FlashLevel20dir > 0 Then
		If Flash2Level20 < FlashLevel20 Then
			Flash2Level20 = Flash2Level20 + FlashFadeUp
		Else
			Flash2Level20 = FlashLevel20
			me.timerenabled = False
		End If
	Else
		If Flash2Level20 > FlashLevel20 Then
			Flash2Level20 = Flash2Level20 - FlashFadeDown
		Else
			Flash2Level20 = FlashLevel20
			me.timerenabled = False
		End If
	End If

	if Flash2Level20 >  0 Then
		Flasher1L.visible = 1
		Flasher1L1.visible = 1
		Flasher1L2.visible = 1
		Flash106wall.visible = 1
		Flasher1L.IntensityScale = Flash2Level20 / 255
		Flasher1L1.IntensityScale = Flash2Level20 / 255
		Flasher1L2.IntensityScale = Flash2Level20 / 255
		Flash106wall.IntensityScale = Flash2Level20 / 255
	else
		Flasher1L.visible = 0
		Flasher1L1.visible = 0
		Flasher1L2.visible = 0
		Flash106wall.visible = 0
	end if

	if Flash2Level20 > 0 Then
		if Flash2Level20/255 < 0.2 then 
			Flasher2.image="flash1lgion1" 
			Flasher2.blenddisablelighting=0.8	
		elseif Flash2Level20/255 <0.4 then
			Flasher2.image="flash1lgion2" 
			Flasher2.blenddisablelighting=0.85	
		elseif Flash2Level20/255 <0.6 then
			Flasher2.image="flash1lgion3" 
			Flasher2.blenddisablelighting=0.9	
		elseif Flash2Level20/255 <0.8 then
			Flasher2.image="flash1lgion4" 
			Flasher2.blenddisablelighting=0.95	
		else
			Flasher2.image="flash1lgion5" 
			Flasher2.blenddisablelighting=1	
		end if

		if Gi1IsOff Then			
			acorns.image="acornsoffflash1"
		else			
			acorns.image="acornsonflash1"
		end if 
	else
		if Gi1IsOff Then
			Flasher2.image="flasher1loffgioff" 'giisoff, flasher is on (lamp 106) (same texture as when gi is on)
			Flasher2.blenddisablelighting=.8				
			acorns.image="acornsoff"
		Else
			Flasher2.image="flasher1loffgion" 'giisoff, flasher is on (lamp 106) (same texture as when gi is on)
			Flasher2.blenddisablelighting=.8				
			acorns.image="acornson"
		end if
	end if
End Sub


' ********* Center Left Flasher **********

'	FadeLights.obj(108) = array(Flasher2L,Flasher2L1,Flasher2L2,Flash108wall)
Sub Flasherset21(value)
	if value < FlashLevel21 Then
		FlashLevel21dir =  -1
	elseif value > FlashLevel21 Then
		FlashLevel21dir = 1
	end If

	FlashLevel21 = value 
	Flasher2L.Timerenabled = true 
End Sub 

Sub Flasher2L_timer()
	me.timerinterval = FlasherInterval

	If FlashLevel21dir > 0 Then
		If Flash2Level21 < FlashLevel21 Then
			Flash2Level21 = Flash2Level21 + FlashFadeUp
		Else
			Flash2Level21 = FlashLevel21
			me.timerenabled = False
		End If
	Else
		If Flash2Level21 > FlashLevel21 Then
			Flash2Level21 = Flash2Level21 - FlashFadeDown
		Else
			Flash2Level21 = FlashLevel21
			me.timerenabled = False
		End If
	End If

	if Flash2Level21 >  0 Then
		Flasher2L.visible = 1
		Flasher2L1.visible = 1
		Flasher2L2.visible = 1
		Flash108wall.visible = 1
		Flasher2L.IntensityScale = Flash2Level21 / 255
		Flasher2L1.IntensityScale = Flash2Level21 / 255
		Flasher2L2.IntensityScale = Flash2Level21 / 255
		Flash108wall.IntensityScale = Flash2Level21 / 255
	else
		Flasher2L.visible = 0
		Flasher2L1.visible = 0
		Flasher2L2.visible = 0
		Flash108wall.visible = 0
	end if

	if Flash2Level21 > 0 Then
		if Flash2Level21/255 < 0.2 then 
			Flasher3.image="flash2gion1" 
			Flasher3.blenddisablelighting=0.8	
		elseif Flash2Level21/255 <0.4 then
			Flasher3.image="flash2gion2" 
			Flasher3.blenddisablelighting=0.85	
		elseif Flash2Level21/255 <0.6 then
			Flasher3.image="flash2gion3" 
			Flasher3.blenddisablelighting=0.9	
		elseif Flash2Level21/255 <0.8 then
			Flasher3.image="flash2gion4" 
			Flasher3.blenddisablelighting=0.95	
		else
			Flasher3.image="flash2gion5" 
			Flasher3.blenddisablelighting=1	
		end if	
	else
		if Gi1IsOff Then
			Flasher3.image="flasher2offgioff"
			Flasher3.blenddisablelighting=.8	
		Else
			Flasher3.image="flasher2offgion"
			Flasher3.blenddisablelighting=.8	
		end if
	end if
End Sub

' ********* Lower Right Flasher **********

'	FadeLights.obj(103) = array(Flasher1R,Flasher1R1,FLasher1R2,Flash103wall) 'lower right flasher
Sub Flasherset22(value)
	if value < FlashLevel22 Then
		FlashLevel22dir =  -1
	elseif value > FlashLevel22 Then
		FlashLevel22dir = 1
	end If

	FlashLevel22 = value 
	Flasher1R.Timerenabled = true 
End Sub 

Sub Flasher1R_timer()
	me.timerinterval = FlasherInterval

	If FlashLevel22dir > 0 Then
		If Flash2Level22 < FlashLevel22 Then
			Flash2Level22 = Flash2Level22 + FlashFadeUp
		Else
			Flash2Level22 = FlashLevel22
			me.timerenabled = False
		End If
	Else
		If Flash2Level22 > FlashLevel22 Then
			Flash2Level22 = Flash2Level22 - FlashFadeDown
		Else
			Flash2Level22 = FlashLevel22
			me.timerenabled = False
		End If
	End If

	if Flash2Level22 >  0 Then
		Flasher1R.visible = 1
		Flasher1R1.visible = 1
		Flasher1R2.visible = 1
		Flash103wall.visible = 1
		Flasher1R.IntensityScale = Flash2Level22 / 255
		Flasher1R1.IntensityScale = Flash2Level22 / 255
		Flasher1R2.IntensityScale = Flash2Level22 / 255
		Flash103wall.IntensityScale = Flash2Level22 / 255
	else
		Flasher1R.visible = 0
		Flasher1R1.visible = 0
		Flasher1R2.visible = 0
		Flash103wall.visible = 0
	end if

	if Flash2Level22 > 0 Then
		flasher1.image="flasher1ron"
		flasher1.blenddisablelighting=1
		if Gi3IsOff Then
			metalramp_prim.image="metalrampGIOFfflash"
		Else
			metalramp_prim.image="metalrampflash"
		end if
	else
		if Gi3IsOff Then
			metalramp_prim.image="metalrampGIOFf"
		Else
			metalramp_prim.image="metalrampGIOn"
		end if
		flasher1.image="flasher1roffgion"
		flasher1.blenddisablelighting=.8
	end if
End Sub

' ********* Flasher 1 (L) **********

'	FadeLights.obj(110) = array(flasherb1a,flasherb1b,Flasherplateb1,Flash110wall)
Sub Flasherset23(value)
	if value < FlashLevel23 Then
		FlashLevel23dir =  -1
	elseif value > FlashLevel23 Then
		FlashLevel23dir = 1
	end If

	FlashLevel23 = value 
	Flasherb1a.Timerenabled = true 
End Sub 

Sub Flasherb1a_timer()
	me.timerinterval = FlasherInterval

	If FlashLevel23dir > 0 Then
		If Flash2Level23 < FlashLevel23 Then
			Flash2Level23 = Flash2Level23 + FlashFadeUp
		Else
			Flash2Level23 = FlashLevel23
			me.timerenabled = False
		End If
	Else
		If Flash2Level23 > FlashLevel23 Then
			Flash2Level23 = Flash2Level23 - FlashFadeDown
		Else
			Flash2Level23 = FlashLevel23
			me.timerenabled = False
		End If
	End If

	if Flash2Level23 >  0 Then
		flasherb1a.visible = 1
		flasherb1b.visible = 1
		Flasherplateb1.visible = 1
		Flash110wall.visible = 1
		flasherb1a.IntensityScale = Flash2Level23 / 255
		flasherb1b.IntensityScale = Flash2Level23 / 255
		Flasherplateb1.IntensityScale = Flash2Level23 / 255
		Flash110wall.IntensityScale = Flash2Level23 / 255
	else
		flasherb1a.visible = 0
		flasherb1b.visible = 0
		Flasherplateb1.visible = 0
		Flash110wall.visible = 0
	end if

	if Flash2Level23 > 0 Then
		if Flash2Level23/255 < 0.2 Then
			flasherb1.image="flashback_on1"
		elseif Flash2Level23/255 < 0.4 Then
			flasherb1.image="flashback_on2"
		elseif Flash2Level23/255 < 0.6 Then
			flasherb1.image="flashback_on3"
		elseif Flash2Level23/255 < 0.8 Then
			flasherb1.image="flashback_on4"
		else
			flasherb1.image="flashback_on5"
		end if
	else
		flasherb1.image="flashersoff"
	end if
End Sub

' ********* Flasher 2 (LC) **********

'	FadeLights.obj(111) = array(flasherb2a,flasherb2b,Flasherplateb2)
Sub Flasherset24(value)
	if value < FlashLevel24 Then
		FlashLevel24dir =  -1
	elseif value > FlashLevel24 Then
		FlashLevel24dir = 1
	end If

	FlashLevel24 = value 
	Flasherb2a.Timerenabled = true 
End Sub 

Sub Flasherb2a_timer()
	me.timerinterval = FlasherInterval

	If FlashLevel24dir > 0 Then
		If Flash2Level24 < FlashLevel24 Then
			Flash2Level24 = Flash2Level24 + FlashFadeUp
		Else
			Flash2Level24 = FlashLevel24
			me.timerenabled = False
		End If
	Else
		If Flash2Level24 > FlashLevel24 Then
			Flash2Level24 = Flash2Level24 - FlashFadeDown
		Else
			Flash2Level24 = FlashLevel24
			me.timerenabled = False
		End If
	End If

	if Flash2Level24 >  0 Then
		flasherb2a.visible = 1
		flasherb2b.visible = 1
		Flasherplateb2.visible = 1
		flasherb2a.IntensityScale = Flash2Level24 / 255
		flasherb2b.IntensityScale = Flash2Level24 / 255
		Flasherplateb2.IntensityScale = Flash2Level24 / 255
	else
		flasherb2a.visible = 0
		flasherb2b.visible = 0
		Flasherplateb2.visible = 0
	end if

	if Flash2Level24 > 0 Then
		if Flash2Level24/255 < 0.2 Then
			flasherb2.image="flashback_on1"
		elseif Flash2Level24/255 < 0.4 Then
			flasherb2.image="flashback_on2"
		elseif Flash2Level24/255 < 0.6 Then
			flasherb2.image="flashback_on3"
		elseif Flash2Level24/255 < 0.8 Then
			flasherb2.image="flashback_on4"
		else
			flasherb2.image="flashback_on5"
		end if
	else
		flasherb2.image="flashersoff"
	end if
End Sub

' ********* Flasher 3 (C) **********

'	FadeLights.obj(112) = array(flasherb3a,flasherb3b,Flasherplateb3)
Sub Flasherset25(value)
	if value < FlashLevel25 Then
		FlashLevel25dir =  -1
	elseif value > FlashLevel25 Then
		FlashLevel25dir = 1
	end If

	FlashLevel25 = value 
	Flasherb3a.Timerenabled = true 
End Sub 

Sub Flasherb3a_timer()
	me.timerinterval = FlasherInterval

	If FlashLevel25dir > 0 Then
		If Flash2Level25 < FlashLevel25 Then
			Flash2Level25 = Flash2Level25 + FlashFadeUp
		Else
			Flash2Level25 = FlashLevel25
			me.timerenabled = False
		End If
	Else
		If Flash2Level25 > FlashLevel25 Then
			Flash2Level25 = Flash2Level25 - FlashFadeDown
		Else
			Flash2Level25 = FlashLevel25
			me.timerenabled = False
		End If
	End If

	if Flash2Level25 >  0 Then
		flasherb3a.visible = 1
		flasherb3b.visible = 1
		Flasherplateb3.visible = 1
		flasherb3a.IntensityScale = Flash2Level25 / 255
		flasherb3b.IntensityScale = Flash2Level25 / 255
		Flasherplateb3.IntensityScale = Flash2Level25 / 255
	else
		flasherb3a.visible = 0
		flasherb3b.visible = 0
		Flasherplateb3.visible = 0
	end if

	if Flash2Level25 > 0 Then
		if Flash2Level25/255 < 0.2 Then
			flasherb3.image="flashback_on1"
		elseif Flash2Level25/255 < 0.4 Then
			flasherb3.image="flashback_on2"
		elseif Flash2Level25/255 < 0.6 Then
			flasherb3.image="flashback_on3"
		elseif Flash2Level25/255 < 0.8 Then
			flasherb3.image="flashback_on4"
		else
			flasherb3.image="flashback_on5"
		end if
	else
		flasherb3.image="flashersoff"
	end if
End Sub


' ********* Flasher 4 (RC) **********

'	FadeLights.obj(113) = array(flasherb4a,flasherb4b,Flasherplateb4)
Sub Flasherset26(value)
	if value < FlashLevel26 Then
		FlashLevel26dir =  -1
	elseif value > FlashLevel26 Then
		FlashLevel26dir = 1
	end If

	FlashLevel26 = value 
	Flasherb4a.Timerenabled = true 
End Sub 

Sub Flasherb4a_timer()
	me.timerinterval = FlasherInterval

	If FlashLevel26dir > 0 Then
		If Flash2Level26 < FlashLevel26 Then
			Flash2Level26 = Flash2Level26 + FlashFadeUp
		Else
			Flash2Level26 = FlashLevel26
			me.timerenabled = False
		End If
	Else
		If Flash2Level26 > FlashLevel26 Then
			Flash2Level26 = Flash2Level26 - FlashFadeDown
		Else
			Flash2Level26 = FlashLevel26
			me.timerenabled = False
		End If
	End If

	if Flash2Level26 >  0 Then
		flasherb4a.visible = 1
		flasherb4b.visible = 1
		Flasherplateb4.visible = 1
		flasherb4a.IntensityScale = Flash2Level26 / 255
		flasherb4b.IntensityScale = Flash2Level26 / 255
		Flasherplateb4.IntensityScale = Flash2Level26 / 255
	else
		flasherb4a.visible = 0
		flasherb4b.visible = 0
		Flasherplateb4.visible = 0
	end if

	if Flash2Level26 > 0 Then
		if Flash2Level26/255 < 0.2 Then
			flasherb4.image="flashback_on1"
		elseif Flash2Level26/255 < 0.4 Then
			flasherb4.image="flashback_on2"
		elseif Flash2Level26/255 < 0.6 Then
			flasherb4.image="flashback_on3"
		elseif Flash2Level26/255 < 0.8 Then
			flasherb4.image="flashback_on4"
		else
			flasherb4.image="flashback_on5"
		end if
	else
		flasherb4.image="flashersoff"
	end if
End Sub


' ********* Flasher 5 (R) **********

'	FadeLights.obj(114) = array(flasherb5a,flasherb5b,Flasherplateb5,Flash114wall)
Sub Flasherset27(value)
	if value < FlashLevel27 Then
		FlashLevel27dir =  -1
	elseif value > FlashLevel27 Then
		FlashLevel27dir = 1
	end If

	FlashLevel27 = value 
	Flasherb5a.Timerenabled = true 
End Sub 

Sub Flasherb5a_timer()
	me.timerinterval = FlasherInterval

	If FlashLevel27dir > 0 Then
		If Flash2Level27 < FlashLevel27 Then
			Flash2Level27 = Flash2Level27 + FlashFadeUp
		Else
			Flash2Level27 = FlashLevel27
			me.timerenabled = False
		End If
	Else
		If Flash2Level27 > FlashLevel27 Then
			Flash2Level27 = Flash2Level27 - FlashFadeDown
		Else
			Flash2Level27 = FlashLevel27
			me.timerenabled = False
		End If
	End If

	dim flashx3 : flashx3 = (FlashLevel27/255)

	if flashx3 >  0 Then
		flasherb5a.visible = 1
		flasherb5b.visible = 1
		Flasherplateb5.visible = 1
		Flash114wall.visible = 1
		flasherb5a.IntensityScale = flashx3
		flasherb5b.IntensityScale = flashx3
		Flasherplateb5.IntensityScale = flashx3
		Flash114wall.IntensityScale = flashx3
	else
		flasherb5a.visible = 0
		flasherb5b.visible = 0
		Flasherplateb5.visible = 0
		Flash114wall.visible = 0
	end if

	if flashx3 > 0 Then
		if flashx3 < 0.2 Then
			flasherb5.image="flashback_on1"
		elseif flashx3 < 0.4 Then
			flasherb5.image="flashback_on2"
		elseif flashx3 < 0.6 Then
			flasherb5.image="flashback_on3"
		elseif flashx3 < 0.8 Then
			flasherb5.image="flashback_on4"
		else
			flasherb5.image="flashback_on5"
		end if
	else
		flasherb5.image="flashersoff"
	end if
End Sub


'*****************************************
'      		General Illumination
'*****************************************
Set GiCallBack2 = GetRef("UpdateGi")

Sub UpdateGi(nr,step)
	Dim ii
	Select Case nr

	Case 0		'Bottom Playfield
		If step=0 Then
			setlamp 115,0
			setlamp 116,1
			For each ii in GI_Bottom:ii.state=0:Next
			gi0isoff=true
			For each ii in rubber_bottom: ii.image = "rubbasGIOFF": Next
			laneguideL.image="metallaneguideloff"
			laneguideR.image="metallaneguideroff"
			acorns.image="acornsoff"
			metals_table.image="metalpostsoff"
			outerinner_prim.image="woodguideoff"
			apronrail.image="apronrailoff"
			rightsign.image="rightsignoff"
		Else
			setlamp 115,1
			setlamp 116,0
			For each ii in GI_Bottom:ii.state=1:Next
			gi0isoff=false
			For each ii in rubber_bottom: ii.image = "rubbasGION": Next
			laneguideL.image="metallaneguidelon"
			laneguideR.image="metallaneguideron"
			acorns.image="acornson"
			metals_table.image="metalpostson"
			outerinner_prim.image="woodguideon"
			apronrail.image="apronrailon"
			rightsign.image="rightsignon"
		End If
		For each ii in GI_Bottom:ii.IntensityScale = 0.125 * step:Next

	Case 1		'Left Playfield
		If step=0 Then
			For each ii in GI_Left:ii.state=0:Next
			If VRRoom = 1 Then
				outerl.image="outerloffVR"
				Else
				outerl.image="outerleftoff"
			End If				
			gi1isoff=true
			psw18.image="drop1off"
			psw17.image="drop1off"
			psw16.image="drop1off"
			If VRRoom = 1 Then
				outerl.image="outerloffVR"
				Else
				outerl.image="outerleftoff"
			End If
			Flasher1L.timerenabled=1'flasher2.image="flasher1loffgioff"
			Flasher2L.timerenabled=1'flasher3.image="flasher2offgioff"
			For each ii in rubber_left: ii.image = "rubbasGIOFF": Next
		Else
			For each ii in GI_Left:ii.state=1:Next
			If VRRoom = 1 Then
				outerl.image="outerleftonVR"
				Else
				outerl.image="outerlefton"
			End If
			gi1isoff=false
			psw18.image="drop3on"
			psw17.image="drop2on"
			psw16.image="drop1on"
			If VRRoom = 1 Then
				outerl.image="outerleftonVR"
				Else
				outerl.image="outerlefton"
			End If
			Flasher1L.timerenabled=1'flasher2.image="flasher1loffgion"
			Flasher2L.timerenabled=1'flasher3.image="flasher2offgion"
			For each ii in rubber_left: ii.image = "rubbasGION": Next
		End If
		For each ii in GI_Left:ii.IntensityScale = 0.125 * step:Next

	Case 2 		'Upper Playfield
		If step=0 Then
			For each ii in GI_Upper:ii.state=0:Next
			gi2isoff=true
			For each ii in rubber_upper: ii.image = "rubbasGIOFF": Next
			rubberposts.image="rubberpostsoff"
			helmet_prim.blenddisablelighting=0.05
			targetwrapper_prim.blenddisablelighting=0.05
			corkscrew_prim.image="corkscrewoff"
			Prim_upperPF.image="upperPFGIOFF"
		Else
			For each ii in GI_Upper:ii.state=1:Next
			gi2isoff=false
			For each ii in rubber_upper: ii.image = "rubbasGION": Next
			rubberposts.image="rubberpostson"
			helmet_prim.blenddisablelighting=.25
			targetwrapper_prim.blenddisablelighting=0.14
			corkscrew_prim.image="corkscrewon"
			Prim_upperPF.image="upperPFGION"
		End If
		For each ii in GI_Upper:ii.IntensityScale = 0.125 * step:Next

	Case 3 		'Right Playfield
		If step=0 Then
			For each ii in GI_Right:ii.state=0:Next
			gi3isoff=true
			sw41_prim.blenddisablelighting=0
			sw42_prim.blenddisablelighting=0
			sw43_prim.blenddisablelighting=0
			sw44_prim.blenddisablelighting=0
			sw45_prim.blenddisablelighting=0
			flasher1.image="flasher1roffgioff"
			For each ii in rubber_right: ii.image = "rubbasGIOFF": Next
			upperrubbers.image="upperpfrubbersoff"
			metalramp_prim.image="metalrampGIOFF"
			If VRRoom = 1 Then
				outerr.image="outerroffVR"
				Else
				outerr.image="outerrightoff"
			End If
			r18_prim.image="18off"
		Else
			For each ii in GI_Right:ii.state=1:Next
			gi3isoff=false
			sw41_prim.blenddisablelighting=0.1
			sw42_prim.blenddisablelighting=0.1
			sw43_prim.blenddisablelighting=0.1
			sw44_prim.blenddisablelighting=0.1
			sw45_prim.blenddisablelighting=0.1
			flasher1.image="flasher1roffgion"
			For each ii in rubber_right: ii.image = "rubbasGION": Next
			upperrubbers.image="upperpfrubberson"
			metalramp_prim.image="metalrampGION"
			If VRRoom = 1 Then
				outerr.image="outerronVR"
				Else
				outerr.image="outerrighton"
			End If
			r18_prim.image="18on"
		End If
		For each ii in GI_Right:ii.IntensityScale = 0.125 * step:Next
	End Select
	UpdateLamp71
	UpdateLamp72
	UpdateLamp73
	UpdateLamp74
	UpdateLamp75
	UpdateLamp76

	If VRRoom = 1 Then
		If Gi1IsOff = True Then
			PinCab_Backglass.image="Backglassdark"
			Else
			PinCab_Backglass.image="Backglassilluminated"
		End If
	End If

' Start WJR Comment
'	UpdateLamp101
'	UpdateLamp102
'	UpdateLamp103
'	UpdateLamp104
''	UpdateLamp105
'	UpdateLamp106
''	UpdateLamp107
'	UpdateLamp108
''	UpdateLamp109
'	UpdateLamp110
'	UpdateLamp111
'	UpdateLamp112
'	UpdateLamp113
'	UpdateLamp114
' End WJR Comment

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
		Controller.Switch(15)=0
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
		Controller.Switch(15)=1
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

'Knocker
Sub SolKnocker(Enabled)
        If enabled Then
                KnockerSolenoid
        End If
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
		PlaySound SoundFX("gunmotor",DOFGear), 0, 1, AudioPan(sw43), 0,0, 1, 0, AudioFade( sw43)
	Else
		Stopsound "gunmotor"
	end if
 End Sub


'**********************************************************************************************************

 ' Drain hole and kickers
Sub Drain_Hit:bsTrough.addball me : RandomSoundDrain drain : End Sub
Sub sw47_Hit:bsLEye.AddBall 0 : SoundSaucerLock : End Sub
Sub sw47_UnHit: SoundSaucerKick 1, sw47 : End Sub
Sub sw48_Hit:bsREye.AddBall 0 : SoundSaucerLock : End Sub
Sub sw48_UnHit: SoundSaucerKick 1, sw48 : End Sub
Sub sw46_Hit:bsSaucer.AddBall 0 : SoundSaucerLock : End Sub
Sub sw46_UnHit: SoundSaucerKick 1, sw46 : End Sub

Sub Ballrelease_UnHit : RandomSoundBallRelease ballrelease : End Sub

dim TargetTransY, TargetDelay
TargetTransY = -8
TargetDelay = 75

'Standup targets
Sub sw67_Hit:vpmTimer.PulseSw 67:End Sub
Sub sw41_Hit:vpmTimer.PulseSw 41:sw41_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw41_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw41_prim.transy=0'":End Sub
Sub sw42_Hit:vpmTimer.PulseSw 42:sw42_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw42_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw42_prim.transy=0'":End Sub
Sub sw43_Hit:vpmTimer.PulseSw 43:sw43_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw43_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw43_prim.transy=0'":End Sub
Sub sw44_Hit:vpmTimer.PulseSw 44:sw44_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw44_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw44_prim.transy=0'":End Sub
Sub sw45_Hit:vpmTimer.PulseSw 45:sw45_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw45_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw45_prim.transy=0'":End Sub
Sub sw51_Hit:vpmTimer.PulseSw 51:sw51_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw51_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw51_prim.transy=0'":End Sub
Sub sw52_Hit:vpmTimer.PulseSw 52:sw52_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw52_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw52_prim.transy=0'":End Sub
Sub sw53_Hit:vpmTimer.PulseSw 53:sw53_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw53_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw53_prim.transy=0'":End Sub
Sub sw54_Hit:vpmTimer.PulseSw 54:sw54_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw54_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw54_prim.transy=0'":End Sub
Sub sw55_Hit:vpmTimer.PulseSw 55:sw55_prim.transy=TargetTransY:vpmTimer.AddTimer TargetDelay,"sw55_prim.transy=TargetTransY/2'":vpmTimer.AddTimer TargetDelay+TargetDelay,"sw55_prim.transy=0'":End Sub
Sub sw38_Hit:vpmTimer.PulseSw 38:End Sub

'Combo Target Hits
Sub sw4142_Hit:sw41_Hit:sw42_Hit:End Sub
Sub sw4243_Hit:sw42_Hit:sw43_Hit:End Sub
Sub sw4344_Hit:sw43_Hit:sw44_Hit:End Sub
Sub sw4445_Hit:sw44_Hit:sw45_Hit:End Sub

Sub sw5152_Hit:sw51_Hit:sw52_Hit:End Sub
Sub sw5253_Hit:sw52_Hit:sw53_Hit:End Sub
Sub sw5354_Hit:sw53_Hit:sw54_Hit:End Sub
Sub sw5455_Hit:sw54_Hit:sw55_Hit:End Sub

''Drop Targets

Sub Sw16_Hit:DTHit 16:End Sub
Sub Sw17_Hit:DTHit 17:End Sub
Sub Sw18_Hit:DTHit 18 End Sub

'Wire Triggers
Sub sw25_Hit:Controller.Switch(25) = 1:End Sub
Sub sw25_UnHit:Controller.Switch(25) = 0:End Sub
Sub sw26_Hit:Controller.Switch(26) = 1:End Sub
Sub sw26_UnHit:Controller.Switch(26) = 0:End Sub
Sub sw27_Hit:Controller.Switch(27) = 1:End Sub
Sub sw27_UnHit:Controller.Switch(27) = 0:End Sub
Sub sw28_Hit:Controller.Switch(28) = 1:End Sub
Sub sw28_UnHit:Controller.Switch(28) = 0:End Sub
Sub sw56_Hit:me.TimerInterval=100:me.TimerEnabled = 1:End Sub  'Debug.Print "sw56":
Sub sw56_Timer:vpmTimer.PulseSw 56:me.TimerEnabled = 0:End Sub 
Sub sw57_Hit:vpmTimer.PulseSw 57:End Sub  'Debug.Print "sw57":
Sub sw58_Hit:Controller.Switch(58) = 1:End Sub
Sub sw58_UnHit:Controller.Switch(58) = 0:End Sub
Sub rampout_Hit:Controller.Switch(36) = 1:End Sub
Sub rampout_UnHit:Controller.Switch(36) = 0:End Sub
Sub rampin_Hit:Controller.Switch(37) = 1:End Sub
Sub rampin_UnHit:Controller.Switch(37) = 0:End Sub
Sub sw68_Hit:Controller.Switch(68) = 1 : BIPL=1: End Sub
Sub sw68_UnHit:Controller.Switch(68) = 0 : BIPL=0 : End Sub


'Bumpers
Sub sw62_bumper2_Hit : vpmTimer.PulseSw(62) : RandomSoundBumperMiddle sw62_bumper2: End Sub
Sub sw61_bumper1_Hit : vpmTimer.PulseSw(61) : RandomSoundBumperTop sw61_bumper1: End Sub
Sub sw63_bumper3_Hit : vpmTimer.PulseSw(63) : RandomSoundBumperBottom sw63_bumper3: End Sub

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep, LaStep, LaaStep, LbStep, LcStep, LccStep, LcccStep, RaStep, RbStep, RcStep, RccStep, RcccStep, ReStep, RfStep, RffStep, RgStep, RhStep, RiStep, RjStep, RkStep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 64
    RandomSoundSlingshotLeft SLING1
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
	vpmTimer.PulseSw 65
    RandomSoundSlingshotRight SLING2
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
    WallLA.TimerEnabled = 1
End Sub
 
Sub WallLA_Timer
    Select Case LAstep
        Case 3:RubberLA3.Visible = 0:RubberLA4.Visible = 1
        Case 4:RubberLA4.Visible = 0:RubberLA.Visible = 1:WallLA.TimerEnabled = 0:
    End Select
    LAstep = LAstep + 1
End Sub

Sub WallLAa_Hit
	vpmTimer.PulseSw 11
    RubberLA.Visible = 0
    RubberLA1.Visible = 1
    LAastep = 0
    WallLAa.TimerEnabled = 1
End Sub
 
Sub WallLAa_Timer
    Select Case LAastep
        Case 3:RubberLA1.Visible = 0:RubberLA2.Visible = 1
        Case 4:RubberLA2.Visible = 0:RubberLA.Visible = 1:WallLAa.TimerEnabled = 0:
    End Select
    LAastep = LAastep + 1
End Sub

Sub WallLb_Hit
    RubberLb.Visible = 0
    RubberLb1.Visible = 1
    Lbstep = 0
    WallLb.TimerEnabled = 1
End Sub
 
Sub WallLb_Timer
    Select Case Lbstep
        Case 3:RubberLb1.Visible = 0:RubberLb2.Visible = 1
        Case 4:RubberLb2.Visible = 0:RubberLb.Visible = 1:WallLb.TimerEnabled = 0:
    End Select
    Lbstep = Lbstep + 1
End Sub

Sub WallLc_Hit
    RubberLc.Visible = 0
    RubberLc1.Visible = 1
    Lcstep = 0
    WallLc.TimerEnabled = 1
End Sub
 
Sub WallLc_Timer
    Select Case Lcstep
        Case 3:RubberLc1.Visible = 0:RubberLc2.Visible = 1
        Case 4:RubberLc2.Visible = 0:RubberLc.Visible = 1:WallLc.TimerEnabled = 0:
    End Select
    Lcstep = Lcstep + 1
End Sub

Sub Walllcc_Hit
	vpmTimer.PulseSw 12
    RubberLc.Visible = 0
    RubberLc3.Visible = 1
    lccstep = 0
    Walllcc.TimerEnabled = 1
End Sub
 
Sub Walllcc_Timer
    Select Case lccstep
        Case 3:RubberLc3.Visible = 0:RubberLc4.Visible = 1
        Case 4:RubberLc4.Visible = 0:RubberLc.Visible = 1:WallLc.TimerEnabled = 0:
    End Select
    lccstep = lccstep + 1
End Sub

Sub Walllccc_Hit
	vpmTimer.PulseSw 12
    RubberLc.Visible = 0
    RubberLc5.Visible = 1
    lcccstep = 0
    Walllccc.TimerEnabled = 1
End Sub
 
Sub Walllccc_Timer
    Select Case lcccstep
        Case 3:RubberLc5.Visible = 0:RubberLc6.Visible = 1
        Case 4:RubberLc6.Visible = 0:RubberLc.Visible = 1:WallLc.TimerEnabled = 0:
    End Select
    lcccstep = lcccstep + 1
End Sub

Sub Wallra_Hit
    Rubberra.Visible = 0
    Rubberra1.Visible = 1
    rastep = 0
    Wallra.TimerEnabled = 1
End Sub
 
Sub Wallra_Timer
    Select Case rastep
        Case 3:Rubberra1.Visible = 0:Rubberra2.Visible = 1
        Case 3:Rubberra2.Visible = 0:Rubberra3.Visible = 1
        Case 4:Rubberra3.Visible = 0:Rubberra.Visible = 1:Wallra.TimerEnabled = 0:
    End Select
    rastep = rastep + 1
End Sub

Sub Wallrb_Hit
	vpmTimer.PulseSw 66
    Rubberrb.Visible = 0
    Rubberrb1.Visible = 1
    rbstep = 0
    Wallrb.TimerEnabled = 1
End Sub
 
Sub Wallrb_Timer
    Select Case rbstep
        Case 3:Rubberrb1.Visible = 0:Rubberrb2.Visible = 1
        Case 3:Rubberrb2.Visible = 0:Rubberrb3.Visible = 1
        Case 4:Rubberrb3.Visible = 0:Rubberrb.Visible = 1:Wallrb.TimerEnabled = 0:
    End Select
    rbstep = rbstep + 1
End Sub

Sub Wallrc_Hit
    Rubberrc.Visible = 0
    Rubberrc1.Visible = 1
    rcstep = 0
    Wallrc.TimerEnabled = 1
End Sub
 
Sub Wallrc_Timer
    Select Case rcstep
        Case 3:Rubberrc1.Visible = 0:Rubberrc2.Visible = 1
        Case 4:Rubberrc2.Visible = 0:Rubberrc.Visible = 1:Wallrc.TimerEnabled = 0:
    End Select
    rcstep = rcstep + 1
End Sub

Sub Wallrcc_Hit
    Rubberrc.Visible = 0
    Rubberrc3.Visible = 1
    rccstep = 0
    Wallrcc.TimerEnabled = 1
End Sub
 
Sub Wallrcc_Timer
    Select Case rccstep
        Case 3:Rubberrc3.Visible = 0:Rubberrc4.Visible = 1
        Case 4:Rubberrc4.Visible = 0:Rubberrc.Visible = 1:Wallrc.TimerEnabled = 0:
    End Select
    rccstep = rccstep + 1
End Sub

Sub Wallrccc_Hit
    Rubberrc.Visible = 0
    Rubberrc5.Visible = 1
    rcccstep = 0
    Wallrccc.TimerEnabled = 1
End Sub
 
Sub Wallrccc_Timer
    Select Case rcccstep
        Case 3:Rubberrc5.Visible = 0:Rubberrc6.Visible = 1
        Case 4:Rubberrc6.Visible = 0:Rubberrc.Visible = 1:Wallrc.TimerEnabled = 0:
    End Select
    rcccstep = rcccstep + 1
End Sub

Sub Wallre_Hit
    Rubberre.Visible = 0
    Rubberre1.Visible = 1
    restep = 0
    Wallre.TimerEnabled = 1
End Sub
 
Sub Wallre_Timer
    Select Case restep
        Case 3:Rubberre1.Visible = 0:Rubberre2.Visible = 1
        Case 3:Rubberre2.Visible = 0:Rubberre3.Visible = 1
        Case 4:Rubberre3.Visible = 0:Rubberre.Visible = 1:Wallre.TimerEnabled = 0:
    End Select
    restep = restep + 1
End Sub

Sub Wallrf_Hit
    Rubberrf.Visible = 0
    Rubberrf1.Visible = 1
    rfstep = 0
    Wallrf.Timerenabled = 1
End Sub
 
Sub Wallrf_Timer
    Select Case rfstep
        Case 3:Rubberrf1.Visible = 0:Rubberrf2.Visible = 1
        Case 3:Rubberrf2.Visible = 0:Rubberrf3.Visible = 1
        Case 4:Rubberrf3.Visible = 0:Rubberrf.Visible = 1:Wallrf.Timerenabled = 0:
    End Select
    rfstep = rfstep + 1
End Sub

Sub Wallrg_Hit
    Rubberrg.Visible = 0
    Rubberrg1.Visible = 1
    rgstep = 0
    Wallrg.TimerEnabled = 1
End Sub
 
Sub Wallrg_Timer
    Select Case rgstep
        Case 3:Rubberrg1.Visible = 0:Rubberrg2.Visible = 1
        Case 4:Rubberrg2.Visible = 0:Rubberrg.Visible = 1:Wallrg.TimerEnabled = 0:
    End Select
    rgstep = rgstep + 1
End Sub

Sub Wallrh_Hit
    Rubberrh.Visible = 0
    Rubberrh1.Visible = 1
    rhstep = 0
    Wallrh.TimerEnabled = 1
End Sub
 
Sub Wallrh_Timer
    Select Case rhstep
        Case 3:Rubberrh1.Visible = 0:Rubberrh2.Visible = 1
        Case 4:Rubberrh2.Visible = 0:Rubberrh.Visible = 1:Wallrh.TimerEnabled = 0:
    End Select
    rhstep = rhstep + 1
End Sub

Sub Wallri_Hit
    Rubberri.Visible = 0
    Rubberri1.Visible = 1
    ristep = 0
    Wallri.TimerEnabled = 1
End Sub
 
Sub Wallri_Timer
    Select Case ristep
        Case 3:Rubberri1.Visible = 0:Rubberri2.Visible = 1
        Case 4:Rubberri2.Visible = 0:Rubberri.Visible = 1:Wallri.TimerEnabled = 0:
    End Select
    ristep = ristep + 1
End Sub

Sub Wallrj_Hit
    Rubberrj.Visible = 0
    Rubberrj1.Visible = 1
    rjstep = 0
    Wallrj.TimerEnabled = 1
End Sub
 
Sub Wallrj_Timer
    Select Case rjstep
        Case 3:Rubberrj1.Visible = 0:Rubberrj2.Visible = 1
        Case 4:Rubberrj2.Visible = 0:Rubberrj.Visible = 1:Wallrj.TimerEnabled = 0:
    End Select
    rjstep = rjstep + 1
End Sub

'Sub Wallrk_Hit
'    Rubberrk.Visible = 0
'    Rubberrk1.Visible = 1
'    rkstep = 0
'    Wallrk.TimerEnabled = 1
'End Sub
' 
'Sub Wallrk_Timer
'    Select Case rkstep
'        Case 3:Rubberrk1.Visible = 0:Rubberrk2.Visible = 1
'        Case 4:Rubberrk2.Visible = 0:Rubberrk.Visible = 1:Wallrk.TimerEnabled = 0:
'    End Select
'    rkstep = rkstep + 1
'End Sub

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
	UpdateGI 0,0:UpdateGI 1,0:UpdateGI 2,0:UpdateGI 3,0
	'FadeLights.Callback(120)="RampCallback "
	FadeLights.obj(11) = array(l11, l11a)
	FadeLights.obj(12) = array(l12, l12a)
	FadeLights.obj(13) = array(l13, l13a)
	FadeLights.obj(14) = array(l14, l14a)
	FadeLights.obj(15) = array(l15, l15a)
	FadeLights.obj(16) = array(l16, l16a)
	FadeLights.obj(17) = array(l17, l17a,f17)
	FadeLights.obj(18) = array(l18, l18a)
	FadeLights.obj(21) = array(l21, l21a)
	FadeLights.obj(22) = array(l22, l22a)
	FadeLights.obj(23) = array(l23, l23a)
	FadeLights.obj(24) = array(l24, l24a)
	FadeLights.obj(25) = array(l25, l25a)
	FadeLights.obj(26) = array(l26, l26a)
	FadeLights.obj(27) = array(l27, l27a)
	FadeLights.obj(28) = array(l28, l28a)
	FadeLights.obj(31) = array(l31, l31a)
	FadeLights.obj(32) = array(l32, l32a)
	FadeLights.obj(33) = array(l33, l33a)
	FadeLights.obj(34) = array(l34, l34a)
	FadeLights.obj(35) = array(l35, l35a)
	FadeLights.obj(36) = array(l36, l36a)
	FadeLights.obj(37) = array(l37, l37a)
	FadeLights.obj(38) = array(l38, l38a)
	FadeLights.obj(41) = array(l41, l41a)
	FadeLights.obj(42) = array(l42, l42a)
	FadeLights.obj(43) = array(l43, l43a)
	FadeLights.obj(44) = array(l44, l44a)
	FadeLights.obj(45) = array(l45, l45a)
	FadeLights.obj(46) = array(l46, l46a)
	FadeLights.obj(47) = array(l47, l47a)
	FadeLights.obj(48) = array(l48, l48a)
	FadeLights.obj(51) = array(l51, l51a)
	FadeLights.obj(52) = array(l52, l52a)
	FadeLights.obj(53) = array(l53, l53a)
	FadeLights.obj(54) = array(l54, l54a)
	FadeLights.obj(55) = array(l55, l55a)
	FadeLights.obj(56) = array(l56, l56a)
	FadeLights.obj(57) = array(l57, l57a)
'	FadeLights.Callback(57) = "UpdateLamp57"
	FadeLights.obj(58) = array(l58, l58a,f58)
	FadeLights.obj(61) = array(l61)
	FadeLights.obj(62) = array(l62)
	FadeLights.obj(63) = array(l63)
	FadeLights.obj(64) = array(l64)
	FadeLights.obj(65) = array(l65)
	FadeLights.obj(66) = array(l66, l66a)
	FadeLights.obj(67) = array(l67, l67a)
	FadeLights.obj(68) = array(l68, l68a)
	FadeLights.obj(71) = array(f71)
	FadeLights.obj(72) = array(f72)
	FadeLights.obj(73) = array(f73)
	FadeLights.obj(74) = array(f74)
'	FadeLights.Callback(72) = "UpdateLamp72"
'	FadeLights.Callback(73) = "UpdateLamp73"
'	FadeLights.Callback(74) = "UpdateLamp74"
'	FadeLights.Callback(75) = "UpdateLamp75" 'Game Saucer
'	FadeLights.Callback(76) = "UpdateLamp76" 'Mega Ramp
	FadeLights.obj(77) = array(l77, l77a)
	FadeLights.obj(78) = array(l78, l78a)
	FadeLights.obj(81) = array(l81, l81a)
	FadeLights.obj(82) = array(l82, l82a)
	FadeLights.obj(83) = array(l83, l83a)
	FadeLights.obj(84) = array(l84, l84a)
	FadeLights.obj(85) = array(l85, l85a)
	FadeLights.obj(86) = array(l86, l86a)
' Start WJR Comment
'	FadeLights.obj(101) = array(flash101) 'right eye flash
'	FadeLights.obj(102) = array(flash102) 'center visor flash
'	FadeLights.obj(103) = array(Flasher1R,Flasher1R1,FLasher1R2,Flash103wall) 'lower right flasher
'	FadeLights.obj(104) = array(flash104) 'left eye flash
''	FadeLights.obj(105) = array(Flasher2TL,Flasher2TL1,Flasher2TL2,Flasher2TL3,Flasher2TR,Flasher2TR1,Flasher2TR2,Flasher2TR3)
'	'FadeLights.obj(106) = array(Flasher1L,Flasher1L1,FLasher1L2,Flasher1R,Flasher1R1,FLasher1R2)
'	FadeLights.obj(106) = array(Flasher1L,Flasher1L1,FLasher1L2,Flash106wall) 'lower left flasher
'	FadeLights.obj(107) = array(l107, l107a, l107z,BumpFlash,FlasherB,Flash107wall)
'	FadeLights.obj(108) = array(Flasher2L,Flasher2L1,Flasher2L2,Flash108wall)
'	FadeLights.obj(109) = array(l109, l109a, l109z, f109)
'	FadeLights.obj(110) = array(flasherb1a,flasherb1b,Flasherplateb1,Flash110wall)
'	FadeLights.obj(111) = array(flasherb2a,flasherb2b,Flasherplateb2)
'	FadeLights.obj(112) = array(flasherb3a,flasherb3b,Flasherplateb3)
'	FadeLights.obj(113) = array(flasherb4a,flasherb4b,Flasherplateb4)
'	FadeLights.obj(114) = array(flasherb5a,flasherb5b,Flasherplateb5,Flash114wall)
' End WJR Comment

'	FadeLights.Callback(115) = "UpdateGIFade"
'	FadeLights.Callback(116) = "UpdateGIFade"
	FadeLights.obj(115) = array(shadowson)
	FadeLights.obj(116) = array(shadowsoff)
End Sub

Sub AllLampsOff
    Dim x
    For x = 0 to 200
        SetLamp x, 0
    Next
End Sub

Sub UpdateLamp71
	if FadeLights.state(71) <> 0 Then
		bulb71.image="rightsignbon"
		'bulb71.blenddisablelighting = 0.85
		bulb71.blenddisablelighting = 1.5
	else
		bulb71.image="rightsignboff"
		bulb71.blenddisablelighting = 0.5
	end if
End Sub

Sub UpdateLamp72
	if FadeLights.state(72) <> 0 Then
		bulb72.image="rightsignbon"
		'bulb72.blenddisablelighting = 0.85
		bulb72.blenddisablelighting = 1.5
	else
		bulb72.image="rightsignboff"
		bulb72.blenddisablelighting = 0.5
	end if
End Sub

Sub UpdateLamp73
	if FadeLights.state(73) <> 0 Then
		bulb73.image="rightsignbon"
		'bulb73.blenddisablelighting = 0.85
		bulb73.blenddisablelighting = 1.5
	else
		bulb73.image="rightsignboff"
		bulb73.blenddisablelighting = 0.5
	end if
End Sub

Sub UpdateLamp74
	if FadeLights.state(74) <> 0 Then
		bulb74.image="rightsignbon"
		'bulb74.blenddisablelighting = 0.85
		bulb74.blenddisablelighting = 1.5
	else
		bulb74.image="rightsignboff"
		bulb74.blenddisablelighting = 0.5
	end if
End Sub

Sub UpdateLamp75
	if FadeLights.state(75) <> 0 Then
		rampsigngreen.image="rampsignon"
		'rampsigngreen.blenddisablelighting=1
		rampsigngreen.blenddisablelighting=1.5
		if FadeLights.state(76) <> 0 Then
			rampsign.image="rampsignallon"
		else
			rampsign.image="rampsigngreen"
		end if
	else
		rampsigngreen.image="rampsignoff"
		rampsigngreen.blenddisablelighting=.5
		if FadeLights.state(76) <> 0 Then
			rampsign.image="rampsignyellow"
		else
			rampsign.image="rampsign"
		end if
	end if
End Sub

Sub UpdateLamp76
	if FadeLights.state(76) <> 0 Then
		rampsignyellow.image="rampsignon"
		'rampsignyellow.blenddisablelighting=1
		rampsignyellow.blenddisablelighting=1.5
		if FadeLights.state(75) <> 0 Then
			rampsign.image="rampsignallon"
		else
			rampsign.image="rampsignyellow"
		end if
	else
		rampsignyellow.image="rampsignoff"
		rampsignyellow.blenddisablelighting=.5
		if FadeLights.state(75) <> 0 Then
			rampsign.image="rampsigngreen"
		else
			rampsign.image="rampsign"
		end if
	end if
End Sub
'
'


Sub SetLamp(nr, value)
	' If the lamp state is not changing, just exit. 
	if FadeLights.state(nr) = value then exit sub

	if nr > 100 and nr < 115 Then
		FadeLights.state(nr) = value/255
	else
		FadeLights.state(nr) = value
	end if
	if nr = 71 then UpdateLamp71
	if nr = 72 then UpdateLamp72
	if nr = 73 then UpdateLamp73
	if nr = 74 then UpdateLamp74
	if nr = 75 then UpdateLamp75
	if nr = 76 then UpdateLamp76

' Start WJR Comment
'	if nr = 101 then UpdateLamp101
'	if nr = 102 then UpdateLamp102
'	if nr = 103 then UpdateLamp103
'	if nr = 104 then UpdateLamp104
''	if nr = 105 then UpdateLamp105
'	if nr = 106 then UpdateLamp106
''	if nr = 107 then UpdateLamp107
'	if nr = 108 then UpdateLamp108
'	if nr = 110 then UpdateLamp110
'	if nr = 111 then UpdateLamp111
'	if nr = 112 then UpdateLamp112
'	if nr = 113 then UpdateLamp113
'	if nr = 114 then UpdateLamp114
' End WJR Comment

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


''*********************************************************************
''                 Positional Sound Playback Functions
''*********************************************************************
'
'' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
'' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
'' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
'Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
'	PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
'End Sub
'
'' Similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
'Sub PlaySoundAt(soundname, tableobj)
'    PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
'End Sub
'
'Sub PlaySoundAtBall(soundname)
'    PlaySoundAt soundname, ActiveBall
'End Sub


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
			Else
				If rolling(b) = True Then
					StopSound("BallRoll_" & b)
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
	corkwire.RotX = gate5.CurrentAngle
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


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
 	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
 	RightFlipperCollide parm
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

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


dim RubbersD : Set RubbersD = new Dampener       	'frubber
RubbersD.name = "Rubbers"
RubbersD.debugOn = False        					'shows info in textbox "TBPout"
RubbersD.Print = False        						'debug, reports in debugger (in vel, out cor)
'cor bounce curve (linear)
'for best results, try to match in-game velocity as closely as possible to the desired curve
'RubbersD.addpoint 0, 0, 0.935        				'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 0.96       					'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.96
RubbersD.addpoint 2, 5.76, 0.967     				'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64     					'there's clamping so interpolate up to 56 at least

dim SleevesD : Set SleevesD = new Dampener      	'this is just rubber but cut down to 85%...
SleevesD.name = "Sleeves"
SleevesD.debugOn = False       						'shows info in textbox "TBPout"
SleevesD.Print = False       						'debug, reports in debugger (in vel, out cor)
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
        Public Print, debugOn 			'tbpOut.text
        public name, Threshold        	'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
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
                                        BOT(b).velx = BOT(b).velx / 1.5
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
'Const PI = 3.1415927

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

' Used for drop targets and stand up targets
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
SOSRampup = 2.5
Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.035

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
		Else
			If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm, 1
        End If
End Sub



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
    
    Dim DT16, DT17, DT18
    
    'Set array with drop target objects
    '
    'DropTargetvar = Array(primary, secondary, prim, swtich, animate)
    '         primary:                         primary target wall to determine drop
    '        secondary:                        wall used to simulate the ball striking a bent or offset target after the initial Hit
    '        prim:                                primitive target used for visuals and animation
    '                                                        IMPORTANT!!! 
    '                                                        rotz must be used for orientation
    '                                                        rotx to bend the target back
    '                                                        transz to move it up and down
    '                                                        the pivot point should be in the center of the target on the x, y and at or below the playfield (0) on z
    '        switch:                                ROM switch number
    '        animate:                        Arrary slot for handling the animation instrucitons, set to 0
    '
    '        Values for annimate: 1 - bend target (hit to primary), 2 - drop target (hit to secondary), 3 - brick target (high velocity hit to secondary), -1 - raise target 
    '
    '        isDropped:  Boolean which determines whether a drop target is dropped. Set to false if they are initially raised, true if initially dropped.

    
    Set DT16 = (new DropTarget)(sw16, sw16offset, psw16, 16, 0, false)
    Set DT17 = (new DropTarget)(sw17, sw17offset, psw17, 17, 0, false)
    Set DT18 = (new DropTarget)(sw18, sw18offset, psw18, 18, 0, false)

    'Add all the Drop Target Arrays to Drop Target Animation Array
    ' DTAnimationArray = Array(DT1, DT2, ....)
    Dim DTArray
    DTArray = Array(DT16, DT17, DT18)
    
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
	DTShadow psw16, dropplate1
	DTShadow psw17, dropplate2
	DTShadow psw18, dropplate3


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
        End If End Function
    
' Used for drop targets
Function Distance(ax,ay,bx,by)
        Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function


Sub ResetDrops(enabled)
     if enabled then
          PlaySoundAt SoundFX(DTResetSound,DOFContactors), cutout_prim1
          DTRaise 16
          DTRaise 17
          DTRaise 18
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


'/////////////////////////////////////////////////////////////////
'                                        End Mechanical Sounds
'/////////////////////////////////////////////////////////////////


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

	Set ScoreFile=FileObj.CreateTextFile(UserDirectory & "JackbotLUT.txt",True)
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
	If Not FileObj.FileExists(UserDirectory & "JackbotLUT.txt") then
		LUTset=12
		Exit Sub
	End if
	Set ScoreFile=FileObj.GetFile(UserDirectory & "JackbotLUT.txt")
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

Dim Object
If VRRoom = 1 Then
	Ramp16.visible=0
	Ramp15.visible=0
	Ramp17.visible=0
	outerl.image = "outerleftonVR"
	outerr.visible = 0
	For each Object in VrRoomStuff : object.visible = 1 : Next
	For each Object in VrCabinet : object.visible = 1 : Next
End If
If VRRoom = 0 Then
	If DesktopMode = True Then 'Show Desktop components
		Ramp16.visible=1
		Ramp15.visible=1
		Else
		Ramp16.visible=0
		Ramp15.visible=0
	End if
	Ramp17.visible=1
	outerl.image = "outerlefton"
	outerr.visible = 1
	For each Object in VrRoomStuff : object.visible = 0 : Next
	For each Object in VrCabinet : object.visible = 0 : Next
	BeerTimer.enabled = 0
	ClockTimer.enabled = 0
End If
