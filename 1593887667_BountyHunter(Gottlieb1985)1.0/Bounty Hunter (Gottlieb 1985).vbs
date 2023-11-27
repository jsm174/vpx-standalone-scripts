Option Explicit
Randomize

'///////////////////////-----General Sound Options-----///////////////////////
'//  VolumeDial:
'//  VolumeDial is the actual global volume multiplier for the mechanical sounds.
'//  Values smaller than 1 will decrease mechanical sounds volume.
'//  Recommended values should be no greater than 1.
Const VolumeDial = 1

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0

Const cSingleLFlip = 0
Const cSingleRFlip = 0

Const cGameName="bountyh",UseSolenoids=2,UseLamps=0,UseGI=0,SSolenoidOn="Solenoid",SSolenoidOff="SolOff"
Const SCoin=""

LoadVPM "01120100","sys80.vbs",3.02

Dim DesktopMode: DesktopMode = Table1.ShowDT

If DesktopMode = True Then 'Show Desktop components
displaytimer.enabled=1
Else
displaytimer.enabled=0
End if

' using table width and height in script slows down the performance
dim tablewidth: tablewidth = Table1.width
dim tableheight: tableheight = Table1.height

Dim BallShadows: Ballshadows=1  		'******************set to 1 to turn on Ball shadows
Dim FlipperShadows: FlipperShadows=1  '***********set to 1 to turn on Flipper shadows

Dim EnableBallControl
EnableBallControl = false 'Change to true to enable manual ball control (or press C in-game) via the arrow keys and B (boost movement) keys

'Solenoid Call backs
'**********************************************************************************************************
SolCallback(1)	= "soldtdrop"
SolCallback(2)	= "soltophole"
SolCallback(5)	= "soldtup"
SolCallback(8)	= "SolKnocker"
SolCallback(9)	= "bsTrough.SolOut"

'******************************************************
'					FLIPPERS
'******************************************************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

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
		RightFlipper001.rotatetoend
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		RightFlipper.RotateToStart
		rightflipper001.rotatetostart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

'******************************************************
'					KNOCKER
'******************************************************
Sub SolKnocker(Enabled)
	If enabled Then
		KnockerSolenoid 'Add knocker position object
	End If
End Sub

'******************************************************
'				SOLENOID CONTROLLED TOYS
'******************************************************

Sub soldtdrop(enabled)
	If enabled Then 
		DTDrop 74
	End If
 End Sub

Sub Soldtup(enabled)
	if enabled then
		PlaySoundAt SoundFX(DTResetSound,DOFContactors), psw74
		DTRaise 74
	end if
End Sub

'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim bsTrough

Sub Table1_Init
	vpmInit Me
'	On Error Resume Next
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Bounty Hunter (Gottlieb 1985)"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 0
		If Err Then MsgBox Err.Description
	End With
	On Error Goto 0
		Controller.SolMask(0)=0
      vpmTimer.AddTimer 2000,"Controller.SolMask(0)=&Hffffffff'" 'ignore all solenoids - then add the timer to renable all the solenoids after 2 seconds
		Controller.Run
	If Err Then MsgBox Err.Description
	On Error Goto 0

	PinMAMETimer.Interval=PinMAMEInterval
	PinMAMETimer.Enabled=1
	vpmNudge.TiltSwitch = 57
	vpmNudge.Sensitivity = 2
	vpmNudge.TiltObj = Array(Bumper1,Bumper2,LeftSlingshot,RightSlingshot)

 	Set bsTrough = New cvpmBallStack
		bsTrough.Initsw 0,67,0,0,0,0,0,0
		bsTrough.Balls = 1
		bsTrough.InitKick BallRelease, 90,5
		randomsoundballrelease drain
		bsTrough.InitExitSnd "",""
		bsTrough.InitEntrySnd "",""
End Sub

Sub Table1_exit()
  Controller.Pause = False
  Controller.Stop
End Sub


'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
	If keycode = LeftFlipperKey Then FlipperActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperActivate RightFlipper, RFPress

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

	If KeyCode = PlungerKey Then Plunger.Pullback:SoundPlungerPull()

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

End Sub

Sub Table1_KeyUp(ByVal KeyCode)

	If keycode = LeftFlipperKey Then FlipperDeActivate LeftFlipper, LFPress
	If keycode = RightFlipperKey Then FlipperDeActivate RightFlipper, RFPress

	If KeyCode = PlungerKey Then
		Plunger.Fire
		If plungelane=1 Then
			SoundPlungerReleaseBall()			'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()			'Plunger release sound when there is no ball in shooter lane
		End If
	End If

	If KeyUpHandler(keycode) Then Exit Sub

    'Manual Ball Control
	If EnableBallControl = 1 Then
		If keycode = 203 Then BCleft = 0	' Left Arrow
		If keycode = 200 Then BCup = 0		' Up Arrow
		If keycode = 208 Then BCdown = 0	' Down Arrow
		If keycode = 205 Then BCright = 0	' Right Arrow
	End If

End Sub

dim plungelane

Sub ballatplunger_hit
	plungelane=1
End Sub

Sub ballatplunger_unhit
	plungelane=0
End Sub

'******************************************************
'			FLIPPER TRICKS
'******************************************************

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
Const EOSTnew = 1 
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
SOSRampup = 2.5
Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.045

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

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
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

'**********************************************************************************************************
'**********************************************************************************************************

 'switches
'**********************************************************************************************************

 ' Drain kickers
 Sub Drain_Hit:RandomSoundDrain drain:bsTrough.AddBall me:End Sub

'******************************************************
'						KICKER
'******************************************************

Sub Kicker_hit()
	SoundSaucerLock
	Controller.Switch(73) = 1
End Sub

Sub Kicker_unhit()
	Controller.Switch(73) = 0
End Sub


Sub SolTopHole(Enabled)
	If Enabled Then
		If controller.Switch(73) = True Then
			SoundSaucerKick 1, Kicker
		Else
			SoundSaucerKick 0, Kicker
		End If
		Kicker.kick 140+Rnd()*2, 15+Rnd()*2
		Kickerprim.rotz = 18
		kicker.timerinterval = 100
		kicker.timerenabled = 1	
	End If
End Sub

Sub Kicker_Timer()
		Kickerprim.rotz =0
		me.timerenabled = 0
End Sub

'Drop Targets
Sub Sw74_Hit:DTHit 74:End Sub

'Leaf switch
Sub sw44a_Hit:VpmTimer.PulseSw 44 : End Sub 
Sub sw44_Hit:VpmTimer.PulseSw 44 : End Sub 

'Stand Up Targets
 Sub Tsw50_Hit: PrimStandupTgtHit 12, Tsw50, PrimT50: vpmTimer.PulseSw 50 : End Sub
 Sub Tsw50_Timer: PrimStandupTgtMove 12, Tsw50, PrimT50: End Sub
 Sub Tsw60_Hit: PrimStandupTgtHit 12, Tsw60, PrimT60: vpmTimer.PulseSw 60 : End Sub
 Sub Tsw60_Timer: PrimStandupTgtMove 12, Tsw60, PrimT60: End Sub
 Sub Tsw70_Hit: PrimStandupTgtHit 12, Tsw70, PrimT70: vpmTimer.PulseSw 70 : End Sub
 Sub Tsw70_Timer: PrimStandupTgtMove 12, Tsw70, PrimT70: End Sub
 Sub Tsw51_Hit: PrimStandupTgtHit 12, Tsw51, PrimT51: vpmTimer.PulseSw 51 : End Sub
 Sub Tsw51_Timer: PrimStandupTgtMove 12, Tsw51, PrimT51: End Sub
 Sub Tsw61_Hit: PrimStandupTgtHit 12, Tsw61, PrimT61: vpmTimer.PulseSw 61 : End Sub
 Sub Tsw61_Timer: PrimStandupTgtMove 12, Tsw61, PrimT61: End Sub
 Sub Tsw71_Hit: PrimStandupTgtHit 12, Tsw71, PrimT71: vpmTimer.PulseSw 71 : End Sub
 Sub Tsw71_Timer: PrimStandupTgtMove 12, Tsw71, PrimT71: End Sub

 'Wire Triggers
 Sub sw40_Hit  : Controller.Switch(40) = 1 : wire005.transz=-6:End Sub
 Sub sw40_UnHit: Controller.Switch(40) = 0 : wire005.transz=0:End Sub
 Sub sw52_Hit  : Controller.Switch(52) = 1 : wire006.transz=-6:End Sub
 Sub sw52_UnHit: Controller.Switch(52) = 0 : wire006.transz=0:End Sub
 Sub sw53_Hit  : Controller.Switch(53) = 1 : wire003.transz=-6:End Sub
 Sub sw53_UnHit: Controller.Switch(53) = 0 : wire003.transz=0:End Sub
 Sub sw54_Hit  : Controller.Switch(54) = 1 : wire002.transz=-6:End Sub
 Sub sw54_UnHit: Controller.Switch(54) = 0 : wire002.transz=0:End Sub
 Sub sw62_Hit  : Controller.Switch(62) = 1 : wire007.transz=-6:End Sub
 Sub sw62_UnHit: Controller.Switch(62) = 0 : wire007.transz=0:End Sub
 Sub sw63_Hit  : Controller.Switch(63) = 1 : wire004.transz=-6:End Sub
 Sub sw63_UnHit: Controller.Switch(63) = 0 : wire004.transz=0:End Sub
 Sub sw64_Hit  : Controller.Switch(64) = 1 : wire001.transz=-6:End Sub
 Sub sw64_UnHit: Controller.Switch(64) = 0 : wire001.transz=0:End Sub
 Sub sw72_Hit  : Controller.Switch(72) = 1 : wire008.transz=-6:End Sub
 Sub sw72_UnHit: Controller.Switch(72) = 0 : wire008.transz=0:End Sub

'Gate Triggers
Sub sw41_Hit:VpmTimer.PulseSw 41:End Sub
Sub sw42_Hit:vpmTimer.PulseSw 42:End Sub

'Bumpers
Sub Bumper1_Hit
	vpmTimer.PulseSw 43
	DOF 103, DOFPulse
	RandomSoundBumperMiddle Bumper1
	bumpskirt1.roty=skirtAY(me,Activeball)
	bumpskirt1.rotx=skirtAX(me,Activeball)
	me.timerinterval = 150
	me.timerenabled=1
End Sub
 
sub bumper1_timer
	bumpskirt1.rotx=0
	bumpskirt1.roty=0
	me.timerenabled=0
end sub

Sub Bumper2_Hit
	vpmTimer.PulseSw 43
	DOF 104, DOFPulse
	RandomSoundBumperMiddle Bumper2
	bumpskirt2.roty=skirtAY(me,Activeball)
	bumpskirt2.rotx=skirtAX(me,Activeball)
	me.timerinterval = 150
	me.timerenabled=1
End Sub
 
sub bumper2_timer
	bumpskirt2.rotx=0
	bumpskirt2.roty=0
	me.timerenabled=0
end sub



'******************************************************
'			SKIRT ANIMATION FUNCTIONS
'******************************************************
' NOTE: set bumper object timer to around 150-175 in order to be able
' to actually see the animation, adjust to your liking

Const PI = 3.1415926
Const SkirtTilt=3		'angle of skirt tilting in degrees

Function SkirtAX(bumper, bumperball)
	skirtAX=cos(skirtA(bumper,bumperball))*(SkirtTilt)		'x component of angle
	if (bumper.y<bumperball.y) then	skirtAX=skirtAX*-1	'adjust for ball hit bottom half
End Function

Function SkirtAY(bumper, bumperball)
	skirtAY=sin(skirtA(bumper,bumperball))*(SkirtTilt)		'y component of angle
	if (bumper.x>bumperball.x) then	skirtAY=skirtAY*-1	'adjust for ball hit left half
End Function

Function SkirtA(bumper, bumperball)
	dim hitx, hity, dx, dy
	hitx=bumperball.x
	hity=bumperball.y

	dy=Abs(hity-bumper.y)					'y offset ball at hit to center of bumper
	if dy=0 then dy=0.0000001
	dx=Abs(hitx-bumper.x)					'x offset ball at hit to center of bumper
	skirtA=(atn(dx/dy)) '/(PI/180)			'angle in radians to ball from center of Bumper1
End Function

'***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************

Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)

InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
LampTimer.Interval = 5 'lamp fading speed
LampTimer.Enabled = 1

' Lamp & Flasher Timers

Sub LampTimer_Timer()
    Dim chgLamp, num, chg, ii
    chgLamp = Controller.ChangedLamps
    If Not IsEmpty(chgLamp) Then
        For ii = 0 To UBound(chgLamp)
            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
			If chgLamp(ii,0) = 6 Then flasher6on chgLamp(ii,1)
			If chgLamp(ii,0) = 7 Then flasher7on chgLamp(ii,1)
			If chgLamp(ii,0) = 8 Then flasher8on chgLamp(ii,1)
			If chgLamp(ii,0) = 9 Then flasher9on chgLamp(ii,1)
        Next
    End If
    UpdateLamps
End Sub

Sub UpdateLamps
'NFadeLm 1, L1
Flash 166, PLAYFIELD_GI1 'GI is inverse of L1
	SetLamp 166, 1-LampState(1)
NFadeLm 3, L3
NFadeLm 6, L6
NFadeLm 6, L6a
NFadeLm 7, L7
NFadeLm 7, L7a
NFadeLm 8, L8
NFadeLm 8, L8a
NFadeLm 9, L9
NFadeLm 9, L9a
NFadeLm 10, L10
NFadeLm 10, L10a
NFadeLm 11, L11
NFadeLm 12, L12
NFadeLm 13, L13
NFadeLm 13, L13a
NFadeLm 14, L14
NFadeLm 15, L15
NFadeLm 16, L16
NFadeLm 17, L17
NFadeLm 17, L17a
NFadeLm 18, L18
NFadeLm 19, L19
NFadeLm 20, L20
NFadeLm 21, L21
NFadeLm 22, L22
NFadeLm 23, L23
NFadeLm 24, L24
NFadeLm 24, L24a
NFadeLm 25, L25
NFadeLm 26, L26
NFadeLm 26, L26a
NFadeLm 27, L27
NFadeLm 28, L28
NFadeLm 29, L29
NFadeLm 30, L30
NFadeLm 31, L31
NFadeLm 31, L32
NFadeLm 33, L33
NFadeLm 34, L34
NFadeLm 35, L35
NFadeLm 36, L36
NFadeLm 37, L37
NFadeLm 38, L38
NFadeLm 39, L39
NFadeLm 40, L40
NFadeLm 41, L41
NFadeLm 42, L42
NFadeLm 43, L43
NFadeLm 44, L44
NFadeLm 45, L45
NFadeLm 46, L46
NFadeLm 47, L47
End Sub

' div lamp subs

Sub InitLamps()
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
End Sub

Sub AllLampsOff
    Dim x
    For x = 0 to 200
        SetLamp x, 0
    Next
End Sub

Sub FadePrim(nr, pri, a, b, c, d)
    Select Case FadingLevel(nr)
        Case 2:pri.image = d:FadingLevel(nr) = 0
        Case 3:pri.image = c:FadingLevel(nr) = 2
        Case 4:pri.image = b:FadingLevel(nr) = 3
        Case 5:pri.image = a:FadingLevel(nr) = 1
    End Select
End Sub

Sub SetLamp(nr, value)
    If value <> LampState(nr) Then
        LampState(nr) = abs(value)
        FadingLevel(nr) = abs(value) + 4
    End If
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
        Case 4:object.image = b:FadingLevel(nr) = 6                   'fading to off...
        Case 5:object.image = a:FadingLevel(nr) = 1                   'ON
        Case 6, 7, 8:FadingLevel(nr) = FadingLevel(nr) + 1             'wait
        Case 9:object.image = c:FadingLevel(nr) = FadingLevel(nr) + 1 'fading...
        Case 10, 11, 12:FadingLevel(nr) = FadingLevel(nr) + 1         'wait
        Case 13:object.image = d:FadingLevel(nr) = 0                  'Off
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

 'Reels
Sub FadeReel(nr, reel)
    Select Case FadingLevel(nr)
        Case 2:FadingLevel(nr) = 0
        Case 3:FadingLevel(nr) = 2
        Case 4:reel.Visible = 0:FadingLevel(nr) = 3
        Case 5:reel.Visible = 1:FadingLevel(nr) = 1
    End Select
End Sub

 'Inverted Reels
Sub FadeIReel(nr, reel)
    Select Case FadingLevel(nr)
        Case 2:FadingLevel(nr) = 0
        Case 3:FadingLevel(nr) = 2
        Case 4:reel.Visible = 1:FadingLevel(nr) = 3
        Case 5:reel.Visible = 0:FadingLevel(nr) = 1
    End Select
End Sub

'******************************************************
'       	Flashers
'******************************************************

Sub flasher6on(enabled)
	If Enabled then
		If GI001.state = 0 then
			PLAYFIELD_GIL6.opacity = 1800
			PLAYFIELD_GIL6.intensityscale = 10 
		else
			PLAYFIELD_GIL6.opacity = 100
			PLAYFIELD_GIL6.intensityscale = 2
		end if
		PLAYFIELD_GIL6.visible = 1
	Else
		PLAYFIELD_GIL6.visible = 0
	end if
End Sub

Sub flasher7on(enabled)
	If Enabled then
		If GI001.state = 0 then
			PLAYFIELD_GIL7.opacity = 1800
			PLAYFIELD_GIL7.intensityscale = 10 
		else
			PLAYFIELD_GIL7.opacity = 100
			PLAYFIELD_GIL7.intensityscale = 2 
		end if
		PLAYFIELD_GIL7.visible = 1
	Else
		PLAYFIELD_GIL7.visible = 0
	end if
End Sub

Sub flasher8on(enabled)
	If Enabled then
		If GI001.state = 0 then
			PLAYFIELD_GIL8.opacity = 1800
			PLAYFIELD_GIL8.intensityscale = 10
		else
			PLAYFIELD_GIL8.opacity = 100
			PLAYFIELD_GIL8.intensityscale = 2 
		end if
		PLAYFIELD_GIL8.visible = 1
	Else
		PLAYFIELD_GIL8.visible = 0
	end if
End Sub

Sub flasher9on(enabled)
	If Enabled then
		If GI001.state = 0 then
			PLAYFIELD_GIL9.opacity = 1800
			PLAYFIELD_GIL9.intensityscale = 10 
		else
			PLAYFIELD_GIL9.opacity = 100
			PLAYFIELD_GIL9.intensityscale = 2 
		end if
		PLAYFIELD_GIL9.visible = 1
	Else
		PLAYFIELD_GIL9.visible = 0
	end if
End Sub

'Gottlieb System 80B
'added by Inkochnito
Sub editDips
Dim vpmDips : Set vpmDips = New cvpmDips
With vpmDips
.AddForm 700,400,"System 80B - DIP switches"
.AddFrame 2,4,190,"Maximum credits",49152,Array("8 credits",0,"10 credits",32768,"15 credits",&H00004000,"20 credits",49152)'dip 15&16
.AddFrame 2,80,190,"Coin chute 1 and 2 control",&H00002000,Array("seperate",0,"same",&H00002000)'dip 14
.AddFrame 2,126,190,"Playfield special",&H00200000,Array("replay",0,"extra ball",&H00200000)'dip 22
.AddFrame 2,172,190,"High games to date control",&H00000020,Array("no effect",0,"reset high games 2-5 on power off",&H00000020)'dip 6
.AddFrame 2,218,190,"3rd coin chute credits control",&H20000000,Array("no effect",0,"add 9",&H20000000)'dip 30
.AddFrame 2,264,190,"Game option dip 7",&H00000040,Array("off",0,"on",&H00000040)'dip 7
.AddFrame 2,310,190,"Game option dip 8",&H00000080,Array("off",0,"on",&H00000080)'dip 8
.AddFrame 205,4,190,"High game to date awards",&H00C00000,Array("not displayed and no award",0,"displayed and no award",&H00800000,"displayed and 2 replays",&H00400000,"displayed and 3 replays",&H00C00000)'dip 23&24
.AddFrame 205,80,190,"Balls per game",&H01000000,Array("5 balls",0,"3 balls",&H01000000)'dip 25
.AddFrame 205,126,190,"Replay limit",&H04000000,Array("no limit",0,"one per game",&H04000000)'dip 27
.AddFrame 205,172,190,"Novelty",&H08000000,Array("normal",0,"extra ball and replay scores 500K",&H08000000)'dip 28
.AddFrame 205,218,190,"Game mode",&H10000000,Array("replay",0,"extra ball",&H10000000)'dip 29
.AddFrame 205,264,190,"Game option dip 31",&H40000000,Array("off",0,"on",&H40000000)'dip 31
.AddFrame 205,310,190,"Game option dip 32",&H80000000,Array("off",0,"on",&H80000000)'dip 32
.AddChk 2,360,120,Array("Match feature",&H02000000)'dip 26
.AddLabel 50,380,300,20,"After hitting OK, press F3 to reset game with new settings."
.ViewDips
End With
End Sub
Set vpmShowDips = GetRef("editDips")

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep, LAstep, LBstep, LB2step, LCstep, LC2step, RCstep, RAStep, RA2Step, RBStep, RB2Step

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 44
	DOF 102, DOFPulse
	RandomSoundSlingshotRight slingr
	RSling.Visible = 0
	RSling1.Visible = 1
	slingr.TransZ = -20
	RStep = 0
	RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
	Select Case RStep
		Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:slingr.TransZ = -10
		Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:slingr.TransZ = 0:RightSlingShot.TimerEnabled = 0:
	End Select
	RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 44
	DOF 101, DOFPulse
	RandomSoundSlingshotLeft slingl
	LSling.Visible = 0
	LSling1.Visible = 1
	slingl.TransZ = -20
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
	Select Case LStep
		Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:slingl.TransZ = -10
		Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:slingl.TransZ = 0:LeftSlingShot.TimerEnabled = 0:
	End Select
	LStep = LStep + 1
End Sub

Const WallPrefix 		= "T" 'Change this based on your naming convention
Const PrimitivePrefix 	= "PrimT"'Change this based on your naming convention
Const PrimitiveBumperPrefix = "BumperRing" 'Change this based on your naming convention
Dim primCnt(120), primDir(120), primBmprDir(120)

'****************************************************************************
'***** Primitive Standup Target Animation
'****************************************************************************
'USAGE: 	Sub sw1_Hit: 	PrimStandupTgtHit  1, Sw1, PrimSw1: End Sub
'USAGE: 	Sub Sw1_Timer: 	PrimStandupTgtMove 1, Sw1, PrimSw1: End Sub

Const StandupTgtMovementDir = "TransY"
Const StandupTgtMovementMax = 6

Sub PrimStandupTgtHit (swnum, wallName, primName)
	PlayTargetSound
	vpmTimer.PulseSw swnum
	primCnt(swnum) = 0 									'Reset count
	wallName.TimerInterval = 10	'Set timer interval
	wallName.TimerEnabled = 1 	'Enable timer
End Sub

Sub	PrimStandupTgtMove (swnum, wallName, primName)
	Select Case StandupTgtMovementDir
		Case "TransX":
			Select Case primCnt(swnum)
				Case 0: 	primName.TransX = -StandupTgtMovementMax 
				Case 1: 	primName.TransX = -StandupTgtMovementMax *2/3
				Case 2: 	primName.TransX = -StandupTgtMovementMax /3
				Case 3: 	primName.TransX = 0
				Case else: 	wallName.TimerEnabled = 0
			End Select
		Case "TransY":
			Select Case primCnt(swnum)
				Case 0: 	primName.TransY = -StandupTgtMovementMax
				Case 1: 	primName.TransY = -StandupTgtMovementMax * 2/3
				Case 2: 	primName.TransY = -StandupTgtMovementMax /3
				Case 3: 	primName.TransY = 0
				Case else: 	wallName.TimerEnabled = 0
			End Select
		Case "TransZ":
			Select Case primCnt(swnum)
				Case 0: 	primName.TransZ = -StandupTgtMovementMax 
				Case 1: 	primName.TransZ = -StandupTgtMovementMax * 2/3
				Case 2: 	primName.TransZ = -StandupTgtMovementMax /3
				Case 3: 	primName.TransZ = 0
				Case else: 	wallName.TimerEnabled = 0
			End Select
	End Select
	primCnt(swnum) = primCnt(swnum) + 1
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

Dim BIPL

Sub StartBallControl_UnHit()
	If Activeball.vely > 0 Then BIPL = 1 Else BIPL = 0 
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

if flippershadows=1 then
	batleftshadow.visible=1
	batrightshadow.visible=1
else
	batleftshadow.visible=0
	batrightshadow.visible=0
end if

'*****************************************
'			FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	batleftshadow.RotZ = LeftFlipper.currentangle
	batrightshadow.RotZ = RightFlipper.currentangle
	Pgate001.rotx = -Gate003.currentangle*0.5
	FlipperL.rotz = LeftFlipper.currentangle  + 240
	FlipperR.rotz = RightFlipper.currentangle + 120
	FlipperR1.rotz = RightFlipper001.currentangle + 150
	if lampstate(166)=1 Then
		Dim xx
		For each xx in Gi:xx.State = 1:Next
		For each xx in layer1col: xx.image = "layer1": Next
		For each xx in layer2col: xx.image = "layer2": Next
		For each xx in layer3col: xx.image = "layer3": Next
		For each xx in plasticcol: xx.image = "plastics": Next
		For each xx in flipperlightcol: xx.image = "flippers": Next
		outers.image="outers"
	Else
		For each xx in Gi:xx.State = 0:Next
		For each xx in layer1col: xx.image = "layer1off": Next
		For each xx in layer2col: xx.image = "layer2off": Next
		For each xx in layer3col: xx.image = "layer3off": Next
		For each xx in plasticcol: xx.image = "plasticsoff": Next
		For each xx in flipperlightcol: xx.image = "flippersoff": Next
		outers.image="outersoff"
	End If
End Sub

if ballshadows=1 then
	BallShadowUpdate.enabled=1
else
	BallShadowUpdate.enabled=0
end if

'*****************************************
'			BALL SHADOW
'*****************************************
Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5,BallShadow6,BallShadow7,BallShadow8,BallShadow9,BallShadow10)

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
		BallShadow(b).X = BOT(b).X - (TableWidth/2 - BOT(b).X)/20
		ballShadow(b).Y = BOT(b).Y + 10
		If BOT(b).Z > 20 Then
			BallShadow(b).visible = 1
		Else
			BallShadow(b).visible = 0
		End If
	Next
End Sub

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 10 ' total number of balls
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
		x.TimeDelay = 80
	Next

	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.05, -2.7	
	AddPt "Polarity", 2, 0.33, -2.7
	AddPt "Polarity", 3, 0.37, -2.7	
	AddPt "Polarity", 4, 0.41, -2.7
	AddPt "Polarity", 5, 0.45, -2.7
	AddPt "Polarity", 6, 0.576,-2.7
	AddPt "Polarity", 7, 0.66, -1.8
	AddPt "Polarity", 8, 0.743, -0.5
	AddPt "Polarity", 9, 0.81, -0.5
	AddPt "Polarity", 10, 0.88, 0

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
'		RUBBER POST AND SLEEVE DAMPENERS
'******************************************************
'These are data mined bounce curves, 
'dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.

Sub dPosts_Hit(idx) 
	RubbersD.dampen Activeball
End Sub

Sub dSleeves_Hit(idx) 
	SleevesD.Dampen Activeball
End Sub

dim RubbersD : Set RubbersD = new Dampener	'frubber
RubbersD.name = "Rubbers"

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
SleevesD.CopyCoef RubbersD, 0.85

Class Dampener
	public name, Threshold 	'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize : redim ModIn(0) : redim Modout(0): End Sub 

	Public Sub AddPoint(aIdx, aX, aY) 
		ShuffleArrays ModIn, ModOut, 1 : ModIn(aIDX) = aX : ModOut(aIDX) = aY : ShuffleArrays ModIn, ModOut, 0
	End Sub

	public sub Dampen(aBall)
		if threshold then if BallSpeed(aBall) < threshold then exit sub end if end if
		dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / cor.ballvel(aBall.id)
		coef = desiredcor / realcor 
		
		aBall.velx = aBall.velx * coef : aBall.vely = aBall.vely * coef
	End Sub

	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		dim x : for x = 0 to uBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x)*aCoef
		Next
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

Sub RDampen_Timer()
	Cor.Update
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
Dim DT74

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

' 1 Bank
Set DT74 = (new DropTarget)(sw74, sw74y, psw74, 74, 0, false)

'Add all the Drop Target Arrays to Drop Target Animation Array
' DTAnimationArray = Array(DT1, DT2, ....)
Dim DTArray
DTArray = Array(DT74)

'Configure the behavior of Drop Targets.
Const DTDropSpeed = 110 			'in milliseconds
Const DTDropUpSpeed = 40 			'in milliseconds
Const DTDropUnits = 50				'VP units primitive drops
Const DTDropUpUnits = 5 			'VP units primitive raises above the up position on drops up
Const DTMaxBend = 8 				'max degrees primitive rotates when hit
Const DTDropDelay = 20	 		'time in milliseconds before target drops (due to friction/impact of the ball)
Const DTRaiseDelay = 40	 		'time in milliseconds before target drops back to normal up position after the solendoid fires to raise the target
Const DTBrickVel = 30				'velocity at which the target will brick, set to '0' to disable brick

Const DTEnableBrick = 0			'Set to 0 to disable bricking, 1 to enable bricking
Const DTHitSound = "Target_Hit_1"	'Drop Target Hit sound
Const DTDropSound = "DTDrop"		'Drop Target Drop sound
Const DTResetSound = "DTReset"	'Drop Target reset sound

Const DTMass = 0.2				'Mass of the Drop Target (between 0 and 1), higher values provide more resistance

'******************************************************
'				DROP TARGETS FUNCTIONS
'******************************************************

Sub DTHit(switch)
	Dim i
	i = DTArrayID(switch)

	PlayTargetSound
	DTArray(i).animate =  DTCheckBrick(Activeball,DTArray(i).prim)
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

' Table specific function
Sub	DTShadow(prim, shadowprim, imagename)
	If prim.transz < -DTDropUnits/2 Then
		shadowprim.image ="blank" 
	Else
		shadowprim.image = imagename 
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

' Used for drop targets
Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

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
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
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
		Case 1 : PlaySound ("Nudge_1"), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
		Case 2 : PlaySound ("Nudge_2"), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
		Case 3 : PlaySound ("Nudge_3"), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
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
Sub RandomSoundBumperTop(Bump)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Top_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
	End Select
End Sub

Sub RandomSoundBumperMiddle(bump)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
	End Select
End Sub

Sub RandomSoundBumperBottom(Bump)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_1",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 2 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_2",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 3 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_3",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 4 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_4",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
		Case 5 : PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_5",DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
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

Sub GatesWire_hit(idx)
	SoundPlayfieldGate
End Sub


'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Arch_L1"), Vol(ActiveBall) * ArchSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Arch_L2"), Vol(ActiveBall) * ArchSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Arch_L3"), Vol(ActiveBall) * ArchSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Arch_L4"), Vol(ActiveBall) * ArchSoundFactor
	End Select
End Sub

Sub RandomSoundRightArch()
	Select Case Int(Rnd*4)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Arch_R1"), Vol(ActiveBall) * ArchSoundFactor
		Case 2 : PlaySoundAtLevelActiveBall ("Arch_R2"), Vol(ActiveBall) * ArchSoundFactor
		Case 3 : PlaySoundAtLevelActiveBall ("Arch_R3"), Vol(ActiveBall) * ArchSoundFactor
		Case 4 : PlaySoundAtLevelActiveBall ("Arch_R4"), Vol(ActiveBall) * ArchSoundFactor
	End Select
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
	Select Case Int(Rnd*2)+1	
		Case 1: PlaySoundAtLevelStatic ("Saucer_Enter_1"), SaucerLockSoundLevel, Activeball
		Case 2: PlaySoundAtLevelStatic ("Saucer_Enter_2"), SaucerLockSoundLevel, Activeball
	End Select
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
