Option Explicit
Randomize
 
On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0
 
Const tnob = 3						'Total number of balls
Const BallMass = 1
Const BallSize = 50
Const cGameName="hs_l4",UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin"
Const UseSolenoids = 2
Const UseLamps = 0
Const UseSync = 1

'VR Dims..
Dim VRRoom
Dim VRThing
Dim Thing
Dim BeaconPos:BeaconPos = 0
Dim Scratches
Dim PoliceOn:PoliceOn = False
dim gilvl:gilvl = 1 ' For Dynamic Shadows




'*****************************************************************************************************************************************************
' *** User Options - Dynamic Shadows *****************************************************************************************************************
'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1		'0 = Static shadow under ball ("flasher" image, like JP's)
									'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
									'2 = flasher image shadow, but it moves like ninuzzu's
' *** End User Options *******************************************************************************************************************************
'*****************************************************************************************************************************************************


'*********************************************************************************************************
'*** VR OPTIONS ******************************************************************************************

VRRoom = 0      ' 0 = Desktop/FS   1 = VR room  2 = Ultra Minimal VR room
Scratches = 1   ' 0 = no glass scratches   1 = Glass scratches on   (This will not run if VRroom = 0)

'*** END VR OPTIONS **************************************************************************************
'*********************************************************************************************************


'///////////////////////-----General Sound Options-----///////////////////////
Const VolumeDial = 0.8				'Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Const BallRollVolume = 0.4 			'Level of ball rolling volume. Value between 0 and 1


 
LoadVPM "01500000", "S11.VBS", 3.10
Dim DesktopMode: DesktopMode = Table1.ShowDT
 
Dim tablewidth, tableheight : tablewidth = table1.width : tableheight = table1.height

If DesktopMode = True Then 'Show Desktop components
	SideRails.visible=1
	LockdownBar.visible=1
	Ramp15.visible=0
	Ramp16.visible=0
	Primitive13.visible=1
	For each VRThing in VRStreet: VRThing.visible = false: Next
	For each VRThing in VRCab: VRThing.visible = false: Next
	For each VRThing in VRBackbox: VRThing.visible = false: Next
	For each Thing in DesktopBackglass: Thing.visible = true: Next
	CarlightBlue4.visible = false
	CarlightRed4.visible = false
	VRRVBLUE.visible = false
	BeaconFR.opacity = 0
	TimerVRPlunger2.enabled = false
	if Scratches = 1 then GlassImpurities.visible = True
	Set LampCallback = GetRef("UpdateDTLamps")
Else
	SideRails.visible=0
	LockdownBar.visible=0
	Primitive13.visible=1
	Ramp15.visible=0
	Ramp16.visible=0
	For each Thing in bg: Thing.visible = false: Next
End if

Sub UpdateDTLamps()
	If Controller.Lamp(1) = 0 Then: l1.visible= 0:	Else: l1.visible=1 'Game Over
	If Controller.Lamp(2) = 0 Then: l2.visible=0:	Else: l2.visible=1 'Match
	If Controller.Lamp(3) = 0 Then: l3a.visible=0:	Else: l3a.visible=1 'Shoot Again
	If Controller.Lamp(6) = 0 Then: l6.visible=0:	Else: l6.visible=1 'Ball In Play
End Sub


'*****************************************************************************************************************
' LOAD VR ROOM ***************************************************************************************************
If VRRoom = 1 then   ' turns these back off for VR..
	SideRails.visible=0
	LockdownBar.visible=0
	Primitive13.visible=1
	Ramp15.visible=0
	Ramp16.visible=0
	For each VRThing in VRStreet: VRThing.visible = true: Next
	For each VRThing in VRCab: VRThing.visible = true: Next
	For each VRThing in VRBackbox: VRThing.visible = true: Next
	For each Thing in DesktopBackglass: Thing.visible = false: Next
	For each Thing in bg: Thing.visible = false: Next
	CarlightBlue4.visible = false
	CarlightRed4.visible = false
	VRRVBLUE.visible = true
	BeaconFR.opacity = 0
	TimerVRPlunger2.enabled = true 
	if Scratches = 1 then GlassImpurities.visible = True
End If


If VRRoom = 2 then   ' Loads Ultra Minimal room - Just the VR lockdown bar, rails, and Backglass
	SideRails.visible=0
	LockdownBar.visible=0
	Primitive13.visible=1
	Ramp15.visible=0
	Ramp16.visible=0
	For each VRThing in VRBackbox: VRThing.visible = true: Next
	For each Thing in DesktopBackglass: Thing.visible = false: Next
	For each Thing in bg: Thing.visible = false: Next
	Primary_LockDownBar.visible = True
	Primary_SideRailMapLeft.visible = True
	Primary_SideRailMapRight.visible = True
	FrontTrimTop.visible = True
	FrontTrimBottom.visible = True
	VRRVBlue.visible = true
	VRbackglass.visible = True
	Primary_Cab_Head.visible = True
	if Scratches = 1 then GlassImpurities.visible = True
End If

' END LOAD VR ROOM ***********************************************************************************************
'*****************************************************************************************************************

'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************
SolCallback(1)      = "bsTrough.SolIn"
SolCallback(2)      = "bstrough.SolOut"
SolCallback(3)      = "bsSaucer.SolOut"

SolCallback(4)      = "policelight"         'Runs VR Beacon and Ploice Car lights

SolCallback(5)      = "SetLamp 105,"		'PF Light
SolCallback(6)      = "SetLamp 106,"		'PF Light
SolCallback(7)      = "bsLeftLock.SolOut"	'Left Hideout Eject
SolCallback(8)      = "bsRightLock.SolOut"	'Right Hideout Eject
SolCallBack(9)      = "FlashSol109"			'X2 Left Dome Flasher
SolCallback(10) 	= "FlashSol100"
SolCallback(11)     = "PFGI"				'General Illumination Relay
SolCallBack(12)     = "FlashSol112"			'X2 Right Dome Flasher
SolCallback(13)     = "Divert"
SolCallback(14)     = "SolKickback"
SolCallback(15)		= "SolKnocker"
Solcallback(22)     = "FlashSol122"			'X2 Top flasher
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Const PositionalSoundPlaybackConfiguration = 3


'Timers

Sub Frametimer_Timer()

	'BallShadowUpdate  - Removed - Replaced with below.
	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows

	TimerFlipper



' Watching the DummyLights Intesity to set the Texture on the Traffic light Prim
	 If l42.intensityscale > 0.5 and l43.intensityscale < 0.5 and l44.intensityscale < 0.5 then 
		stoplight_prim.image="hslightred copy"
		f42.visible=1
		If VRroom = 1 then VRTL_RED.visible = true :VRTL_Yellow.visible = false:VRTL_Green.visible = false
	 End If

	 If l42.intensityscale < 0.5 and l43.intensityscale > 0.5 and l44.intensityscale < 0.5 then 
		stoplight_prim.image="hslightyellow copy" 
		f43.visible=1
		If VRroom = 1 then VRTL_RED.visible = false :VRTL_Yellow.visible = true:VRTL_Green.visible = false
	 End If

	 If l42.intensityscale < 0.5 and l43.intensityscale < 0.5 and l44.intensityscale > 0.5 then 
		stoplight_prim.image="hslightgreen copy" 
		f44.visible=1
		If VRroom = 1 then VRTL_RED.visible = false :VRTL_Yellow.visible = false:VRTL_Green.visible = true
	 End if

	 If l42.intensityscale < 0.5 and l43.intensityscale > 0.5 and l44.intensityscale > 0.5 then 
		stoplight_prim.image="hslightgreenyellow copy"
		f43.visible=1
		f44.visible=1
		If VRroom = 1 then VRTL_RED.visible = false :VRTL_Yellow.visible = true:VRTL_Green.visible = true
	 End If

	 If l42.intensityscale < 0.5 and l43.intensityscale < 0.5 and l44.intensityscale < 0.5 then 
		stoplight_prim.image="hslightoff"
        If VRroom = 1 then VRTL_RED.visible = false :VRTL_Yellow.visible = false:VRTL_Green.visible = false
	 End If

	 If l42.intensityscale > 0.5 and l43.intensityscale > 0.5 and l44.intensityscale > 0.5 then 
		stoplight_prim.image="hslighton"
		f42.visible=1
		f43.visible=1
		f44.visible=1
		If VRroom = 1 then VRTL_RED.visible = true :VRTL_Yellow.visible = true:VRTL_Green.visible = true
	 End If


	Flasherflash1a.opacity = Flasherflash1.opacity
	Flasherflash1b.opacity = Flasherflash1.opacity
	Flasherflash1c.opacity = Flasherflash1.opacity
	Flasherflash2a.opacity = Flasherflash2.opacity
	Flasherflash2b.opacity = Flasherflash2.opacity
	Flasherflash3a.opacity = Flasherflash3.opacity
	Flasherflash3b.opacity = Flasherflash3.opacity
	Flasherflash3c.opacity = Flasherflash3.opacity
	Flasherflash4a.opacity = Flasherflash4.opacity
	Flasherflash4b.opacity = Flasherflash4.opacity
'	Flasherflash5a.opacity = Flasherflash5.opacity
	Flasherflash5b.opacity = Flasherflash5.opacity
'	Flasherflash5c.opacity = Flasherflash5.opacity
	Flasherflash6c.opacity = Flasherflash6.opacity


	Flasherflash1a.visible = Flasherflash1.visible
	Flasherflash1b.visible = Flasherflash1.visible
	Flasherflash1c.visible = Flasherflash1.visible
	Flasherflash2a.visible = Flasherflash2.visible
	Flasherflash2b.visible = Flasherflash2.visible
	Flasherflash3a.visible = Flasherflash3.visible
	Flasherflash3b.visible = Flasherflash3.visible
	Flasherflash3c.visible = Flasherflash3.visible
	Flasherflash4a.visible = Flasherflash4.visible
	Flasherflash4b.visible = Flasherflash4.visible
'	Flasherflash5a.visible = Flasherflash5.visible
	Flasherflash5b.visible = Flasherflash5.visible
'	Flasherflash5c.visible = Flasherflash5.visible
	Flasherflash6c.visible = Flasherflash6.visible

End Sub


'******************************************************
' VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1 		'0 = normal standup targets, 1 = bouncy targets, 2 = orig TargetBouncer
Const TargetBouncerFactor = 0.7 	'Level of bounces. Recommmended value of 0.7 when TargetBouncerEnabled=1, and 1.1 when TargetBouncerEnabled=2

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
    elseif TargetBouncerEnabled = 2 and aball.z < 30 then
		'debug.print "velz: " & activeball.velz
		Select Case Int(Rnd * 4) + 1
			Case 1: zMultiplier = defvalue+1.1
			Case 2: zMultiplier = defvalue+1.05
			Case 3: zMultiplier = defvalue+0.7
			Case 4: zMultiplier = defvalue+0.3
		End Select
		aBall.velz = aBall.velz * zMultiplier * TargetBouncerFactor
		'debug.print "----> velz: " & activeball.velz
		'debug.print "conservation check: " & BallSpeed(aBall)/vel
	end if
end sub


'******************************************
'				FLIPPERS
'******************************************

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
		rightflipper1.RotateToEnd

		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		RightFlipper.RotateToStart:RightFlipper1.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If        
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub


' Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch Activeball, LeftFlipper, LFCount, parm
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch Activeball, RightFlipper, RFCount, parm
	RightFlipperCollide parm
End Sub



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
	AddPt "Polarity", 1, 0.05, -5
	AddPt "Polarity", 2, 0.4, -5
	AddPt "Polarity", 3, 0.6, -4.5
	AddPt "Polarity", 4, 0.65, -4.0
	AddPt "Polarity", 5, 0.7, -3.5
	AddPt "Polarity", 6, 0.75, -3.0
	AddPt "Polarity", 7, 0.8, -2.5
	AddPt "Polarity", 8, 0.85, -2.0
	AddPt "Polarity", 9, 0.9,-1.5
	AddPt "Polarity", 10, 0.95, -1.0
	AddPt "Polarity", 11, 1, -0.5
	AddPt "Polarity", 12, 1.1, 0
	AddPt "Polarity", 13, 1.3, 0

	addpt "Velocity", 0, 0,         1.1
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
'  FLIPPER CORRECTION FUNCTIONS
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
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS 
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
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then EOSNudge1 = 0
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

' Used for drop targets and stand up targets
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
' End - Check ball distance from Flipper for Rem
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
'Const EOSTnew = 1 'EM's to late 80's
Const EOSTnew = 0.8 '90's and later
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
'Const EOSReturn = 0.055  'EM's
'Const EOSReturn = 0.045  'late 70's to mid 80's
Const EOSReturn = 0.035  'mid 80's to early 90's
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
		Dim b, BOT
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


'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************


'*****************************************************************************************************
'*******************************************************************************************************
'END nFOZZY FLIPPERS'


'**********************************************************************************************************
 'Solenoid Controlled toys
'**********************************************************************************************************
 

Sub SolKnocker(Enabled)
        If enabled Then
                KnockerSolenoid 'Add knocker position object
        End If
End Sub

Sub SolKickBack(enabled)
    If enabled Then
       Plunger1.Fire
       PlaySound SoundFX("Popper",DOFContactors)
    Else
       Plunger1.PullBack
    End If
End Sub
 
Sub Divert(enabled)
    If Enabled Then
        Diverter1.IsDropped = 0  
        Diverter2.IsDropped = 0
        PrimFlipper1.roty = -90
        PrimFlipper2.roty = -90
        PlaySound SoundFX("sc_loop2 2",DOFContactors)
    Else
        Diverter1.IsDropped = 1
        Diverter2.IsDropped = 1
        PrimFlipper1.roty = 0
        PrimFlipper2.roty = 0
        PlaySound SoundFX("sc_loop2 2",DOFContactors)
    End If
End Sub
 
'Playfield GI
Sub PFGI(Enabled)
    If Enabled Then
        dim xx
        For each xx in GI:xx.State = 0: Next
        Sound_GI_Relay 0, Bumper3
		gioff.visible=1
		gion.visible=0
		RLD.visible=0
		RLS.visible=0
		RLG.visible=0
		RLG1.visible=0
		RRS.visible=0
		RRS1.visible=0
		RRD.visible=0
		RRD1.visible=0
		RRG1.visible=0
		RRG2.visible=0
		Flasher_wall1.visible=0
		Flasher_wall2.visible=0
		Flasher_wall3.visible=0
		SetLamp 120,0
		metalwireramps.blenddisableLighting=0.1
		metalwalls.blenddisableLighting=0.5
	Else
        For each xx in GI:xx.State = 1: Next
        Sound_GI_Relay 1, Bumper3
		gioff.visible=0
		gion.visible=1
		RLD.visible=1
		RLS.visible=1
		RLG.visible=1
		RLG1.visible=1
		RRS.visible=1
		RRS1.visible=1
		RRD.visible=1
		RRD1.visible=1
		RRG1.visible=1
		RRG2.visible=1
		Flasher_wall1.visible=1
		Flasher_wall2.visible=1
		Flasher_wall3.visible=1
		SetLamp 120,1
		metalwireramps.blenddisableLighting=0.3
		metalwalls.blenddisableLighting=1
	End If
End Sub
 
'**********************************************************************************************************
 'Initiate Table
'**********************************************************************************************************
 
Dim bsTrough, bsSaucer, bsLeftLock, bsRightLock, SubSpeed
 ' using table width and height in script slows down the performance
'dim tablewidth: tablewidth = Table1.width
'dim tableheight: tableheight = Table1.height
Sub Table1_Init
    vpmInit Me
    On Error Resume Next
        With Controller
        .GameName = cGameName
        If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
        .SplashInfoLine = "High Speed (Williams)"&chr(13)&"32a & Chokeee"
        .HandleMechanics=0
        .HandleKeyboard=0
        .ShowDMDOnly=1
        .ShowFrame=0
        .ShowTitle=0
        .hidden = 1
         On Error Resume Next
		 
		Controller.SolMask(0)=0
         vpmTimer.AddTimer 2500,"Controller.SolMask(0)=&Hffffffff'"
         .Run GetPlayerHWnd
         If Err Then MsgBox Err.Description
         On Error Goto 0
     End With
     On Error Goto 0
 
    PinMAMETimer.Interval=PinMAMEInterval  
    PinMAMETimer.Enabled=1
 
    vpmNudge.TiltSwitch=1
    vpmNudge.Sensitivity=3
    vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)
   
    Set bsTrough=New cvpmBallStack
        bsTrough.InitSw 9,12,11,10,0,0,0,0
        bsTrough.InitKick BallRelease,90,10
'        bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
        bsTrough.Balls=3
 
    Set bsSaucer = New cvpmBallStack
        bsSaucer.InitSaucer sw16,16,96,10
		bsSaucer.KickforceVar = 5
        bsSaucer.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)
 
    Set bsLeftLock = New cvpmBallStack
        bsLeftLock.InitSw 0,0,40,0,0,0,0,0
        bsLeftLock.InitSaucer LKick,40,0,200
		bsLeftLock.KickforceVar = 100
        bsLeftLock.InitExitSnd SoundFX("kicker2",DOFContactors), SoundFX("Solenoid",DOFContactors)
 
     Set bsRightLock = New cvpmBallStack
        bsRightLock.InitSw 0,0,48,0,0,0,0,0
        bsRightLock.InitSaucer RKick,48,0,200
		bsRightLock.KickforceVar = 100
        bsRightLock.InitExitSnd SoundFX("kicker2",DOFContactors), SoundFX("Solenoid",DOFContactors)
 
    Plunger1.Pullback
    Diverter1.IsDropped = 1
    Diverter2.IsDropped = 1
	PFGI 0
 
 End Sub



 
'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Dim BIPL : BIPL=0

Sub Table1_KeyDown(ByVal keycode)

	If KeyCode = PlungerKey Then 
		Plunger.Pullback
		SoundPlungerPull()
		If VRRoom = 1 Then
			TimerVRPlunger.enabled = true  ' We enable the plunger timer below..  look for the TimerVPlunger Sub.
			TimerVRPlunger2.enabled = False' We disaable the plunger2 timer below..  look for the TimerVPlunger2 Sub.
		End if
	End if

	If keycode = LeftFlipperKey Then 
		FlipperActivate LeftFlipper, LFPress
		If VRRoom = 1 then VRFlipperButtonLeft.x = VRFlipperButtonLeft.x +5	
	End If

	If keycode = RightFlipperKey Then 
		FlipperActivate RightFlipper, RFPress
		If VRRoom = 1 then VRFlipperButtonRight.x = VRFlipperButtonRight.x -5
	End If

	If keycode = LeftTiltKey Then Nudge 90, 2:SoundNudgeLeft()
    If keycode = RightTiltKey Then Nudge 270, 2:SoundNudgeRight()
    If keycode = CenterTiltKey Then Nudge 0, 2:SoundNudgeCenter()

    If keycode = keyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25

		End Select
	End If
     
	if keycode=StartGameKey then 
		soundStartButton()
		If VRRoom = 1 then VRStartButton.y = VRStartButton.y - 5
	End If

	If KeyDownHandler(keycode) Then Exit Sub
End Sub


Sub Table1_KeyUp(ByVal KeyCode)
	If keycode = LeftFlipperKey Then 
		FlipperDeActivate LeftFlipper, LFPress
		If VRRoom = 1  then VRFlipperButtonLeft.x = VRFlipperButtonLeft.x -5
	End If

	If keycode = RightFlipperKey Then 
		FlipperDeActivate RightFlipper, RFPress
		If VRRoom = 1 then VRFlipperButtonRight.x = VRFlipperButtonRight.x +5
	End If

	
    If keycode = PlungerKey Then 
		If VRRoom = 1 then
			TimerVRPlunger.enabled = false  'Disabling the plungertimer.. this is used to animate the plunger
			TimerVRPlunger2.enabled = true  'enabling the plungertimer.. this is used to animate the plunger
		End if
	End If

	If KeyCode = PlungerKey Then
		Plunger.Fire
		If BIPL = 1 Then
			SoundPlungerReleaseBall()                        'Plunger release sound when there is a ball in shooter lane
		Else
			SoundPlungerReleaseNoBall()                        'Plunger release sound when there is no ball in shooter lane
		End If
	End If

	if keycode=StartGameKey then 
	If VRRoom = 1 then VRStartButton.y = VRStartButton.y + 5
	End If

	If KeyUpHandler(keycode) Then Exit Sub
End Sub
 
'**********************************************************************************************************
 
 ' Drain hole and kickers
Sub Drain_Hit:bsTrough.addball me : RandomSoundDrain Drain : End Sub
Sub BallRelease_UnHit: RandomSoundBallRelease ballrelease : End Sub
Sub sw16_Hit:bsSaucer.addball 0 : playsound"popper_ball" : End Sub
'Sub sw16_Hit:bsSaucer.addball 0 : SoundSaucerLock : End Sub
Sub LKick_Hit:bsLeftLock.AddBall 0 : SoundSaucerLock : End Sub
Sub RKick_Hit:bsRightLock.AddBall 0 : SoundSaucerLock : End Sub
 
'fake 180 turn wire Ramp
Sub kicker1_Hit
    SubSpeed=ABS(ActiveBall.VelY)
    kicker1.DestroyBall
    kicker2.CreateSizedballWithMass Ballsize/2,BallMass
    kicker2.Kick 180,SQR(SubSpeed)
	SoundSaucerKick 1, kicker2
End Sub
 
Sub kicker3_Hit
    SubSpeed=ABS(ActiveBall.VelY)
    kicker3.DestroyBall
    kicker4.CreateSizedballWithMass Ballsize/2,BallMass
    kicker4.Kick 180,SQR(SubSpeed)
	SoundSaucerKick 1, kicker4
End Sub

'Plastic Triggers
Sub TriggerRamp1_Hit():PlaySound "fx_lr7": End Sub
Sub TriggerRamp2_Hit():PlaySound "fx_lr6": End Sub
Sub TriggerRamp3_Hit():PlaySound "fx_lr4": End Sub
 
'Wire Triggers
Sub SW20_Hit : Controller.Switch(20)=1 : End Sub
Sub SW20_unHit : Controller.Switch(20)=0:End Sub
Sub SW21_Hit : Controller.Switch(21)=1 :  End Sub
Sub SW21_unHit : Controller.Switch(21)=0:End Sub
Sub SW31_Hit : Controller.Switch(31)=1 :  End Sub
Sub SW31_unHit : Controller.Switch(31)=0 : End Sub
Sub SW32_Hit : Controller.Switch(32)=1 :  End Sub
Sub SW32_unHit : Controller.Switch(32)=0:End Sub
Sub SW36_Hit : Controller.Switch(36)=1 :  BIPL=1 : End Sub
Sub SW36_unHit : Controller.Switch(36)=0:BIPL=0 :End Sub
sub Sw39_hit:controller.Switch(39) = 1 :  End Sub
sub Sw39_unhit:controller.Switch(39) = 0 : end sub
sub Sw47_hit:controller.Switch(47) = 1 :  End Sub
sub Sw47_unhit:controller.Switch(47) = 0 : end sub
 

'Stand Up Targets

Sub sw17_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 17
End Sub

Sub sw18_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 18
End Sub

Sub sw19_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 19
End Sub

Sub sw13_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 13
End Sub

Sub sw14_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 14
End Sub

Sub sw15_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 15
End Sub

Sub sw22_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 22
End Sub

Sub sw23_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 23
End Sub

Sub sw24_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 24
End Sub

Sub sw25_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 25
End Sub

Sub sw26_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 26
End Sub

Sub sw27_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 27
End Sub

Sub sw28_Hit
	TargetBouncer Activeball, 1	
	vpmTimer.PulseSw 28
End Sub

Sub sw29_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 29
End Sub

Sub sw30_Hit
	TargetBouncer Activeball, 1
	vpmTimer.PulseSw 30
End Sub



'******************************************************
'					BUMPERS
'******************************************************

Dim dirRing1 : dirRing1 = -1
Dim dirRing2 : dirRing2 = -1
Dim dirRing3 : dirRing3 = -1


Sub Bumper1_Hit
vpmTimer.PulseSw 33
RandomSoundBumperTop Bumper1
End Sub
 
Sub Bumper2_Hit
vpmTimer.PulseSw 34
RandomSoundBumperMiddle Bumper2
End Sub

Sub Bumper3_Hit
vpmTimer.PulseSw 35
RandomSoundBumperBottom Bumper3
End Sub
 
Sub Bumper1_timer()
	BR1.Z = BR1.Z + (5 * dirRing1)
	BR4.Y = mdistY - SQR((ABS(BR1.Y)-mdistY)^2 +(BR1.Z)^2) * dCos(mangle) + BR1.Z * dSin(mangle)
	BR4.Z = BR1.Z * dCos(mangle) + SQR((ABS(BR1.Y)-mdistY)^2 +(BR1.Z)^2) * dSin(mangle)
	If BR1.Z <= 0 Then dirRing1 = 1
	If BR1.Z >= 40 Then
		dirRing1 = -1
		BR1.Z = 40
		Me.TimerEnabled = 0
	End If
End Sub

Sub Bumper2_timer()
	BR2.Z = BR2.Z + (5 * dirRing2)
	If BR2.Z <= 0 Then dirRing2 = 1
	If BR2.Z >= 40 Then
		dirRing2 = -1
		BR2.Z = 40
		Me.TimerEnabled = 0
	End If
End Sub

Sub Bumper3_timer()
	BR3.Z = BR3.Z + (5 * dirRing3)
	If BR3.Z <= 0 Then dirRing3 = 1
	If BR3.Z >= 40 Then
		dirRing3 = -1
		BR3.Z = 40
		Me.TimerEnabled = 0
	End If
End Sub

'Ramp Triggers
Sub SW42_Hit:Controller.Switch(42)=1:End Sub
Sub SW42_unHit:Controller.Switch(42)=0:End Sub
Sub SW43_Hit:Controller.Switch(43)=1:End Sub
Sub SW43_unHit:Controller.Switch(43)=0:End Sub
 

'Spinner
Sub sw44_Spin:vpmTimer.PulseSw 44 : SoundSpinner sw44 : End Sub
Sub sw45_Spin:vpmTimer.PulseSw 45 : SoundSpinner sw45 : End Sub
Sub sw46_Spin:vpmTimer.PulseSw 46 : SoundSpinner sw46 : End Sub
 
'Star Triggers
Sub SW51_Hit:Controller.Switch(51)=1 : End Sub
Sub SW51_unHit:Controller.Switch(51)=0:End Sub
Sub SW52_Hit:Controller.Switch(52)=1 : End Sub
Sub SW52_unHit:Controller.Switch(52)=0:End Sub
 
'Generic Ramp Sounds
Sub Trigger1_Hit : playsound"Wire Ramp" : End Sub
Sub Trigger2_Hit : playsound"Wire Ramp" : End Sub
'Sub Trigger3_Hit : playsound"vuk_exit" : End Sub
'Sub Trigger4_Hit : playsound"vuk_exit" : End Sub
Sub Trigger5_Hit : stopSound "Wire Ramp"
                   playsound"WireRamp_Hit2": End Sub
Sub Trigger6_Hit : stopSound "Wire Ramp"
                   playsound"WireRamp_Hit2": End Sub
'Sub Trigger7_Hit : playsound"Ball Drop" : End Sub
'Sub Trigger8_Hit : playsound"Ball Drop" : End Sub
'Sub Trigger9_Hit : playsound"Ball Drop" : End Sub
Sub Trigger10_Hit : playsound"WireRamp_Hit" : End Sub
Sub Trigger11_Hit : playsound"WireRamp_Hit" : End Sub
Sub Trigger12_Hit : playsound"WireRamp_Hit1" : End Sub
Sub Trigger13_Hit : playsound"WireRamp_Hit1" : End Sub


'******************************************************
'				SLINGSHOTS
'******************************************************


Dim LStep, RStep

Sub LeftSlingshot_Slingshot
    RandomSoundSlingshotLeft SLING2
    vpmTimer.PulseSw 49
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub
 
Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -35
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
End Sub


Sub RightSlingshot_Slingshot
    RandomSoundSlingshotRight SLING1
    vpmTimer.PulseSw 50
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub
 
Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -35
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub



 
 '***************************************************
'       JP's VP10 Fading Lamps & Flashers
'       Based on PD's Fading Light System
' SetLamp 0 is Off
' SetLamp 1 is On
' fading for non opacity objects is 4 steps
'***************************************************
 
Dim LampState(200), FadingLevel(200)
Dim FlashSpeedUp(200), FlashSpeedDown(200), FlashMin(200), FlashMax(200), FlashLevel(200)
 
'InitLamps()             ' turn off the lights and flashers and reset them to the default parameters
'LampTimer.Interval = 5 'lamp fading speed
'LampTimer.Enabled = 1
' 
'' Lamp & Flasher Timers
' 
'Sub LampTimer_Timer()
'    Dim chgLamp, num, chg, ii
'    chgLamp = Controller.ChangedLamps
'    If Not IsEmpty(chgLamp) Then
'        For ii = 0 To UBound(chgLamp)
'            LampState(chgLamp(ii, 0) ) = chgLamp(ii, 1)       'keep the real state in an array
'            FadingLevel(chgLamp(ii, 0) ) = chgLamp(ii, 1) + 4 'actual fading step
'        Next
'    End If
'    UpdateLamps
'End Sub
 




 Sub UpdateLamps
  'check to make sure that the kickback is disabled and didn't get re-enabled after a kick - from PacDude!
   

	' I do not know why these are not working???
	'Flash 1, VRgameover
	'Flash 2, VRmatch
	'Flash 3, VRshootagain
	'Flash 6, VRBallinPlay


If DesktopMode = True Then
'    FadeReel 1 ,L1 'GameOver
'    FadeReel 2, L2 'Match Game
end if
 
    NFadeLm 3, l3
	NFadeLm 3, L3b
If DesktopMode = True Then
'    FadeReel 3, L3a 'Shoot Again
end if
 
    NFadeLm 4, l4
    NFadeLm 4, L4a
	'Flash 4, FlasherSpecialL
	'Flash 4, FlasherSpecialLa
    NFadeLm 5, l5
    NFadeLm 5, L5a
	'Flash 5, FlasherSpecialR
 
If DesktopMode = True Then
'    FadeReel 6, L6 'Ball In Play
end if
 
'    NFadeLm  7, L7
'	NFadeLm 7, L7a
'    NFadeLm 8, L8
'	NFadeLm 8, L8a
'	NFadeLm 8, L8b
'    NFadeLm 9, L9
'	NFadeLm 9, L9a
'	NFadeLm 9, L9b
'	Flash 9, F9
'	Flash 9, F9a
'	NFadeLm 9, L9b1
'    NFadeLm 10, L10
'	NFadeLm 10, L10a
'	NFadeLm 10, L10b
'    NFadeLm 11, L11
'	NFadeLm 11, L11a
'	NFadeLm 11, L11b
'    NFadeLm 12, l12
'	NFadeLm 12, L12a
'    NFadeLm 13, l13
'	NFadeLm 13, L13a
'	Flash 13, Flasher6
'    NFadeLm 14, l14
'	NFadeLm 14,  L14a
'	Flash 14, Flasher5
'    NFadeLm 15, l15
'    NFadeLm 15, L15a
'	Flash 15, Flasher4
'    NFadeLm 16, l16
'    NFadeLm 16, L16a
'	Flash 16, FlasherKickback
'	Flash 16, FlasherKickbacka
'    NFadeLm 17, L17
'	NFadeLm 17, L17a
'	Flash 17, Flasher15
'    NFadeLm 18, L18
'	NFadeLm 18, L18a
'	Flash 18, Flasher16
'    NFadeLm 19, L19
'	NFadeLm 19, L19a
'	Flash 19, Flasher17
'    NFadeLm  20, l20
'	NFadeLm 20, L20a
'	NFadeLm 21, L21
'	NFadeLm 21, L21a
'	NFadeLm 21, L21b
'    NFadeLm 22, l22
'	NFadeLm 22, L22a
'	Flash 22, Flasher3
'    NFadeLm 23, l23
'	NFadeLm 23, L23a
'	Flash 23, Flasher2
'    NFadeLm 24, l24
'    NFadeLm 24, L24a
'	Flash 24, Flasher1
'    NFadeLm 25, l25
'	NFadeLm 25, L25a
'	Flash 25, Flasher11
'    NFadeLm 26, l26
'	NFadeLm 26, L26a
'	Flash 26, Flasher10
'    NFadeLm 27, l27
'	NFadeLm 27, L27a
'	Flash 27, Flasher7
'    NFadeLm 28, L28
'	NFadeLm 28, L28a
'	Flash 28, Flasher12
'    NFadeLm 29, l29
'	NFadeLm 29, L29a
'	Flash 29, Flasher13
'    NFadeLm 30, L30
'	NFadeLm 30, L30a
'	Flash 30, Flasher14
'    NFadeLm 31, l31
'	NFadeLm 31, L31a
'    NFadeLm 32, l32
'	NFadeLm 32, L32a
'    NFadeLm 33, l33
'	NFadeLm 33, L33a
'    NFadeLm 34, L34
'	NFadeLm 34, L34a
'    NFadeLm 35, L35
'	NFadeLm 35, L35a
'    NFadeLm 36, L36
'	NFadeLm 36, L36a
'	NFadeLm 36, L36b
'    NFadeLm 37, l37
'	NFadeLm 37, L37a
'    NFadeLm 38, l38
'	NFadeLm 38, L38a
'    NFadeLm 39, l39
'	NFadeLm 39, L39a
'	NFadeLm 40, l40
'	NFadeLm 40, L40a
'    NFadeLm 41, l41
'	NFadeLm 41, L41a   

'   NFadeObjm 42, stoplight_prim, "hslightred copy", "hslightOFF"
'       Flash 42, F42 'Ramp Traffic Light
'   NFadeObjm 43, stoplight_prim, "hslightyellow copy", "hslightOFF"
'       Flash 43, F43 'Ramp Traffic Light
'   NFadeObjm 44, stoplight_prim, "hslightgreen copy", "hslightOFF"
'       Flash 44, F44 'Ramp Traffic Light
'
'    NFadeL 42, l42	'Traffic Lights
'    NFadeL 43, l43	'Traffic Lights
'    NFadeL 44, l44	'Traffic Lights
'
'    NFadeLm 45, L45
'    NFadeLm 45, L45a
'    NFadeLm 46, L46
'    NFadeLm 46, L46a
'    NFadeLm 47, L47
'    NFadeLm 47, L47a
'    NFadeLm 48, L48
'    NFadeLm 48, L48a
'    NFadeLm 49, L49
'    NFadeLm 49, L49a
'    NFadeLm 50, L50
'    NFadeLm 50, L50a
'    NFadeLm 51, L51
'    NFadeLm 51, L51a
'    NFadeLm 52, L52
'    NFadeLm 52, L52a
'    NFadeLm 53, L53
'    NFadeLm 53, L53a
'    NFadeLm 54, L54
'	NFadeLm 54, L54a
'    NFadeLm 55, L55
'	NFadeLm 55, L55a
'    NFadeLm 56, L56
'	NFadeLm 56, L56a
'    NFadeLm 57, L57
'	NFadeLm 57, L57a
'    NFadeLm 58, L58
'	NFadeLm 58, L58a
'    NFadeLm 59, L59
'	NFadeLm 59, L59a
'    NFadeLm 60, L60
'	NFadeLm 60, L60a
'    NFadeLm 61, L61
'	NFadeLm 61, L61a
'    NFadeLm 62, L62
'	NFadeLm 62, L62a
'    NFadeLm 63, L63
'	NFadeLm 63, L63a
'    NFadeLm 64, L64
'	NFadeLm 64, L64a
' 
' 'Solenoid Controlled Flashers
'    NFadeLm 105, F105
''	NFadeLm 105, F105a
'    NFadeLm 105, F105b
'    NFadeLm 105, F105c
'    NFadeLm 105, F105d
'	NFadeLm 105, F105e
'	NFadeLm 106, F106
''	NFadeLm 106, F106a
'    NFadeLm 106, F106b
'	NFadeLm 106, F106c
'    NFadeLm 106, F106d
'	NFadeLm 106, F106e

'	NFadeLm 109, FlasherLight1
'   NFadeLm 109, f109g
'   NFadeLm 109, f109h
'	NFadeLm 109, FlasherLight1a
 '	NFadeLm 109, F109c
'	NFadeLm 109, FlasherLight2
'	NFadeLm 109, F109a
'	Flashm 109, Flasherflash2a
'	Flashm 109, Flasherflash2b
'	Flashm 109, Flasherflash2c
'	Flashm 109, Flasherflash1a
'	Flashm 109, Flasherflash1b
'	Flashm 109, Flasherflash1c
'	FlupperFlashm 109, Flasherflash2, FlasherLit2, FlasherBase2, FlasherLight2
'	FlupperFlash 109, Flasherflash1, FlasherLit1, FlasherBase1, FlasherLight1
'
'	NFadeLm 112, FlasherLight4
'	NFadeLm 112, F112c
'	NFadeLm 112, FlasherLight3
'	NFadeLm 112, F112a
'	NFadeLm 112, FlasherLight4a
'	Flashm 112, Flasherflash3a
'	Flashm 112, Flasherflash3b
'	Flashm 112, Flasherflash3c
'	Flashm 112, Flasherflash4a
'	Flashm 112, Flasherflash4b
'	Flashm 112, Flasherflash4c
'	FlupperFlashm 112, Flasherflash4, FlasherLit4, FlasherBase4, FlasherLight4
'	FlupperFlash 112, Flasherflash3, FlasherLit3, FlasherBase3, FlasherLight3


'	FlupperFlashm 122, Flasherflash5, FlasherLit5, FlasherBase5, FlasherLight5
'	FlupperFlash 122, Flasherflash6, FlasherLit6, FlasherBase6, FlasherLight6

'	NFadeLm 122, Flasherlight5
'	NFadeLm 122, Flasherlight5a
'	NFadeLm 122, Flasherlight5b
'	NFadeLm 122, FlasherLight5a1
'	Flashm 122, Flasherflash5
'	Flashm 122, Flasherflash5a


'	NFadeLm 122, Flasherlight6
'	NFadeLm 122, Flasherlight6a
'	NFadeLm 122, Flasherlight6b
'	NFadeLm 122, FlasherLight6a1
'	Flashm 122, Flasherflash6
'	Flashm 122, Flasherflash6a
'	Flash 122, Flasherflash6b

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

Sub SetLamp(aNr, aOn)
    Lampz.state(aNr) = abs (aOn)
End Sub
 
'Sub SetLamp(nr, value)
'    If value <> LampState(nr) Then
'        LampState(nr) = abs(value)
'        FadingLevel(nr) = abs(value) + 4
'    End If
'End Sub
 
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
 
'******************************************************
'*****   FLUPPER DOMES 
'******************************************************

Sub FlashSol109(flstate)
	If Flstate Then
		Objlevel(1) = 1 : FlasherFlash1_Timer
		Objlevel(2) = 1 : FlasherFlash2_Timer
	End If
End Sub

Sub FlashSol112(flstate)
	If Flstate Then
		Objlevel(3) = 1 : FlasherFlash3_Timer
		Objlevel(4) = 1 : FlasherFlash4_Timer
	End If
End Sub

Sub FlashSol122(flstate)
	If Flstate Then
		Objlevel(5) = 1 : FlasherFlash5_Timer
		Objlevel(6) = 1 : FlasherFlash6_Timer
	End If
End Sub

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1			' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.6		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.4		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherBloomIntensity = 0.3		' *** lower this, if the blooms are too bright (i.e. 0.1)			***	
FlasherOffBrightness = 0.3		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20)

InitFlasher 1, "red"
InitFlasher 2, "red"
InitFlasher 3, "red"
InitFlasher 4, "red"
InitFlasher 5, "white"
InitFlasher 6, "white"

'rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 1,17 : RotateFlasher 2,0 : RotateFlasher 3,90 : RotateFlasher 4,90 

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	Set objbloom(nr) = Eval("Flasherbloom" & nr)
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
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4) : objbloom(nr).color = RGB(255,32,4)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59) : objbloom(nr).color = RGB(255,240,150)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objbloom(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objbloom(nr).opacity = 100 *  FlasherBloomIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 100 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objbloom(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub
Sub FlasherFlash6_Timer() : FlashFlasher(6) : End Sub

'******************************************************
'******  END FLUPPER DOMES
'******************************************************

'**********************************************************************************************************
'Digital Display
'**********************************************************************************************************


if VRRoom = 0 then  'Starts timer and sets up digits for DesktopMode

DisplayTimer.enabled = true
VRDisplayTimer.enabled = false

'*************************************************************************************************************************************************
' Desktop Digits.... *****************************************************************************************************************************

 Dim Digits(32)
 Digits(0)=Array(a00, a05, a0c, a0d, a08, a01, a06, a0f, a02, a03, a04, a07, a0b, a0a, a09, a0e)
 Digits(1)=Array(a10, a15, a1c, a1d, a18, a11, a16, a1f, a12, a13, a14, a17, a1b, a1a, a19, a1e)
 Digits(2)=Array(a20, a25, a2c, a2d, a28, a21, a26, a2f, a22, a23, a24, a27, a2b, a2a, a29, a2e)
 Digits(3)=Array(a30, a35, a3c, a3d, a38, a31, a36, a3f, a32, a33, a34, a37, a3b, a3a, a39, a3e)
 Digits(4)=Array(a40, a45, a4c, a4d, a48, a41, a46, a4f, a42, a43, a44, a47, a4b, a4a, a49, a4e)
 Digits(5)=Array(a50, a55, a5c, a5d, a58, a51, a56, a5f, a52, a53, a54, a57, a5b, a5a, a59, a5e)
 Digits(6)=Array(a60, a65, a6c, a6d, a68, a61, a66, a6f, a62, a63, a64, a67, a6b, a6a, a69, a6e)
 
 Digits(7)=Array(a70, a75, a7c, a7d, a78, a71, a76, a7f, a72, a73, a74, a77, a7b, a7a, a79, a7e)
 Digits(8)=Array(a80, a85, a8c, a8d, a88, a81, a86, a8f, a82, a83, a84, a87, a8b, a8a, a89, a8e)
 Digits(9)=Array(a90, a95, a9c, a9d, a98, a91, a96, a9f, a92, a93, a94, a97, a9b, a9a, a99, a9e)
 Digits(10)=Array(aa0, aa5, aac, aad, aa8, aa1, aa6, aaf, aa2, aa3, aa4, aa7, aab, aaa, aa9, aae)
 Digits(11)=Array(ab0, ab5, abc, abd, ab8, ab1, ab6, abf, ab2, ab3, ab4, ab7, abb, aba, ab9, abe)
 Digits(12)=Array(ac0, ac5, acc, acd, ac8, ac1, ac6, acf, ac2, ac3, ac4, ac7, acb, aca, ac9, ace)
 Digits(13)=Array(ad0, ad5, adc, add, ad8, ad1, ad6, adf, ad2, ad3, ad4, ad7, adb, ada, ad9, ade)
 
 ' 3rd Player
Digits(14) = Array(LED150,LED151,LED152,LED153,LED154,LED155,LED156)
Digits(15) = Array(LED160,LED161,LED162,LED163,LED164,LED165,LED166)
Digits(16) = Array(LED170,LED171,LED172,LED173,LED174,LED175,LED176)
Digits(17) = Array(LED180,LED181,LED182,LED183,LED184,LED185,LED186)
Digits(18) = Array(LED190,LED191,LED192,LED193,LED194,LED195,LED196)
Digits(19) = Array(LED200,LED201,LED202,LED203,LED204,LED205,LED206)
Digits(20) = Array(LED210,LED211,LED212,LED213,LED214,LED215,LED216)
 
' 4th Player
Digits(21) = Array(LED220,LED221,LED222,LED223,LED224,LED225,LED226)
Digits(22) = Array(LED230,LED231,LED232,LED233,LED234,LED235,LED236)
Digits(23) = Array(LED240,LED241,LED242,LED243,LED244,LED245,LED246)
Digits(24) = Array(LED250,LED251,LED252,LED253,LED254,LED255,LED256)
Digits(25) = Array(LED260,LED261,LED262,LED263,LED264,LED265,LED266)
Digits(26) = Array(LED270,LED271,LED272,LED273,LED274,LED275,LED276)
Digits(27) = Array(LED280,LED281,LED282,LED283,LED284,LED285,LED286)
 
' Ball in play
Digits(28) = Array(LED290,LED291,LED292,LED293,LED294,LED295,LED296)
Digits(29) = Array(LED300,LED301,LED302,LED303,LED304,LED305,LED306)
 
' Num of Credits
Digits(30) = Array(LED310,LED311,LED312,LED313,LED314,LED315,LED316)
Digits(31) = Array(LED320,LED321,LED322,LED323,LED324,LED325,LED326)
 
end If


 Sub DisplayTimer_Timer
    Dim ChgLED, ii, jj, num, chg, stat, obj, b, x
    ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
    If Not IsEmpty(ChgLED)Then
        If DesktopMode = True Then
       For ii=0 To UBound(chgLED)
          num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
            if (num < 32) then
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


'**************************************************************************************************************************************************
'VRDigits........  ********************************************************************************************************************************

If VRRoom > 0 then 'Starts timer and sets up digits for VR

VRDisplayTimer.enabled = true
DisplayTimer.enabled = false

Dim VRDigits(32)

VRDigits(0)=Array(D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11,D12,D13,D14,D15)
VRDigits(1)=Array(D16,D17,D18,D19,D20,D21,D22,D23,D24,D25,D26,D27,D28,D29,D30)
VRDigits(2)=Array(D31,D32,D33,D34,D35,D36,D37,D38,D39,D40,D41,D42,D43,D44,D45)
VRDigits(3)=Array(D46,D47,D48,D49,D50,D51,D52,D53,D54,D55,D56,D57,D58,D59,D60)
VRDigits(4)=Array(D61,D62,D63,D64,D65,D66,D67,D68,D69,D70,D71,D72,D73,D74,D75)
VRDigits(5)=Array(D76,D77,D78,D79,D80,D81,D82,D83,D84,D85,D86,D87,D88,D89,D90)
VRDigits(6)=Array(D91,D92,D93,D94,D95,D96,D97,D98,D99,D100,D101,D102,D103,D104,D105)

VRDigits(7)=Array(D106,D107,D108,D109,D110,D111,D112,D113,D114,D115,D116,D117,D118,D119,D120)
VRDigits(8)=Array(D121,D122,D123,D124,D125,D126,D127,D128,D129,D130,D131,D132,D133,D134,D135)
VRDigits(9)=Array(D136,D137,D138,D139,D140,D141,D142,D143,D144,D145,D146,D147,D148,D149,D150)
VRDigits(10)=Array(D151,D152,D153,D154,D155,D156,D157,D158,D159,D160,D161,D162,D163,D164,D165)
VRDigits(11)=Array(D166,D167,D168,D169,D170,D171,D172,D173,D174,D175,D176,D177,D178,D179,D180)
VRDigits(12)=Array(D181,D182,D183,D184,D185,D186,D187,D188,D189,D190,D191,D192,D193,D194,D195)
VRDigits(13)=Array(D196,D197,D198,D199,D200,D201,D202,D203,D204,D205,D206,D207,D208,D209,D210)

VRDigits(14)=Array(D211,D212,D213,D214,D215,D216,D217,D218)
VRDigits(15)=Array(D219,D220,D221,D222,D223,D224,D225,D226)
VRDigits(16)=Array(D227,D228,D229,D230,D231,D232,D233,D234)
VRDigits(17)=Array(D235,D236,D237,D238,D239,D240,D241,D242)
VRDigits(18)=Array(D243,D244,D245,D246,D247,D248,D249,D250)
VRDigits(19)=Array(D251,D252,D253,D254,D255,D256,D257,D258)
VRDigits(20)=Array(D259,D260,D261,D262,D263,D264,D265,D266)

VRDigits(21)=Array(D267,D268,D269,D270,D271,D272,D273,D274)
VRDigits(22)=Array(D275,D276,D277,D278,D279,D280,D281,D282)
VRDigits(23)=Array(D283,D284,D285,D286,D287,D288,D289,D290)
VRDigits(24)=Array(D291,D292,D293,D294,D295,D296,D297,D298)
VRDigits(25)=Array(D299,D300,D301,D302,D303,D304,D305,D306)
VRDigits(26)=Array(D307,D308,D309,D310,D311,D312,D313,D314)
VRDigits(27)=Array(D315,D316,D317,D318,D319,D320,D321,D322)

VRDigits(28)=Array(D323,D324,D325,D326,D327,D328,D329,D330)
VRDigits(29)=Array(D331,D332,D333,D334,D335,D336,D337,D338)
VRDigits(30)=Array(D339,D340,D341,D342,D343,D344,D345,D346)
VRDigits(31)=Array(D347,D348,D349,D350,D351,D352,D353,D354)

dim DisplayColor, DisplayColorG
DisplayColor =  RGB(255,40,1)

InitDigits

End If


Sub VRDisplaytimer_Timer
	Dim ii, jj, obj, b, x
	Dim ChgLED,num, chg, stat
	ChgLED=Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
		If Not IsEmpty(ChgLED) Then
			For ii=0 To UBound(chgLED)
				num=chgLED(ii, 0) : chg=chgLED(ii, 1) : stat=chgLED(ii, 2)
				For Each obj In VRDigits(num)
					If chg And 1 Then FadeDisplay obj, stat And 1	
					chg=chg\2 : stat=stat\2
				Next
			Next
		End If
End Sub

Sub FadeDisplay(object, onoff)
	If OnOff = 1 Then
		object.color = DisplayColor
	Else
		Object.Color = RGB(1,1,1)
	End If
End Sub


Sub InitDigits()
	dim tmp, x, obj
	for x = 0 to uBound(VRDigits)
		if IsArray(VRDigits(x) ) then
			For each obj in VRDigits(x)
				obj.height = obj.height + 18
				FadeDisplay obj, 0
			next
		end If
	Next
End Sub

' END VR Digits code ***********************************************************************************************************
'*******************************************************************************************************************************



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

Sub RollingUpdate_timer()
	Dim BOT, b
	BOT = GetBalls

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

 

 '*********** FLUPPER'S BATS *********************************

Sub TimerFlipper
    batleft.objrotz = LeftFlipper.CurrentAngle + 1
    batleftshadow.objrotz = batleft.objrotz
 
    batright.objrotz = RightFlipper.CurrentAngle + 1
    batrightshadow.objrotz = batright.objrotz
    batright1.objrotz = RightFlipper1.CurrentAngle + 1
    batrightshadow.objrotz = batright.objrotz
End sub



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
SpinnerSoundLevel = 0.1                              					'volume level; range [0, 1]

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
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd*7)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd*7)+1,DOFContactors), SlingshotSoundLevel, Sling
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
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.3*RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.3*RelayGISoundLevel, obj
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
'****  FLEEP MECHANICAL SOUNDS
'******************************************************



'******************************************************
'		TRACK ALL BALL VELOCITIES
'		FOR RUBBER DAMPENER AND DROP TARGETS
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
'****  LAMPZ by nFozzy
'******************************************************
' 
' Lampz is a utility designed to manage and fade the lights and light-related objects on a table that is being driven by a ROM.
' To set up Lampz, one must populate the Lampz.MassAssign array with VPX Light objects, where the index of the MassAssign array
' corrisponds to the ROM index of the associated light. More that one Light object can be associated with a single MassAssign index (not shown in this example)
' Optionally, callbacks can be assigned for each index using the Lampz.Callback array. This is very useful for allowing 3D Insert primitives
' to be controlled by the ROM. Note, the aLvl parameter (i.e. the fading level that ranges between 0 and 1) is appended to the callback call.
'
' NOTE: The below timer is for flashing the inserts as a demonstration of Lampz. Should be replaced by actual lamp states.
'       In other words, delete this sub (InsertFlicker_timer) and associated timer if you are going to use Lampz with a ROM.
'dim flickerX, FlickerState : FlickerState = 0

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
	UpdateLamps
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
	'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
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
	'	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
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



Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (1 / full MS fading time)
	dim x : for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/3 : Lampz.FadeSpeedDown(x) = 1/8 : next

	'Lampz Assignments
	'  In a ROM based table, the lamp ID is used to set the state of the Lampz objects

	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays
	
	Lampz.MassAssign(7)= L7
	Lampz.MassAssign(7)= L7a
	Lampz.Callback(7) = "DisableLighting p7on, 100,"

	Lampz.MassAssign(8)= L8
	Lampz.MassAssign(8)= L8a
	Lampz.Callback(8) = "DisableLighting p8on, 100,"

	Lampz.MassAssign(9)= L9
	Lampz.MassAssign(9)= L9a
	Lampz.Callback(9) = "DisableLighting p9on, 100,"

	Lampz.MassAssign(9)= L9b
	Lampz.MassAssign(9)= L9b1
	Lampz.Callback(9) = "DisableLighting p9bon, 100,"

	Lampz.MassAssign(10)= L10
	Lampz.MassAssign(10)= L10a
	Lampz.Callback(10) = "DisableLighting p10on, 100,"

	Lampz.MassAssign(11)= L11
	Lampz.MassAssign(11)= L11a
	Lampz.Callback(11) = "DisableLighting p11on, 100,"

	Lampz.MassAssign(12)= L12
	Lampz.MassAssign(12)= L12a
	Lampz.Callback(12) = "DisableLighting p12on, 100,"
	
	Lampz.MassAssign(13)= L13
	Lampz.MassAssign(13)= L13a
	Lampz.MassAssign(13)= F13
	Lampz.Callback(13) = "DisableLighting p13on, 100,"
	Lampz.MassAssign(14)= L14
	Lampz.MassAssign(14)= L14a
	Lampz.MassAssign(14)= F14
	Lampz.Callback(14) = "DisableLighting p14on, 100,"
	Lampz.MassAssign(15)= L15
	Lampz.MassAssign(15)= L15a
	Lampz.MassAssign(15)= F15
	Lampz.Callback(15) = "DisableLighting p15on, 100,"


	Lampz.MassAssign(16)= L16
	Lampz.MassAssign(16)= L16a
	Lampz.MassAssign(16)= F16
	Lampz.MassAssign(16)= F16a
	Lampz.Callback(16) = "DisableLighting p16on, 100,"

	Lampz.MassAssign(17)= L17
	Lampz.MassAssign(17)= L17a
	Lampz.MassAssign(17)= F17
	Lampz.Callback(17) = "DisableLighting p17on, 100,"
	Lampz.MassAssign(18)= L18
	Lampz.MassAssign(18)= L18a
	Lampz.MassAssign(18)= F18
	Lampz.Callback(18) = "DisableLighting p18on, 100,"
	Lampz.MassAssign(19)= L19
	Lampz.MassAssign(19)= L19a
	Lampz.MassAssign(19)= F19
	Lampz.Callback(19) = "DisableLighting p19on, 100,"

	Lampz.MassAssign(20)= L20
	Lampz.MassAssign(20)= L20a
	Lampz.Callback(20) = "DisableLighting p20on, 100,"
	Lampz.MassAssign(21)= L21
	Lampz.MassAssign(21)= L21a
	Lampz.Callback(21) = "DisableLighting p21on, 100,"
	Lampz.MassAssign(22)= L22
	Lampz.MassAssign(22)= L22a
	Lampz.MassAssign(22)= F22
	Lampz.Callback(22) = "DisableLighting p22on, 100,"
	Lampz.MassAssign(23)= L23
	Lampz.MassAssign(23)= L23a
	Lampz.MassAssign(23)= F23
	Lampz.Callback(23) = "DisableLighting p23on, 100,"
	Lampz.MassAssign(24)= L24
	Lampz.MassAssign(24)= L24a
	Lampz.MassAssign(24)= F24
	Lampz.Callback(24) = "DisableLighting p24on, 100,"
	Lampz.MassAssign(25)= L25
	Lampz.MassAssign(25)= L25a
	Lampz.MassAssign(26)= F25
	Lampz.Callback(25) = "DisableLighting p25on, 100,"
	Lampz.MassAssign(26)= L26
	Lampz.MassAssign(26)= L26a
	Lampz.MassAssign(26)= F26
	Lampz.Callback(26) = "DisableLighting p26on, 100,"
	Lampz.MassAssign(27)= L27
	Lampz.MassAssign(27)= L27a
	Lampz.MassAssign(27)= F27
	Lampz.Callback(27) = "DisableLighting p27on, 100,"
	Lampz.MassAssign(28)= L28
	Lampz.MassAssign(28)= L28a
	Lampz.MassAssign(28)= F28 
	Lampz.Callback(28) = "DisableLighting p28on, 100,"
	Lampz.MassAssign(29)= L29
	Lampz.MassAssign(29)= L29a
	Lampz.MassAssign(29)= F29
	Lampz.Callback(29) = "DisableLighting p29on, 100,"
	Lampz.MassAssign(30)= L30
	Lampz.MassAssign(30)= L30a
	Lampz.MassAssign(30)= F30
	Lampz.Callback(30) = "DisableLighting p30on, 100,"

	Lampz.MassAssign(31)= l31
	Lampz.MassAssign(31)= L31a
	Lampz.Callback(31) = "DisableLighting p31on, 100,"
	Lampz.MassAssign(32)= L32
	Lampz.MassAssign(32)= L32a
	Lampz.Callback(32) = "DisableLighting p32on, 100,"
	Lampz.MassAssign(33)= L33
	Lampz.MassAssign(33)= L33a
	Lampz.Callback(33) = "DisableLighting p33on, 100,"
	Lampz.MassAssign(34)= L34
	Lampz.MassAssign(34)= L34a
	Lampz.Callback(34) = "DisableLighting p34on, 100,"

	Lampz.MassAssign(37)= L37
	Lampz.MassAssign(37)= L37a
	Lampz.Callback(37) = "DisableLighting p37on, 100,"

	Lampz.MassAssign(38)= L38
	Lampz.MassAssign(38)= L38a
	Lampz.Callback(38) = "DisableLighting p38on, 100,"

	Lampz.MassAssign(35)= L35
	Lampz.MassAssign(35)= L35a
	Lampz.Callback(35) = "DisableLighting p35on, 100,"

	Lampz.MassAssign(36)= L36
	Lampz.MassAssign(36)= L36a
	Lampz.MassAssign(36)= L36b
	Lampz.Callback(36) = "DisableLighting p36on, 100,"

	Lampz.MassAssign(45)= L45
	Lampz.MassAssign(45)= L45a
	Lampz.Callback(45) = "DisableLighting p45on, 100,"

	Lampz.MassAssign(39)= L39
	Lampz.MassAssign(39)= L39a
	Lampz.Callback(39) = "DisableLighting p39on, 100,"

	
	Lampz.MassAssign(40)= L40
	Lampz.MassAssign(40)= L40a
	Lampz.Callback(40) = "DisableLighting p40on, 100,"

	Lampz.MassAssign(41)= L41
	Lampz.MassAssign(41)= L41a
	Lampz.Callback(41) = "DisableLighting p41on, 100,"

	Lampz.MassAssign(46)= L46
	Lampz.MassAssign(46)= L46a
	Lampz.Callback(46) = "DisableLighting p46on, 100,"

	Lampz.MassAssign(47)= L47
	Lampz.MassAssign(47)= L47a
	Lampz.Callback(47) = "DisableLighting p47on, 100,"

	Lampz.MassAssign(48)= L48
	Lampz.MassAssign(48)= L48a
	Lampz.Callback(48) = "DisableLighting p48on, 100,"

	Lampz.MassAssign(49)= L49
	Lampz.MassAssign(49)= L49a
	Lampz.Callback(49) = "DisableLighting p49on, 100,"

	Lampz.MassAssign(50)= L50
	Lampz.MassAssign(50)= L50a
	Lampz.Callback(50) = "DisableLighting p50on, 100,"

	Lampz.MassAssign(51)= L51
	Lampz.MassAssign(51)= L51a
	Lampz.Callback(51) = "DisableLighting p51on, 100,"

	Lampz.MassAssign(52)= L52
	Lampz.MassAssign(52)= L52a
	Lampz.Callback(52) = "DisableLighting p52on, 100,"


	Lampz.MassAssign(53)= L53
	Lampz.MassAssign(53)= L53a
	Lampz.Callback(53) = "DisableLighting p53on, 100,"

	Lampz.MassAssign(54)= L54
	Lampz.MassAssign(54)= L54a
	Lampz.Callback(54) = "DisableLighting p54on, 100,"

	Lampz.MassAssign(55)= L55
	Lampz.MassAssign(55)= L55a
	Lampz.Callback(55) = "DisableLighting p55on, 100,"

	Lampz.MassAssign(56)= L56
	Lampz.MassAssign(56)= L56a
	Lampz.Callback(56) = "DisableLighting p56on, 100,"

	Lampz.MassAssign(57)= L57
	Lampz.MassAssign(57)= L57a
	Lampz.Callback(57) = "DisableLighting p57on, 100,"

	Lampz.MassAssign(58)= L58
	Lampz.MassAssign(58)= L58a
	Lampz.Callback(58) = "DisableLighting p58on, 100,"

	Lampz.MassAssign(59)= L59
	Lampz.MassAssign(59)= L59a
	Lampz.Callback(59) = "DisableLighting p59on, 100,"

	Lampz.MassAssign(60)= L60
	Lampz.MassAssign(60)= L60a
	Lampz.Callback(60) = "DisableLighting p60on, 100,"

	Lampz.MassAssign(61)= L61
	Lampz.MassAssign(61)= L61a
	Lampz.Callback(61) = "DisableLighting p61on, 100,"
	Lampz.MassAssign(63)= L63
	Lampz.MassAssign(63)= L63a
	Lampz.Callback(63) = "DisableLighting p63on, 100,"
	Lampz.MassAssign(64)= L64
	Lampz.MassAssign(64)= L64a
	Lampz.Callback(64) = "DisableLighting p64on, 100,"



If VRRoom > 0 then
Lampz.MassAssign(1)= VRgameover
Lampz.MassAssign(2)= VRmatch
Lampz.MassAssign(3)= VRshootagain
Lampz.MassAssign(6)= VRBallinPlay
End if

	Lampz.Callback(35) = "DisableLighting p35on, 100,"
	Lampz.MassAssign(3)= L3
	Lampz.MassAssign(3)= L3b	

	Lampz.Callback(3) = "DisableLighting p3on, 100,"
	Lampz.MassAssign(4)= L4
	Lampz.MassAssign(4)= L4a
	Lampz.MassAssign(4)= F4
	Lampz.MassAssign(4)= F4a
	Lampz.Callback(4) = "DisableLighting p4on, 100,"
	Lampz.MassAssign(5)= L5
	Lampz.MassAssign(5)= L5a
	Lampz.MassAssign(5)= F5
	Lampz.Callback(5) = "DisableLighting p5on, 100,"

	Lampz.MassAssign(62)= L62
	Lampz.MassAssign(62)= L62a
	Lampz.Callback(62) = "DisableLighting p62on, 100,"

	Lampz.MassAssign(105)= F105
'	Lampz.MassAssign(105)= F105a
	Lampz.MassAssign(105)= F105b
	Lampz.MassAssign(105)= F105c
	Lampz.MassAssign(105)= F105d
	Lampz.MassAssign(105)= F105e
	Lampz.MassAssign(105)= F105f
	Lampz.MassAssign(106)= F106
'	Lampz.MassAssign(106)= F106a
	Lampz.MassAssign(106)= F106b
	Lampz.MassAssign(106)= F106c
	Lampz.MassAssign(106)= F106d
	Lampz.MassAssign(106)= F106e
	Lampz.MassAssign(106)= F106f

	Lampz.MassAssign(120)=L001
	Lampz.MassAssign(120)=L001a
	Lampz.MassAssign(120)=F001
	Lampz.MassAssign(120)=F001a
	Lampz.MassAssign(120)=L002
	Lampz.MassAssign(120)=L002a
	Lampz.MassAssign(120)=F002
	Lampz.MassAssign(120)=F002a
	Lampz.Callback(120) = "DisableLighting pTriggerRon, 100,"
	Lampz.Callback(120) = "DisableLighting pTriggerLon, 100,"

	Lampz.MassAssign(42)= l42 'Traffic Lights'
	Lampz.MassAssign(42)= f42
	Lampz.MassAssign(43)= l43 'Traffic Lights'
	Lampz.MassAssign(43)= f43
	Lampz.MassAssign(44)= l44 'Traffic Lights'
	Lampz.MassAssign(44)= f44
	

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
'*****   3D INSERTS
'******************************************************
'
'
' Before you get started adding the inserts to your playfield in VPX, there are a few things you need to have done to prepare:
' 	1. Cut out all the inserts on the playfield image so there is alpha transparency where they should be.
'      Make sure the playfield material has Opacity Active checkbox checked.
'	2. All the  insert text and/or images that lie over the insert plastic needs to be in its own file with 
'	   alpha transparency. Many playfields may require finding the original font and remaking the insert text.
' 
' To add the inserts:
'	1. Import all the textures (images) and materials from this file that start with the word "Insert" into your Table
'   2. Copy and past the two primitves that make up the insert you want to use. One primitive is for the on state, the other for the off state.
'   3. Align the primitives with the associated insert light. Name the on and off primitives correctly.
'   4. Update the Lampz object array. Follow the example in this file.
'   5. You will need to manually tweak the disable lighting value and material parameters to achielve the effect you want.
'
'
' Quick Reference:  Laying the Inserts ( Tutorial From Iaakki)
' - Each insert consists of two primitives. On and Off primitive. Suggested naming convention is to use lamp number in the name. For example 
'   is lamp number is 57, the On primitive is "p57" and the Off primitive is "p57off". This makes it easier to work on script side.
' - When starting from a new table, I'd first select to make few inserts that look quite similar. Lets say there is total of 6 small triangle 
'   inserts, 4 yellow and 2 blue ones.
' - Import the insert on/off images from the image manager and the vpx materials used from the sample project first, and those should appear 
'   selected properly in the primitive settings when you paste your actual insert trays in your target table . Then open up your target project 
'   at same time as the sample project and use copy&paste to copy desired inserts to target project. 
' - There are quite many parameters in primitive that affect a lot how they will look. I wouldn't mess too much with them. Use Size options to 
'   scale the insert properly into PF hole. Some insert primitives may have incorrect pivot point, which means that changing the depth, you may 
'   also need to alter the Z-position too.
' - Once you have the first insert in place, wire it up in the script (detailed in part 3 below). Then set the light bulb's intensity to zero, 
'   so it won't harass the adjustment.
' - Start up the game with F6 and visually see if the On-primitive blinks properly. If it is too dim, hit D and open editor. Write: 
' - p57.BlendDisableLighting = 300 and hit enter
' - -> The insert should appear differently. Find good looking brightness level. Not too bright as the light bulb is still missing. Just generic good light.
'     - If you cannot find proper light color or "mood", you can also fiddle with primitive material values. Provided material should be 
'       quite ok for most of the cases.
'     - Now when you have found proper DL value (165), but that into script:
'     - Lampz.Callback(57) = " DisableLighting p57, 165,"
' - That one insert is now adjusted and you should be able to copy&paste rest of the triangle inserts in place and name them correctly. And add them 
'   into script. And fine tune their brightness and color.
'
' Light bulbs and ball reflection:
' 
' - This kind of lighted primitives are not giving you ball reflections. Also some more glow vould be needed to make the insert to bloom correctly.
' - Take the original lamp (l57), set the bulb mode enabled, set Halo Height to -3 (something that is inside the 2 insert primitives). I'd start with 
'   falloff 100, falloff Power 2-2.5, Intensity 10, scale mesh 10, Transmit 5.
' - Start the game with F6, throw a ball on it and move the ball near the blinking insert. Visually see how the reflection looks.
' - Hit D once the reflection is the highest. Open light editor and start fine tuning the bulb values to achieve realistic look for the reflection.
' - Falloff Power value is the one that will affect reflection creatly. The higher the power value is, the brighter the reflection on the ball is. 
'   This is the reason why falloff is rather large and falloff power is quite low. Change scale mesh if reflection is too small or large.
' - Transmit value can bring nice bloom for the insert, but it may also affect to other primitives nearby. Sometimes one need to set transmit to 
'   zero to avoid affecting surrounding plastics. If you really need to have higher transmit value, you may set Disable Lighting From Below to 1 
'   in surrounding primitive. This may remove the problem, but can make the primitive look worse too.



'******************************************************
'*****   END 3D INSERTS
'******************************************************


'VR Stuff Below..*************************************************************************
'*****************************************************************************************

Sub CopTimer_timer()

If PoliceOn = false then
VRRVBLUE.image = "BGCopBlue"
VRbackglass.image = "BGNEWLIGHT"
copchassis.image = "CarBlue"
copwindows.image = "WindowBlue"
CarLightBlue.visible = true
CarLightBlue1.visible = true
CarLightBlue2.visible = true
CarlightBlue3.visible = true
CarlightBlue4.visible = true
CarLightRed.visible = false
CarLightRed1.visible = false
CarLightRed2.visible = false
CarlightRed3.visible = false
CarlightRed4.visible = false
PoliceOn = true
Else

VRRVBLUE.image = "BGCopRed"
VRbackglass.image = "BGNEWDARK"
copchassis.image = "CarRed"
copwindows.image = "WindowRed"
CarLightBlue.visible = false
CarLightBlue1.visible = false
CarLightBlue2.visible = false
CarlightBlue3.visible = false
CarlightBlue4.visible = false
CarLightRed.visible = true
CarLightRed1.visible = true
CarLightRed2.visible = true
CarlightRed3.visible = True
CarlightRed4.visible = True
PoliceOn = false
End if
End Sub


CopLightsOFF ' runs once at table start to reset Copcar lights, and is also called when the beaconTimer stops (rom controlled)

Sub CopLightsOFF()  ' for turning cop lights Off at end of beacon lighting...

VRRVBLUE.image = "BGCopOff2"
CopTimer.enabled = false
VRbackglass.image = "BGNEWLIGHT"
copchassis.image = "CarBlue"
copwindows.image = "WindowBlue"
CarLightBlue.visible = false
CarLightBlue1.visible = false
CarLightBlue2.visible = false
CarlightBlue3.visible = false
CarlightBlue4.visible = false
CarLightRed.visible = false
CarLightRed1.visible = false
CarLightRed2.visible = false
CarlightRed3.visible = false
CarlightRed4.visible = false
End Sub


Sub BeaconTimer_Timer
	BeaconPos = BeaconPos + 3
	if BeaconPos = 360 then BeaconPos = 0
	BeaconRedInt.RotY = BeaconPos
    BeaconRed.BlendDisableLighting=.5 * abs(sin((BeaconPos+90) * 6.28 / 360))
	BeaconFR.RotY = BeaconPos
    if BeaconPos < 90 or BeaconPos > 270 then BeaconFr.IntensityScale = -.05  else BeaconFr.IntensityScale = 1
End Sub

Sub policelight(Enabled)
    If Enabled then
		If VRRoom = 1 then  ' ONLY RUN THE POLICE LIGHT AND BEACON IN MAIN VR ROOM
        BeaconRed.image = "dome3_red_lit"
        BeaconTimer.Enabled = true
		CopTimer.enabled = true
		BeaconFR.opacity =10000
		End if
    Else
		If VRRoom = 1 then
		BeaconTimer.Enabled = false
        BeaconRed.image = "dome3_red"
		BeaconFR.opacity =0
        CopLightsOFF  ' turns the lights OFF and turns off the timers.
        if BeaconPos < 90 or BeaconPos > 270 then BeaconFr.IntensityScale = +.001  else BeaconFr.IntensityScale = .001	
		End If
    End If
End Sub

'*****VR Flasher******

Sub FlashSol100(Enabled)
	If enabled Then
		If VRbackglass.image = "BGNEWLIGHT" Then
			VRbackglass.image = "BGNEWLIGHTFLASH"
		End If
	Else
		VRbackglass.image = "BGNEWLIGHT"
	End If
End Sub


' Below is the code needed to run the VR plungers.  Both analog and button press. Will only run if VRroom = 1 *****************
'******************************************************************************************************************************

Sub TimerVRPlunger_Timer
if VRPlunger.Y < 2240 then VRPlunger.Y = VRPlunger.y +6 
End Sub

Sub TimerVRPlunger2_Timer
VRPlunger.Y = 2129 + (5* Plunger.Position) -20  ' This follows our dummy plunger position for analog plunger hardware users.
end sub




' New shadow code...


Const lob = 0						'Locked balls


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




' Dynamic Shadows
'******************************************************************************************************************************

' Dynamic Shadow stuff, Don't touch.
Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 1	    '0 to 1, higher is darker
Const AmbientBSFactor 		= 0.8	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source


Dim sourcenames, currentShadowCount, DSSources(30), numberofsources, numberofsources_hold
sourcenames = Array ("","","","","","","","","","","","")
currentShadowCount = Array (0,0,0,0,0,0,0,0,0,0,0,0)

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
		objrtx1(iii).z = iii/1000 + 0.01
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = (iii)/1000 + 0.02
		objrtx2(iii).visible = 0

		currentShadowCount(iii) = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = iii/1000 + 0.04
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
	numberofsources_hold = iii
end sub


Sub DynamicBSUpdate
	Dim falloff:	falloff = 150			'Max distance to light sources, can be changed if you have a reason
	Dim ShadowOpacity, ShadowOpacity2 
	Dim s, Source, LSd, currentMat, AnotherSource, BOT, iii
	BOT = GetBalls

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
				'objBallShadow(s).visible = 1
				If BOT(s).Y < 220 then objBallShadow(s).visible = 0 else objBallShadow(s).visible = 1

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
				'objBallShadow(s).Y = BOT(s).Y + fovY
				objBallShadow(s).Y = BOT(s).Y + 12  ' I think this works better. - Rawd
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
				For iii = 0 to numberofsources - 1 
					LSd=DistanceFast((BOT(s).x-DSSources(iii)(0)),(BOT(s).y-DSSources(iii)(1)))	'Calculating the Linear distance to the Source
					If LSd < falloff And gilvl > 0 Then						    'If the ball is within the falloff range of a light and light is on (we will set numberofsources to 0 when GI is off)
						currentShadowCount(s) = currentShadowCount(s) + 1		'Within range of 1 or 2
						if currentShadowCount(s) = 1 Then						'1 dynamic shadow source
							sourcenames(s) = iii
							currentMat = objrtx1(s).material
							objrtx2(s).visible = 0 : objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01						'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity = (falloff-LSd)/falloff									'Sets opacity/darkness of shadow by distance to light
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness						'Scales shape of shadow with distance/opacity
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^2,RGB(0,0,0),0,0,False,True,0,0,0,0
							If AmbientBallShadowOn = 1 Then
								currentMat = objBallShadow(s).material									'Brightens the ambient primitive when it's close to a light
								UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-ShadowOpacity),RGB(0,0,0),0,0,False,True,0,0,0,0
							Else
								BallShadowA(s).Opacity = 100*AmbientBSFactor*(1-ShadowOpacity)
							End If

						Elseif currentShadowCount(s) = 2 Then
																	'Same logic as 1 shadow, but twice
							currentMat = objrtx1(s).material
							AnotherSource = sourcenames(s)
							objrtx1(s).visible = 1 : objrtx1(s).X = BOT(s).X : objrtx1(s).Y = BOT(s).Y + fovY
	'						objrtx1(s).Z = BOT(s).Z - 25 + s/1000 + 0.01							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx1(s).rotz = AnglePP(DSSources(AnotherSource)(0),DSSources(AnotherSource)(1), BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity = (falloff-DistanceFast((BOT(s).x-DSSources(AnotherSource)(0)),(BOT(s).y-DSSources(AnotherSource)(1))))/falloff
							objrtx1(s).size_y = Wideness*ShadowOpacity+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0

							currentMat = objrtx2(s).material
							objrtx2(s).visible = 1 : objrtx2(s).X = BOT(s).X : objrtx2(s).Y = BOT(s).Y + fovY
	'						objrtx2(s).Z = BOT(s).Z - 25 + s/1000 + 0.02							'Uncomment if you want to add shadows to an upper/lower pf
							objrtx2(s).rotz = AnglePP(DSSources(iii)(0), DSSources(iii)(1), BOT(s).X, BOT(s).Y) + 90
							ShadowOpacity2 = (falloff-LSd)/falloff
							objrtx2(s).size_y = Wideness*ShadowOpacity2+Thinness
							UpdateMaterial currentMat,1,0,0,0,0,0,ShadowOpacity2*DynamicBSFactor^3,RGB(0,0,0),0,0,False,True,0,0,0,0
							If AmbientBallShadowOn = 1 Then
								currentMat = objBallShadow(s).material									'Brightens the ambient primitive when it's close to a light
								UpdateMaterial currentMat,1,0,0,0,0,0,AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2)),RGB(0,0,0),0,0,False,True,0,0,0,0
							Else
								BallShadowA(s).Opacity = 100*AmbientBSFactor*(1-max(ShadowOpacity,ShadowOpacity2))
							End If
						end if
					Else
						currentShadowCount(s) = 0
						BallShadowA(s).Opacity = 100*AmbientBSFactor
					End If
				Next
			Else									'Hide dynamic shadows everywhere else
				objrtx2(s).visible = 0 : objrtx1(s).visible = 0
			End If
		End If
	Next
End Sub
'****************************************************************
'****  END VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'****************************************************************


