'Simpsons Treehouse of Horror MOD for...

'The Simpsons Party Pinball (Stern 2003)
'https://www.ipdb.org/machine.cgi?id=6154
'                                 ___    _
'                                  | |_||_
'      sssSSSSSs                   | | ||_
'   sSSSSSSSSSSSs                           sSSSSs             nn   sSSSs
'  SSSS           ii  mM     mmm   pPPPPpp sSS           nn    nn sSSSSSS
' SSSs           iII mMMMM  mMmmm pPP  PpppSs      oOoo  nNN   nN SS   SS
' SSSs           iII mMMMMM mM Mm Pp     PPSSSSs  OOOOOO NNNn nNN SSSs
' SSSSSSSssss    iIi mMM MMmM  Mm ppPPppPP  SSSSsoO   OO NNNNNNNN  SSSSss
'    SSSSSSSSSs  iIi MMM  MMM  Mm PPPPppP      sSOO   OO NN  nNNN     SSSs
'          SSSS IIi  mMM  MMm  Mm  Pp   sSSssSSSSOO ooOO nN   NN        SS
'           sSS III   MM       MMm pPp    SSSSSS  OOOOO          sssssSsSS
'sSSsssssSSSSS   II                                               SSSSSSS
'  SSSSSSSSS
'                               PINBALL PARTY
'
'************************************
'Simpson's Pinball Party Team VPW Mod
'************************************

'Project Lead - idigstuff
'VPW Team - apophis, sixtoe, tomate, bord, nestorgian, iaakki, skitso
'VR World - rawd
'Desktop BG - hauntfreaks
'Testing - Rik, PinstratsDan, BountyBob

'Original Table Authors Coindropper, 32assassin
'3D models : Dark, Watacaractr
'Major Mods by: HauntedFreaks, Neofr45, VPW

'*********************************

Option Explicit
Randomize

'*************************************************************
'User Options
'*************************************************************

' ---- Flasher Option ---- 
Const PWMflashers = False 			    'Enable more realistic flashers. Requires 2023 VPinMame 3.6 beta or later - Reverted to False for this release. 2.0.2 default was False - redbone

'----- VR Options -----
Const VRRoomChoice = 1					'1 - Simpsons Room, 2 - Minimal Room, 3 - Ultra-Minimal Room
Const VRBackglassGI = 1					'0 - Static VR Backglass (like real Table), 1 - VR Backglass shuts off with table GI
Const GlassScratches = 0				'0 - Off, 1 - Glass scratches on  (Will only run in VR rooms 1 and 2)

'----- General Sound Options -----
Const VolumeDial = 0.8					'Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Const BallRollVolume = 0.4 				'Level of ball rolling volume. Value between 0 and 1
Const RampRollVolume = 0.4 				'Level of ramp rolling volume. Value between 0 and 1
Const intro = 1 						'Music Intro 1=on, 0=off

'----- Shadow Options -----
Const DynamicBallShadowsOn = 1			'0 = no dynamic ball shadow ("triangles" near slings and such), 1 = enable dynamic ball shadow
Const AmbientBallShadowOn = 1			'0 = Static shadow under ball ("flasher" image, like JP's)
										'1 = Moving ball shadow ("primitive" object, like ninuzzu's) - This is the only one that shows up on the pf when in ramps and fades when close to lights!
										'2 = flasher image shadow, but it moves like ninuzzu's
'----- Other Options -----
Const CabinetMode = 0					'0 = Off, 1 = Side blades longer, siderails hidden, other adjustments
Const SideBladeArt = 0					'0 = Default black side blades, 1 = Side blade artwork
Const CabinetSideBladeStretch=true     'True= Side blades stretched taller in cab mode. False= Sideblades do not stretch in cab mode
Const DuffCanToy = 0					'0 = Default dome, 1 = Duff Can "Oktoberfest"
Const Garagedecal = 0					'0 = Original decal, 1 = Quick-e-Mart decal
Const Moetoy = 0						'0 =  no Moe 1 = Moe at upper right
Const Plungerlight= 1					'0 = Original (no plunger light), 1 = Plunger light enabled
Const BlueBumperTowers = 1				'0 = Original (grey towers), 1 = Blue Towers w/halloween spray paint tag


'*************************************************************
'End of User Options
'*************************************************************


'*************************************************************
'Loading
'*************************************************************

Dim tablewidth, tableheight : tablewidth = table1.width : tableheight = table1.height
Const BallSize = 50		'Ball size must be 50
Const BallMass = 1		'Ball mass must be 1
Const tnob = 5	 		'Total number of balls
Const lob = 0	 		'Locked balls

Dim DesktopMode:DesktopMode = Table1.ShowDT
Dim UseVPMDMD, VRRoom
If RenderingMode = 2 Then VRRoom = VRRoomChoice Else VRRoom = 0
If VRRoom <> 0 Then UseVPMDMD = True Else UseVPMDMD = DesktopMode

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="simpprty",UseSolenoids=2,UseLamps=0,UseGI=0,UseSync=1,HandleMech=1

dim UseVPMModSol : UseVPMModSol = cBool(PWMflashers)
LoadVPM "01560000", "Sega.VBS", 3.02

If PWMflashers and Controller.Version < "03060000" Then msgbox "VPinMAME ver 3.6 beta or later is required. Or set script option PWMflashers = False"

'1001 to 1032 for solenoids, 1201 to 1025 for GIs (WPC only), 1301+ for lamps (not yet implemented)
Const VPM_MODOUT_DEFAULT              =   0 ' Uses default driver modulated solenoid implementation
Const VPM_MODOUT_PWM_RATIO            =   1 ' pulse ratio over the last integration period, allow (approximated) device emulation by the calling app
Const VPM_MODOUT_BULB_44_6_3V_AC      = 100 ' Incandescent #44/555 Bulb connected to 6.3V, commonly used for GI
Const VPM_MODOUT_BULB_47_6_3V_AC      = 101 ' Incandescent #47 Bulb connected to 6.3V, commonly used for (darker) GI with less heat
Const VPM_MODOUT_BULB_86_6_3V_AC      = 102 ' Incandescent #86 Bulb connected to 6.3V, seldom use: TZ, CFTBL,...
Const VPM_MODOUT_BULB_44_18V_DC_WPC   = 201 ' Incandescent #44/555 Bulb connected to 18V, commonly used for lamp matrix with short strobing
Const VPM_MODOUT_BULB_44_18V_DC_GTS3  = 202 ' Incandescent #44/555 Bulb connected to 18V, commonly used for lamp matrix with short strobing
Const VPM_MODOUT_BULB_44_18V_DC_S11   = 203 ' Incandescent #44/555 Bulb connected to 18V, commonly used for lamp matrix with short strobing
Const VPM_MODOUT_BULB_89_20V_DC_WPC   = 301 ' Incandescent #89/906 Bulb connected to 12V, commonly used for flashers
Const VPM_MODOUT_BULB_89_20V_DC_GTS3  = 302 ' Incandescent #89/906 Bulb connected to 12V, commonly used for flashers
Const VPM_MODOUT_BULB_89_32V_DC_S11   = 303 ' Incandescent #89/906 Bulb connected to 32V, used for flashers on S11 with output strobing
Const VPM_MODOUT_LED                  = 400 ' LED PWM (in fact mostly human eye reaction, since LED are nearly instantaneous)

Sub InitPWM() ' called from Table_Init
	dim BulbType : BulbType = VPM_MODOUT_BULB_89_20V_DC_WPC
'	dim BulbType : BulbType = VPM_MODOUT_LED

	Controller.SolMask(1021) = BulbType 'Flash Pops Clear/Green
	Controller.SolMask(1022) = BulbType 'R.Ramp Red
	Controller.SolMask(1023) = BulbType 'R.Ramp Orange / Duff Can

	Controller.SolMask(1025) = BulbType 'Flash Itchy
	Controller.SolMask(1026) = BulbType 'Flash Scratchy
	Controller.SolMask(1027) = BulbType 'Homer Head
	Controller.SolMask(1028) = BulbType 'Flash Couch
	Controller.SolMask(1029) = BulbType 'Flash Comic Book Guy

	Controller.SolMask(1031) = BulbType 'UPF Orange
	Controller.SolMask(1032) = BulbType 'UPF Red
end Sub


' VR Dims *******************************************
Dim Obj
Dim BartMove0:BartMove0 = True
Dim BartMove1:BartMove1 = false
Dim BartMove2:BartMove2 = false
Dim BartMove3:BartMove3 = false
Dim BartMove4:BartMove4 = false
Dim BartMove5:BartMove5 = false
Dim BartMove6:BartMove6 = false
Dim TruckMove1:TruckMove1 = true
Dim TruckMove2:TruckMove2 = False
Dim TruckMove3:TruckMove3 = false
Dim TruckMove4:TruckMove4 = False
Dim TruckMove5:TruckMove5 = False
Dim MaggieMove1:MaggieMove1 = True
Dim MaggieMove2:Maggiemove2 = False
Dim BartRunning:BartRunning = False
Dim MoeSpeed:MoeSpeed = 0.08
Dim BrainMove:BrainMove = 0.02
Dim BrainMove2:BrainMove2 = 0.025
Dim BrainMove3:BrainMove3 = 0.03
Dim BrainCount:BrainCount = 1
Dim SootherSpeed:SootherSpeed = 0.4
Dim Carnumber:Carnumber = 1
Dim PassingCar:PassingCar = 1

'END VR Dims *****************************************


if BlueBumperTowers = 1 Then
	bcap1Lit.image = "bumptop_diff_blue"
	bcap2Lit.image = "bumptop_diff_blue"
end if
'set VR Room - don't touch

If VRRoom=0 then 
	SetVRRoomSign.visible = true
Else
	SetVRRoomSign.visible = false
end if

If VRRoom=1 then 
	for each obj in VRRoomStuff: Obj.visible = true: Next
	for each obj in VRCabinet: Obj.visible = true: Next
	for each obj in VRBackglass: Obj.visible = true: Next
    'Below need to be set non visible because they are part of collection code
	VRChoke.visible = false
	VRDonut.visible = false
	VRBacon.visible = false ' ?
	VRBartShadow.visible = false
	VRCarShadow.visible = false
	VRNewcar2.visible = false
	VRNewcar3.visible = false
	VRNewcar4.visible = false
	MainVRTimer.enabled = true
	VRStartBartTimer.enabled = true
	HomerBrainTimer.enabled = true
	TextBox1.visible = 0
    TimerVRPlunger2.Enabled = True
	DMD.visible = true
	SetBackglass
	If GlassScratches = 1 then  GlassImpurities.visible = true
end If

If VRRoom=2 then 
	for each obj in VRMinimal: Obj.visible = true: Next
	for each obj in VRCabinet: Obj.visible = true: Next
	for each obj in VRBackglass: Obj.visible = true: Next
	TextBox1.visible = 0
	TimerVRPlunger2.Enabled = True
	DMD.visible = true
	If GlassScratches = 1 then  GlassImpurities.visible = true
	SetBackglass
end If

If VRRoom=3 then 
	PinCab_Backbox.visible = true
	DMDHousing.visible = true
	TextBox1.visible = 0
	DMD.visible = true
	for each obj in VRBackglass: Obj.visible = true: Next
	SetBackglass
end If


' save the insert intensities so they can be updated when GI is off
InitInsertIntensities
Sub InitInsertIntensities
	dim bulb
	for each bulb in Inserts
		bulb.uservalue = bulb.intensity
	next
End sub


'*******************************************
'  Timers
'*******************************************

' The game timer interval is 10 ms
Sub GameTimer_Timer()
	RollingUpdate					'update rolling sounds
	
	If VRroom >0 and VRroom <3 then
	VRStartButton.blenddisablelighting = l31.IntensityScale  'lights start button - iDigStuff already had a lamp assigned for the plunger lane l31
	end if

End Sub

' The frame timer interval is -1, so executes at the display frame rate
dim FrameTime, InitFrameTime : InitFrameTime = 0
Sub FrameTimer_Timer()
	FrameTime = gametime - InitFrameTime : InitFrameTime = gametime

	If DynamicBallShadowsOn Or AmbientBallShadowOn Then DynamicBSUpdate 'update ball shadows

    FlipperLSh.RotZ = LeftFlipper.currentangle
    FlipperRSh.RotZ = RightFlipper.currentangle
    FlipperRSh1.RotZ = RightFlipper2.currentangle

	LFLogo.RotZ = LeftFlipper.CurrentAngle
	RFlogo.RotZ = RightFlipper.CurrentAngle
	RF2logo.RotZ = RightFlipper2.CurrentAngle

	bart1a.X = capBall.X
	bart1a.Y = capBall.Y
	board1a.X = capBall.X
	board1a.Y = capBall.Y
	bart1aLit.x = bart1a.X
	bart1aLit.y = bart1a.y

End Sub



'*************************************************************
'Solenoid Call backs
'*************************************************************
 SolCallBack(1) = "SolRelease"
 SolCallBack(2) = "Auto_Plunger"
 SolCallback(3) = "CouchExit"'"vlLock.SolExit"
 SolCallback(4) = "SW17.blenddisablelighting = 0 : SW18.blenddisablelighting = 0: SW19.blenddisablelighting = 0:dtDrop.SolDropUp"
 SolCallback(5) = "bsBR.SolOut"
 SolCallback(6) = "bsVUK.SolOut"
 SolCallback(7) = "SolTVRelease"
 SolCallback(8) = "SolHomer"

 SolCallback(12) = "SolTopLeftFlipper"
 SolCallback(13) = "SolTopRightFlipper"
 SolCallback(14) = "SolRightFlipper2"

 SolCallBack(19) = "bsTR.SolOut"
 SolCallBack(20) =  "GarageUp"  
 SolCallBack(30) = "SolDropBankTrips"

'Flippers
SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		LF.Fire  'leftflipper.rotatetoend
		TopLeftFlipper.RotateToEnd

		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then 
			RandomSoundReflipUpLeft LeftFlipper
		Else 
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If		
	Else
		LeftFlipper.RotateToStart
		TopLeftFlipper.RotateToStart

		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		RF.Fire 'rightflipper.rotatetoend
		RightFlipper2.RotateToEnd
		TopRightFlipper.RotateToEnd

		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else 
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		RightFlipper.RotateToStart
		RightFlipper2.RotateToStart
		TopRightFlipper.RotateToStart
		
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub


Sub SolRightFlipper2(Enabled)
	If RFPress = 0 Then  'Only do the following if flipper actuatated while there is no button press from player
		If Enabled Then
			RightFlipper2.RotateToEnd

			If RightFlipper2.currentangle > RightFlipper2.endangle - ReflipAngle Then
				RandomSoundReflipUpRight RightFlipper2
			Else 
				SoundFlipperUpAttackRight RightFlipper2
				RandomSoundFlipperUpRight RightFlipper2
			End If
		Else
			RightFlipper2.RotateToStart
			
			If RightFlipper2.currentangle > RightFlipper2.startAngle + 5 Then
				RandomSoundFlipperDownRight RightFlipper2
			End If	
			FlipperRightHitParm = FlipperUpSoundLevel
		End If
	End If
End Sub

Sub SolTopLeftFlipper(enabled)
	If LFPress = 0 Then 'Only do the following if flipper actuatated while there is no button press from player
		If Enabled Then
			TopLeftFlipper.RotateToEnd

			If TopLeftFlipper.currentangle > TopLeftFlipper.endangle - ReflipAngle Then
				RandomSoundReflipUpRight TopLeftFlipper
			Else 
				SoundFlipperUpAttackRight TopLeftFlipper
				RandomSoundFlipperUpRight TopLeftFlipper
			End If
		Else
			TopLeftFlipper.RotateToStart
			
			If TopLeftFlipper.currentangle > TopLeftFlipper.startAngle + 5 Then
				RandomSoundFlipperDownRight TopLeftFlipper
			End If	
			FlipperRightHitParm = FlipperUpSoundLevel
		End If
	End If
End Sub

Sub SolTopRightFlipper(enabled)
	If RFPress = 0 Then 'Only do the following if flipper actuatated while there is no button press from player
		If Enabled Then
			TopRightFlipper.RotateToEnd

			If TopRightFlipper.currentangle > TopRightFlipper.endangle - ReflipAngle Then
				RandomSoundReflipUpRight TopRightFlipper
			Else 
				SoundFlipperUpAttackRight TopRightFlipper
				RandomSoundFlipperUpRight TopRightFlipper
			End If
		Else
			TopRightFlipper.RotateToStart
			
			If TopRightFlipper.currentangle > TopRightFlipper.startAngle + 5 Then
				RandomSoundFlipperDownRight TopRightFlipper
			End If	
			FlipperRightHitParm = FlipperUpSoundLevel
		End If
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

Sub RightFlipper2_Collide(parm)
	RandomSoundRubberFlipper(parm)
End Sub

Sub TopLeftFlipper_Collide(parm)
	RandomSoundRubberFlipper(parm)
End Sub

Sub TopRightFlipper_Collide(parm)
	RandomSoundRubberFlipper(parm)
End Sub





'#########################################################################
'nFozzy Flippers
'#########################################################################
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
        addpt "Velocity", 2, 0.41, 1.05
        addpt "Velocity", 3, 0.53,  1'0.982
        addpt "Velocity", 4, 0.702, 0.968
        addpt "Velocity", 5, 0.95,  0.968
        addpt "Velocity", 6, 1.03,  0.945

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
'                FLIPPER POLARITY AND RUBBER DAMPENER
'                        SUPPORTING FUNCTIONS 
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


'******************************************************
'                        FLIPPER TRICKS
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
        FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
        FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
		FlipperTricks RightFlipper2, RF2Press, RF2Count, RF2EndAngle, RF2State
		FlipperTricks TopLeftFlipper, TLFPress, TLFCount, TLFEndAngle, TLFState
		FlipperTricks TopRightFlipper, TRFPress, TRFCount, TRFEndAngle, TRFState
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

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

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
' End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, RF2Press, TLFPress, TRFPress, LFCount, RFCount, RF2Count, TLFCount, TRFCount
dim LFState, RFState, RF2State, TLFState, TRFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle, RF2EndAngle, TLFEndAngle, TRFEndAngle

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
Const EOSTnew = 0.8 '90's and later
Const EOSAnew = 1
Const EOSRampup = 0
Const SOSRampup = 2.5

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle
RF2EndAngle = RightFlipper2.endangle
TLFEndAngle = TopLeftFlipper.endangle
TRFEndAngle = TopRightFlipper.endangle

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
	AddSlingsPt 1, 0.45,	-6
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	6
	AddSlingsPt 5, 1.00,	4

End Sub


Sub AddSlingsPt(idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LS, RS)
	dim x : for each x in a
		x.addpoint idx, aX, aY
	Next
End Sub

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

Sub RDampen_Timer()
	Cor.Update
End Sub



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




'**********************************************************************************************************
'Solenoid Controlled toys
'**********************************************************************************************************

Sub SolRelease(Enabled)
	If Enabled Then
	bsTrough.ExitSol_On
	vpmTimer.PulseSw 15
	End If
End Sub
 
Sub Auto_Plunger(Enabled)
	If Enabled Then
	PlungerIM.AutoFire
	End If
End Sub


set GICallback = GetRef("UpdateGI")
Sub UpdateGI(no, Enabled)
	dim xx
	If Enabled Then
'		For each xx in GI:xx.State = 1:	Next

		Lampz.state(111) = 1  			'GI Updates
        Sound_GI_Relay 1,Bumper1
		DOF 101, DOFOn
		If VRBackglassGI = 1 Then
			BGBright.visible = 1
			BGLight.visible = 1
		End If

	Else 
'		For each xx in GI:xx.State = 0: Next

		Lampz.state(111) = 0  			'GI Updates
        Sound_GI_Relay 0,Bumper1
		DOF 101, DOFOff
		If VRBackglassGI = 1 Then
			BGBright.visible = 0
			BGLight.visible = 0
		End If
	End If
End Sub

 'DROPBANK HANDLE
 Sub SolDropBankTrips(Enabled)
	If Enabled Then
		dtDrop.Hit 1
		dtDrop.Hit 2
		dtDrop.Hit 3
		SW17.blenddisablelighting = -0.02
		SW18.blenddisablelighting = -0.02
		SW19.blenddisablelighting = -0.02
	End If
 End Sub

 'TV LOCK HANDLE
 Sub SolTVRelease(Enabled):
	If Enabled Then
		TopPost.IsDropped = 1:
		playsound SoundFX("Saucer_Empty",DOFContactors)
	Else
		TopPost.IsDropped = 0
	End If
 End Sub


'GARAGE DOOR
Dim DoorStatus

Sub GarageUp(Enabled)
	If Enabled Then
		DoorStatus = 1
		GDoorT.enabled = 1
		SoundSaucerKick 0,GDoor
	Else
		DoorStatus = 0
		GDoorT.enabled = 1
	End If
End Sub


' -0.07 - 0.02 ==> 0.09
' 15 steps ==> 0.006
' 

Sub GdoorT_Timer
	If DoorStatus = 1 Then
		If Gdoor.RotX < 60 Then
			Gdoor.RotX = Gdoor.RotX +4
			GDoor.blenddisablelighting = GDoor.blenddisablelighting - 0.006
		Else
			GDoor.blenddisablelighting = 0.015 * gilvl - 0.085 * (1) + (1-gilvl) * 0.02
			sw48.isdropped = 1
			GDoorT.Enabled = 0
		End If
	End If
	If DoorStatus = 0 Then
		If Gdoor.RotX > 0 Then
			Gdoor.RotX = Gdoor.RotX -4
			GDoor.blenddisablelighting = GDoor.blenddisablelighting + 0.006
		Else
			GDoor.blenddisablelighting = 0.015 * gilvl - 0.085 * (0) + (1-gilvl) * 0.02
			sw48.isdropped = 0
			GDoorT.Enabled = 0
		End If
	End If
'	debug.print GDoor.blenddisablelighting
End Sub
	
  'COUCH LOCK	
Sub CouchExit(enabled)
	If Enabled Then
		CouchDrop.Enabled = 1
	Else
		couchdrop.Enabled = 0
		DropCheck.Enabled = 1
	End If
End Sub

Sub DropCheck_Timer
	Drop1.isdropped = 1
	DropCheck.enabled = 0
End Sub

Sub CouchDrop_Hit:Controller.Switch(38) = 0:End Sub
Sub CouchDrop_UnHit:SoundSaucerKick 1 : couchdrop:End Sub

'**********************************************************************************************************
'Initiate Table
'**********************************************************************************************************

Dim bsTrough, bsBR, bsTR, bsVuk, dtDrop, capBall
Set LampCallback = GetRef("UpdateLeds") 'Color TV

Sub Table1_Init
	With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game " & cGameName & vbNewLine & Err.Description:Exit Sub
		.SplashInfoLine = "The Simpsons Pinball Party (Stern 2003)"&chr(13)&"VPW"
		.HandleKeyboard = 0
		.ShowTitle = 0
		.ShowDMDOnly = 1
		.ShowFrame = 0
		.Hidden = 0
		On Error Resume Next
		.Run GetPlayerHWnd
		If Err Then MsgBox Err.Description
		On Error Goto 0
	End With

	PinMAMETimer.Interval = PinMAMEInterval
	PinMAMETimer.Enabled = 1

	If intro=1 then Playsound"ragtime"

    vpmNudge.TiltSwitch=swTilt
    vpmNudge.Sensitivity=3
    vpmNudge.TiltObj=Array(Bumper1, Bumper2, Bumper3, LeftSlingshot, RightSlingshot)

 Set bsTrough = New cvpmBallStack
	 bsTrough.InitSw 0, 14, 13, 12, 11, 10, 0, 0
	 bsTrough.InitKick BallRelease, 45, 9
	 'bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
	 bsTrough.Balls = 5

 Set bsBR = New cvpmBallStack
	 bsBR.InitSaucer sw20, 20, 232, 30
	 bsBR.KickForceVar = 2.5
	 'bsBR.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)

 Set bsTR = New cvpmBallStack
	 bsTR.InitSaucer sw24, 24, 100, 1
	 bsTR.KickForceVar = 2
	 'bsTR.InitExitSnd SoundFX("Popper",DOFContactors), SoundFX("Solenoid",DOFContactors)

 Set bsVuk = New cvpmBallStack
	 bsVuk.InitSw 0, 55, 0, 0, 0, 0, 0, 0
	 bsVuk.InitKick VukOut, 180, 0
	 bsVuk.InitExitSnd SoundFX("Saucer_Kick",DOFContactors), SoundFX("Saucer_Empty",DOFContactors)

 Set dtDrop = New cvpmDropTarget
	 dtDrop.InitDrop Array(sw17, sw18, sw19), Array(17, 18, 19)
	 dtDrop.InitSnd SoundFX("Drop_Target_Down_1",DOFContactors),SoundFX("Drop_Target_Reset_1",DOFContactors)

 set capBall = CapKicker.CreateBall
	CapKicker.kick 0,0

	Drop1.isdropped = 1
	Drop2.isdropped = 1

	'flasher preloads
	vpmtimer.addtimer 100, "FlashPops true:FlashCouch true:UPFRed true:RightRampRed true:DuffCan true:FlashItchy true:UPFOrange true:FlashScratchy true:FlashCBG true:HomerHead true '"
	vpmtimer.addtimer 500, "FlashPops false:FlashCouch false:UPFRed false:RightRampRed false:DuffCan false:FlashItchy false:UPFOrange false:FlashScratchy false:FlashCBG false:HomerHead false '"

	' Initialize beta PWM
	if UseVPMModSol then InitPWM()	

End sub

Sub Table1_Paused:Controller.Pause = 1:End Sub
Sub Table1_unPaused:Controller.Pause = 0:End Sub
sub Table1_Exit:Controller.Stop:end sub

 
'*****************************************************************
'***********   TV LED Init ***************************************
'*****************************************************************
Dim x
		For each x in LEDR:x.visible = 0:next
		For each x in LEDG:x.visible = 0:next
		For each x in LEDY:x.visible = 0:next

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************
Dim BIPL : BIPL = 0

Sub Table1_KeyDown(ByVal KeyCode)

	If keycode = LeftFlipperKey Then
		FlipperActivate LeftFlipper, LFPress
		FlipperActivate TopLeftFlipper, TLFPress
		If VRRoom >0 and VRroom <3 Then VRFlipperLeft.x = VRFlipperLeft.x +5
	End If

	If keycode = RightFlipperKey Then
		FlipperActivate RightFlipper, RFPress
		FlipperActivate RightFlipper2, RF2Press
		FlipperActivate TopRightFlipper, TRFPress
		If VRRoom >0 and VRroom <3 Then VRFlipperRight.x = VRFlipperRight.x -5		
	End If


	'*********************************
	'Keydown Sounds
	'*********************************
        If keycode = LeftTiltKey Then Nudge 90, 1:SoundNudgeLeft()
        If keycode = RightTiltKey Then Nudge 270, 1:SoundNudgeRight()
        If keycode = CenterTiltKey Then Nudge 0, 1:SoundNudgeCenter()

        If keycode = KeyInsertCoin1 or keycode = keyInsertCoin2 or keycode = keyInsertCoin3 or keycode = keyInsertCoin4 Then
                Select Case Int(rnd*3)
                        Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
                        Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
                        Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25

                End Select
        End If

        If keyCode = PlungerKey Then 
		Plunger.Pullback
        SoundPlungerPull()

		If VRRoom >0 and VRroom <3 then 
		TimerVRPlunger.Enabled = True   'VR Plunger
		TimerVRPlunger2.Enabled = False   'VR Plunger
		End If
End if

        If keycode=StartGameKey then 
		Controller.Switch(54) = 1
		SoundStartButton()
		If Vrroom >0 and Vrroom < 3 then
        VRStartButton.y = VRStartButton.y - 5
		VRStartButton2.y = VRStartButton2.y - 5
		End if
		End If


	If Keycode = AddCreditKey Then 
	Controller.Switch(6) = 1
	End If	

	If KeyDownHandler(KeyCode) Then Exit Sub

End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	
	If keycode = LeftFlipperKey Then
		FlipperDeActivate LeftFlipper, LFPress
		FlipperDeActivate TopLeftFlipper, TLFPress
		If VRRoom >0 and VRroom <3 Then VRFlipperLeft.x = VRFlipperLeft.x -5
	End If

	If keycode = RightFlipperKey Then
		FlipperDeActivate RightFlipper, RFPress
		FlipperDeActivate RightFlipper2, RF2Press
		FlipperDeActivate TopRightFlipper, TRFPress

		If VRRoom >0 and VRroom <3 Then
		VRFlipperRight.x = VRFlipperRight.x +5
		End if

	End If


	If keycode = StartGameKey Then 
		Controller.Switch(54) = 0
		If Vrroom >0 and Vrroom < 3 then
        VRStartButton.y = VRStartButton.y + 5
		VRStartButton2.y = VRStartButton2.y + 5
		End if

		Exit Sub
	End If
	If Keycode = AddCreditKey Then 
		Controller.Switch(6) = 0
		Exit Sub
	End if



	'*********************************
	'KeyUp Sounds
	'*********************************
        If KeyCode = PlungerKey Then
                Plunger.Fire

						If VRRoom >0 And VRroom <3  then
						TimerVRPlunger.Enabled = False  'VR Plunger
						TimerVRPlunger2.Enabled = True   ' VR Plunger
						VRPlungerTest.Y = 2310.241   ' VR Plunger
						End if

                If BIPL = 1 Then
                        SoundPlungerReleaseBall()                        'Plunger release sound when there is a ball in shooter lane
                Else
                        SoundPlungerReleaseNoBall()                        'Plunger release sound when there is no ball in shooter lane
                End If
        End If

	If KeyUpHandler(KeyCode) Then Exit Sub
End Sub

 
' IMPULSE PLUNGER
Dim plungerIM
 Const IMPowerSetting = 75 ' Plunger Power
 Const IMTime = 0.7        ' Time in seconds for Full Plunge
Set plungerIM = New cvpmImpulseP
	plungerIM.InitImpulseP swPlunger, IMPowerSetting, IMTime
	plungerIM.Random 0.3
	plungerIM.InitExitSnd SoundFX("Saucer_Kick",DOFContactors), SoundFX("Saucer_Empty",DOFContactors)
	plungerIM.CreateEvents "plungerIM"

'**********************************************************************************************************

 Sub swPlunger_Hit: BIPL=1 :End Sub
 Sub swPlunger_UnHit: BIPL=0 :End Sub

'DRAIN and Kickers
 Sub Drain_Hit:bsTrough.AddBall Me : RandomSoundDrain drain : End Sub
 Sub BallRelease_UnHit: RandomSoundBallRelease ballrelease : End Sub

 Sub sw20_Hit:bsBR.AddBall 0 : SoundSaucerLock : End Sub
 Sub sw20_UnHit:SoundSaucerKick 1, sw20 : End  Sub
 Sub sw24_Hit:bsTR.AddBall 0 : SoundSaucerLock : End Sub
 Sub sw24_UnHit:SoundSaucerKick 1, sw24 : End Sub
 Sub sw55_Hit:bsVuk.AddBall Me : SoundSaucerLock : End Sub
' Sub sw55_UnHit:SoundSaucerKick 1, sw55 : End Sub	'These don't trigger from the bsVUK
' Sub vukout_Hit:SoundSaucerKick 1, sw55 : End Sub
' Sub vukout_UnHit
'	SoundSaucerKick 1, sw55
'	debug.print "kick"
' End Sub

'Drop Targets
 Sub sw17_Dropped:dtDrop.Hit 1 : SW17.blenddisablelighting = -0.02 :End Sub  
 Sub sw18_Dropped:dtDrop.Hit 2 : SW18.blenddisablelighting = -0.02:End Sub  
 Sub sw19_Dropped:dtDrop.Hit 3 : SW19.blenddisablelighting = -0.02:End Sub

'Wire Triggers
 Sub sw16_Hit:Controller.Switch(16) = 1 : BIPL = 1 : End Sub
 Sub sw16_Unhit:Controller.Switch(16) = 0: BIPL = 0 : End Sub
 Sub sw25_Hit:Controller.Switch(25) = 1 : End Sub
 Sub sw25_Unhit:Controller.Switch(25) = 0: End Sub
 Sub sw26_Hit:Controller.Switch(26) = 1 : End Sub
 Sub sw26_Unhit:Controller.Switch(26) = 0: End Sub
 Sub sw29_Hit:Controller.Switch(29) = 1 : End Sub
 Sub sw29_Unhit:Controller.Switch(29) = 0: End Sub
 Sub sw37_Hit:Controller.Switch(37) = 1 : End Sub
 Sub sw37_Unhit:Controller.Switch(37) = 0::End Sub
 Sub sw44_Hit:Controller.Switch(44) = 1 : End Sub
 Sub sw44_Unhit:Controller.Switch(44) = 0: End Sub
 Sub sw57_Hit:Controller.Switch(57) = 1 : End Sub
 Sub sw57_UnHit:Controller.Switch(57) = 0: End Sub
 Sub sw58_Hit:Controller.Switch(58) = 1 : End Sub
 Sub sw58_UnHit:Controller.Switch(58) = 0: End Sub
 Sub sw61_Hit:Controller.Switch(61) = 1 : End Sub
 Sub sw61_UnHit:Controller.Switch(61) = 0: End Sub
 Sub sw60_Hit:Controller.Switch(60) = 1 : End Sub
 Sub sw60_UnHit:Controller.Switch(60) = 0: End Sub
 Sub sw63_Hit:Controller.Switch(63) = 1 : End Sub
 Sub sw63_Unhit:Controller.Switch(63) = 0: End Sub
 Sub sw64_Hit:Controller.Switch(64) = 1 : End Sub
 Sub sw64_Unhit:Controller.Switch(64) = 0: End Sub
 
'Gate Triggers
 Sub sw36_Hit
	vpmTimer.PulseSw 36
    If activeball.velx < 0 Then
        WireRampOn True 'Play Plastic Ramp Sound
    Else
        WireRampOff
    End If
End Sub
 Sub sw45_Hit:vpmTimer.PulseSw 45:End Sub
 Sub sw46_Hit:vpmTimer.PulseSw 46:End Sub
 Sub sw47_Hit:vpmTimer.PulseSw 47:End Sub

'Garage Door
 Sub sw48_Hit:vpmTimer.PulseSW 48 : RandomSoundMetal : End Sub

'Stand Up Targets
 Sub sw9_Hit:vpmTimer.PulseSw 9:End Sub
 Sub sw31_Hit:vpmTimer.PulseSw 31:End Sub
 Sub sw32_Hit:vpmTimer.PulseSw 32:End Sub
 Sub sw30_Hit:vpmTimer.PulseSw 30:End Sub
 Sub sw33_Hit:vpmTimer.PulseSw 33:End Sub
 Sub sw34_Hit:vpmTimer.PulseSw 34:End Sub
 Sub sw35_Hit:vpmTimer.PulseSw 35:End Sub
 Sub sw38_Hit:Controller.Switch(38) = 1:Drop1.isDropped = 0 : End Sub
 Sub sw38_Unhit:Controller.Switch(38) = 0:End Sub
 Sub sw39_Hit:Controller.Switch(39) = 1:Drop2.isdropped = 0 : End Sub
 Sub sw39_Unhit:Controller.Switch(39) = 0:Drop2.isDropped = 1:End Sub
 Sub sw40_Hit:Controller.Switch(40) = 1:RandomSoundBallBouncePlayfieldSoft activeball:WireRampOff:End Sub 'Couch; drop ball, turn off the Plastic Ramp Sound
 Sub sw40_Unhit:Controller.Switch(40) = 0:End Sub 

 Sub sw41_Hit:vpmTimer.PulseSw 41:End Sub
 Sub sw42_Hit:vpmTimer.PulseSw 42:End Sub
 Sub sw43_Hit:vpmTimer.PulseSw 43:End Sub
 Sub sw52_Hit:vpmTimer.PulseSw 52:End Sub
 
'SPINNERS
 Sub sw21_Spin:vpmTimer.PulseSw 21 : SoundSpinner sw21 : End Sub

'BART
 Sub sw22_Hit:Controller.Switch(22) = 1 : End Sub
 Sub sw22_unHit: Controller.Switch(22) = 0: End Sub
 Sub sw23_Hit:Controller.Switch(23) = 1:End Sub
 Sub sw23_unHit: Controller.Switch(23) = 0: End Sub

'Bumpers
Sub Bumper1_Hit
		vpmTimer.PulseSw 50
		BCap1.transz = -10 : bcap1Lit.transz = -10
		l18a.bulbhaloheight = 40
		Me.TimerEnabled = 1
		RandomSoundBumperTop bumper1
End Sub

Sub Bumper2_Hit
		vpmTimer.PulseSw 51
		BCap2.transz = -10 : bcap2Lit.transz = -10
		l19a.bulbhaloheight = 40
		Me.TimerEnabled = 1
		RandomSoundBumperBottom bumper2
End Sub

Sub Bumper1_Timer : BCap1.transz = 0 : bcap1Lit.transz = 0 : l18a.bulbhaloheight = 50 : Me.TimerEnabled = 0 : End Sub

Sub Bumper2_Timer : BCap2.transz = 0 : bcap2Lit.transz = 0 : l19a.bulbhaloheight = 50 : Me.TimerEnabled = 0 : End Sub

Sub Bumper3_Hit:vpmTimer.PulseSw 49 : RandomSoundBumperMiddle bumper3: End Sub


'*******************************************
'  Ramp Triggers
'*******************************************
Sub trigger1_hit()
    If activeball.vely < 0 Then
        WireRampOn True 'Play Plastic Ramp Sound
    Else
        WireRampOff
    End If
End Sub

Sub trigger2_hit()
	WireRampOff ' Turn off the Plastic Ramp Sound
End Sub

Sub trigger3_hit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound
End Sub

Sub trigger4_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub trigger4_unhit()
	PlaySoundAt "WireRamp_Stop", trigger4
End Sub

Sub trigger12_hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub trigger13_hit()
	WireRampOff 'Turn off the Plastic Ramp Sound
End Sub

Sub trigger6_hit()
	WireRampOn True 'Play Plastic Ramp Sound
End Sub

Sub trigger6_unhit()
	if activeball.velx > 0 then WireRampOff 'Play Plastic Ramp Sound
End Sub

Sub trigger14_hit()
    If activeball.vely > 0 Then
        WireRampOn True 'Play Plastic Ramp Sound
    Else
        WireRampOff
    End If
End Sub

Sub trigger11_hit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound
End Sub

Sub trigger7_hit()
	WireRampOn False ' On Wire Ramp Play Wire Ramp Sound
End Sub

'Sub trigger8_hit()
'	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
'End Sub


'Sub trigger9_hit()
	'WireRampOn False ' On Wire Ramp Play Wire Ramp Sound
'End Sub

Sub trigger10_hit()
	WireRampOff ' Exiting Wire Ramp Stop Playing Sound
End Sub

Sub trigger10_unhit()
	PlaySoundAt "WireRamp_Stop", trigger10
End Sub

'************************************************************
'******************** HOMERs HEAD ***************************
'************************************************************
  Dim HeadPos, HeadDir,HeadState, HeadRef, Homeractive
	HeadPos = 0:HeadDir = 5:HeadState = 0:HeadRef = 0

'  Sub CheckLamps()
'	Dim RR, RO, LO
'	'HeadRef determines which image is displayed
''	If F22.State = 1 then: RO = 1:else: RO = 0:end if
''	If F23.State = 1 then: RR = 1:else: RR = 0:end if
''	If F32.State = 1 then: LO = 1:else: LO = 0:end if
'	If Objlevel(4) > 0.8 then: RO = 1:else: RO = 0:end if
'	If Lampz.State(101) = 1 then: RR = 1:else: RR = 0:end if
'	If Objlevel(3) > 0.8 then: LO = 1:else: LO = 0:end if
'	If RR = 1 then HeadRef = 1
'	If RO = 1 then HeadRef = 2
'	If LO = 1 then HeadRef = 3
'	If RO = 1 and LO = 1 then HeadRef = 4
'	If RO = 1 and RR = 1 then HeadRef = 5
'	If LO = 1 and RR = 1 then HeadRef = 5
'	If RO = 1 and RR = 1 and LO = 1 then HeadRef = 5
'	If RO = 0 and RR = 0 and LO = 0 then HeadRef = 0
'  End Sub

 Sub SolHomer(Enabled)
	 If Enabled Then
		HomerActive = 1:HeadDir = 2:HeadTimer.Enabled = 1
	 Else
		HomerActive = 0:HeadDir = -2:HeadTimer.Enabled = 1
	 End If
 End Sub

 Sub HeadTimer_Timer()

	If HeadPos > 41 or HeadPos < 4 Then
		HeadPos = HeadPos + HeadDir/5
	Else
		HeadPos = HeadPos + HeadDir
	End If

	If HeadPos > 45 then 
		HeadPos = 45:HeadTimer.Enabled = 0
	end if
	If HeadPos < 0 then
		HeadPos = 0:HeadTimer.Enabled = 0
	end if
	UpdateHead
  End Sub

Sub UpdateHead()
	HHead.Roty = HeadPos
End Sub

Sub UpdateHead3()
	CheckLamps
	Select case HeadRef
		case 0 : 'no flasher reflections
			HHead.Roty = HeadPos':HHead.Image = "hh"
		case 1 : 'RRFlasher
			HHead.Roty = HeadPos':HHead.Image = "hhRF"
		case 2 : 'RO Flasher
			HHead.Roty = HeadPos':HHead.Image = "hhROF"
		case 3 : 'LO flasher
			HHead.Roty = HeadPos':HHead.Image = "hhLOF"
		case 4 : 'Both Orange
			HHead.Roty = HeadPos':HHead.Image = "hhOOF"
		case 5 : 'Orange and Red
			HHead.Roty = HeadPos':HHead.Image = "hhORF"
	end Select
End Sub


Sub UpdateHead2()
	CheckLamps
	Select case HeadRef
		case 0 : 'no flasher reflections
				Select case HeadState
					case 0 : HHead.Roty = HeadPos:HHead.Image = "hh"
					case 1 : HHead.Roty = HeadPos:HHead.Image = "hhFb"
					case 2 : HHead.Roty = HeadPos:HHead.Image = "hhFa"
					case 3 : HHead.Roty = HeadPos:HHead.Image = "hhFOn"
				end Select
		case 1 : 'RRFlasher
				Select case HeadState
					case 0 : HHead.Roty = HeadPos:HHead.Image = "hhRF"
					case 1 : HHead.Roty = HeadPos:HHead.Image = "hhFb"
					case 2 : HHead.Roty = HeadPos:HHead.Image = "hhFa"
					case 3 : HHead.Roty = HeadPos:HHead.Image = "hhFOn"
				end Select
		case 2 : 'RO Flasher
				Select case HeadState
					case 0 : HHead.Roty = HeadPos:HHead.Image = "hhROF"
					case 1 : HHead.Roty = HeadPos:HHead.Image = "hhFb"
					case 2 : HHead.Roty = HeadPos:HHead.Image = "hhFa"
					case 3 : HHead.Roty = HeadPos:HHead.Image = "hhFOn"
				end Select
		case 3 : 'LO flasher
				Select case HeadState
					case 0 : HHead.Roty = HeadPos:HHead.Image = "hhLOF"
					case 1 : HHead.Roty = HeadPos:HHead.Image = "hhFb"
					case 2 : HHead.Roty = HeadPos:HHead.Image = "hhFa"
					case 3 : HHead.Roty = HeadPos:HHead.Image = "hhFOn"
				end Select
		case 4 : 'Both Orange
				Select case HeadState
					case 0 : HHead.Roty = HeadPos:HHead.Image = "hhOOF"
					case 1 : HHead.Roty = HeadPos:HHead.Image = "hhFb"
					case 2 : HHead.Roty = HeadPos:HHead.Image = "hhFa"
					case 3 : HHead.Roty = HeadPos:HHead.Image = "hhFOn"
				end Select
		case 5 : 'Orange and Red
				Select case HeadState
					case 0 : HHead.Roty = HeadPos:HHead.Image = "hhORF"
					case 1 : HHead.Roty = HeadPos:HHead.Image = "hhFb"
					case 2 : HHead.Roty = HeadPos:HHead.Image = "hhFa"
					case 3 : HHead.Roty = HeadPos:HHead.Image = "hhFOn"
				end Select
	end Select
End Sub
 
Sub Homer_Hit
	If HomerActive Then
		If ActiveBall.VelX <0 Then
			HomerOff.Enabled = 0:HomerOn.Enabled = 1
		Else
			HomerOn.Enabled = 0:HomerOff.Enabled = 1
		End If
	End If
End Sub
 
Sub Homer3_Hit
 If HomerActive Then
 HomerOn.Enabled = 0:HomerOff.Enabled = 1
 End If
End Sub
 
Sub Homer2_Hit
	If HomerActive Then
		HomerOff.Enabled = 0:HomerOn.Enabled = 1
	End If
End Sub
 
 Sub HomerOn_Timer
' Select Case HomerState
' Case 0:HomerState = 1:HomeRm 127, homerhead, HomerState
' Case 1:HomerState = 2:HomeRm 127, homerhead, HomerState
' Case 2:HomerState = 3:HomeRm 127, homerhead, HomerState:Me.Enabled = 0
' End Select
 End Sub
 
 Sub HomerOff_Timer
' Select Case HomerState
' Case 3:HomerState = 2:HomeRm 127, homerhead, HomerState
' Case 2:HomerState = 1:HomeRm 127, homerhead, HomerState
' Case 1:HomerState = 0:HomeRm 127, homerhead, HomerState:Me.Enabled = 0
' End Select
 End Sub
 
 Sub homer4_Hit
' HomerReset:HomerHead(0).Visible = 1
 End sub




'******************************************************
'*****   FLUPPER DOMES 
'******************************************************
' Based on FlupperDoms2.2

' What you need in your table to use these flashers:
' Open this table and your table both in VPX
' Export all the materials domebasemat, Flashermaterial0 - 20 and import them in your table
' Export all textures (images) starting with the name "dome" and "ronddome" and import them into your table with the same names
' Export all textures (images) starting with the name "flasherbloom" and import them into your table with the same names
' Copy a set of 4 objects flasherbase, flasherlit, flasherlight and flasherflash from layer 7 to your table
' If you duplicate the four objects for a new flasher dome, be sure that they all end with the same number (in the 0-20 range)
' Copy the flasherbloom flashers from layer 10 to your table. you will need to make one per flasher dome that you plan to make
' Select the correct flasherbloom texture for each flasherbloom flasher, per flasher dome
' Copy the script below 

' Place your flasher base primitive where you want the flasher located on your Table
' Then run InitFlasher in the script with the number of your flasher objects and the color of the flasher.  This will align the flasher object, light object, and 
' flasher lit primitive.  It will also assign the appropriate flasher bloom images to the flasher bloom object.
'
' Example: InitFlasher 1, "green"
'
' Color Options: "blue", "green", "red", "purple", "yellow", "white", and "orange"

' You can use the RotateFlasher call to align the Rotz/ObjRotz of the flasher primitives with "handles".  Don't set those values in the editor,
' call the RotateFlasher sub instead (this call will likely crash VP if it's call for the flasher primitives without "handles")
'
' Example: RotateFlasher 1, 180 		'where 1 is the flasher number and 180 is the angle of Z rotation

' For flashing the flasher use in the script: "ObjLevel(1) = 1 : FlasherFlash1_Timer"
' This should also work for flashers with variable flash levels from the rom, just use ObjLevel(1) = xx from the rom (in the range 0-1)
'
' Notes (please read!!):
' - Setting TestFlashers = 1 (below in the ScriptsDirectory) will allow you to see how the flasher objects are aligned (need the targetflasher image imported to your table)
' - The rotation of the primitives with "handles" is done with a script command, not on the primitive itself (see RotateFlasher below)
' - Color of the objects are set in the script, not on the primitive itself
' - Screws are optional to copy and position manually
' - If your table is not named "Table1" then change the name below in the script
' - Every flasher uses its own material (Flashermaterialxx), do not use it for anything else
' - Lighting > Bloom Strength affects how the flashers look, do not set it too high
' - Change RotY and RotX of flasherbase only when having a flasher something other then parallel to the playfield
' - Leave RotX of the flasherflash object to -45; this makes sure that the flash effect is visible in FS and DT
' - If you want to resize a flasher, be sure to resize flasherbase, flasherlit and flasherflash with the same percentage
' - If you think that the flasher effects are too bright, change flasherlightintensity and/or flasherflareintensity below

' Some more notes for users of the v1 flashers and/or JP's fading lights routines:
' - Delete all textures/primitives/script/materials in your table from the v1 flashers and scripts before you start; they don't mix well with v2
' - Remove flupperflash(m) routines if you have them; they do not work with this new script
' - Do not try to mix this v2 script with the JP fading light routine (that is making it too complicated), just use the example script below

' example script for rom based tables (non modulated):

if not UseVPMModSol then 
	SolCallBack(21) = "FlashPops" 					'Flash Pops Clear/Green
	SolCallBack(22) = "RightRampRed"   				'R.Ramp Red
	SolCallBack(23) = "DuffCan"						'R.Ramp Orange / Duff Can

	SolCallBack(25) = "FlashItchy" 					'Flash Itchy
	SolCallBack(26) = "FlashScratchy"  				'Flash Scratchy
	SolCallBack(27) = "HomerHead" 					'Homer Head
	SolCallBack(28) = "FlashCouch" 					'Flash Couch
	SolCallBack(29) = "FlashCBG" 					'Flash Comic Book Guy

	SolCallBack(31) = "UPFOrange" 					'UPF Orange
	SolCallBack(32) = "UPFRed" 						'UPF Red
else 
	SolModCallback(21) = "ModLampz.SetModLamp 21," 	'Flash Pops Clear/Green
	SolModCallback(22) = "ModLampz.SetModLamp 22,"  'R.Ramp Red
	SolModCallback(23) = "ModLampz.SetModLamp 23,"	'R.Ramp Orange / Duff Can

	SolModCallback(25) = "ModLampz.SetModLamp 25," 	'Flash Itchy
	SolModCallback(26) = "ModLampz.SetModLamp 26,"  'Flash Scratchy
	SolModCallback(27) = "ModLampz.SetModLamp 27," 	'Homer Head
	SolModCallback(28) = "ModLampz.SetModLamp 28," 	'Flash Couch
	SolModCallback(29) = "ModLampz.SetModLamp 29," 	'Flash Comic Book Guy

	SolModCallback(31) = "ModLampz.SetModLamp 31," 	'UPF Orange
	SolModCallback(32) = "ModLampz.SetModLamp 32," 	'UPF Red
end if


'normal callbacks
 Sub FlashPops(flstate)
'	debug.print "pops flstate " & flstate
	If Flstate Then
		ObjTargetLevel(1) = 1
	Else
		ObjTargetLevel(1) = 0
	End If
	FlasherFlash1_Timer
 End Sub

 Sub FlashCouch(flstate)
'	debug.print "couch flstate " & flstate
	If Flstate Then
		ObjTargetLevel(2) = 1
	Else
		ObjTargetLevel(2) = 0
	End If
	FlasherFlash2_Timer
 End Sub

 Sub UPFRed(flstate)
'	debug.print "upfr flstate " & flstate
	If Flstate Then
		ObjTargetLevel(3) = 1
	Else
		ObjTargetLevel(3) = 0
	End If
	FlasherFlash3_Timer
 End Sub

 Sub RightRampRed(flstate)
'	debug.print "rr flstate " & flstate
	If Flstate Then
		ObjTargetLevel(4) = 1
	Else
		ObjTargetLevel(4) = 0
	End If
	FlasherFlash4_Timer
 End Sub

 Sub DuffCan(flstate)
'	debug.print "duff flstate " & flstate
	If Flstate Then
		ObjTargetLevel(5) = 1
'		Lampz.State(101)=1
	Else
		ObjTargetLevel(5) = 0
'		Lampz.State(101)=0
	End If
	FlasherFlash5_Timer
 End Sub

 Sub FlashItchy(flstate)
'	debug.print "itch flstate " & flstate
	If Flstate Then
		ObjTargetLevel(6) = 1
	else
		ObjTargetLevel(6) = 0
	End If
	FlasherFlash6_Timer
 End Sub

 Sub UPFOrange(flstate)
'	debug.print "upfo flstate " & flstate
	If Flstate Then
		ObjTargetLevel(7) = 1
	Else
		ObjTargetLevel(7) = 0
	End If
	FlasherFlash7_Timer
 End Sub

Sub FlashScratchy(flstate)
'	debug.print "s flstate " & flstate
	If Flstate Then
		ObjTargetLevel(8) = 1
	Else
		ObjTargetLevel(8) = 0
	End If
	FlasherFlash8_Timer
 End Sub


Sub FlashCBG(flstate)
'	debug.print "cbg flstate " & flstate
	If Flstate Then
        ObjTargetLevel(9) = 1
'		Lampz.State(102)=1
	Else
        ObjTargetLevel(9) = 0
'		Lampz.State(102)=0
	End If
	FlasherFlash9_Timer
 End Sub

Sub HomerHead(flstate)
'	debug.print "hh flstate " & flstate
	If Flstate Then
        ObjTargetLevel(10) = 1
'		Lampz.State(100)=1
	Else
        ObjTargetLevel(10) = 0
'		Lampz.State(100)=0
	End If
	FlasherFlash10_Timer
 End Sub



'PWM modulated callbacks
sub ModFlash21(aValue) 
	UpdateCaps 1, aValue
end Sub

sub ModFlash22(aValue) 
	UpdateCaps 4, aValue
end Sub

sub ModFlash23(aValue) 
	UpdateCaps 5, aValue
end Sub

sub ModFlash25(aValue) 
	UpdateCaps 6, aValue
end Sub

sub ModFlash26(aValue) 
	UpdateCaps 8, aValue
end Sub

sub ModFlash27(aValue) 
	UpdateCaps 10, aValue
end Sub

sub ModFlash28(aValue) 
	UpdateCaps 2, aValue
end Sub

sub ModFlash29(aValue) 
	UpdateCaps 9, aValue
end Sub

sub ModFlash31(aValue) 
	UpdateCaps 7, aValue
end Sub

sub ModFlash32(aValue) 
	UpdateCaps 3, aValue
end Sub



Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness, FlasherPFIntensity, FlasherWallIntensity

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 0.1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 0.03	' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherBloomIntensity = 0.02	' *** lower this, if the blooms are too bright (i.e. 0.1)			***	
FlasherOffBrightness = 0.4		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************
FlasherPFIntensity = 3
FlasherWallIntensity = 0.3

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20), ObjTargetLevel(20)
'Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"

InitFlasher 1, "White" 		'FlashPops
InitFlasher 2, "Yellow" 	'FlashCouch
InitFlasher 3, "Red"		'UPF Red Back Wall
InitFlasher 4, "Red" 		'Right Ramp

if DuffCanToy = 0 then
	DuffBeer.visible = false
	Flasherbase5.size_x = 22
	Flasherbase5.size_y = 22
	Flasherbase5.size_z = 22
	Flasherlit5.size_x = 22
	Flasherlit5.size_y = 22
	Flasherlit5.size_z = 22
	Flasherbase5.z = 120
	Flasherflash5.height = 160
	Flasher5a.color = RGB(255,100,0)
	FlasherPF5.color = RGB(255,100,0)
	InitFlasher 5, "orange"		'Orig Orange Dome
else
	DuffBeer.visible = true
	Flasherbase5.size_x = 1
	Flasherbase5.size_y = 1
	Flasherbase5.size_z = 1
	Flasherlit5.size_x = 1
	Flasherlit5.size_y = 1
	Flasherlit5.size_z = 1
	Flasherbase5.z = -10
	Flasherflash5.height = -10
	Flasher5a.color = RGB(141,139,141)
	FlasherPF5.color = RGB(255,255,255)
	InitFlasher 5, "White"		'DuffCan
end if

if Garagedecal = 0 then
	Primitive_shootdecal_mod.visible = false
	Primitive_shootdecal_original.visible = true

else
	Primitive_shootdecal_original.visible = false
	Primitive_shootdecal_mod.visible = true

end if

if moetoy = 0 then
	moe.visible = false

else
	moe.visible = true

end if

if plungerlight = 0 then
	l31.visible = false

else
	l31.visible = true

end if

InitFlasher 6, "White"		'FlashItchy
InitFlasher 7, "Orange"		'UPF Orange
InitFlasher 8, "yellow"		'FlashScratchy
InitFlasher 9, "Red"		'FlashCBG
InitFlasher 10, "White"		'FlashCBG


' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 1,17 : RotateFlasher 2,0 : RotateFlasher 3,90 : RotateFlasher 4,90 

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	Set objbloom(nr) = Eval("Flasherbloom" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 40
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness

	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2baseWhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objbloom(nr).color = RGB(4,120,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4) : objbloom(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4) : objbloom(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) : objbloom(nr).color = RGB(230,49,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50) : objbloom(nr).color = RGB(200,173,25)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59) : objbloom(nr).color = RGB(255,240,150)
		Case "orange" : objlight(nr).color = RGB(255,70,0) : objflasher(nr).color = RGB(255,70,0) : objbloom(nr).color = RGB(255,70,0)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

sub LimitFlasherLevels(nr)
	dim FlasherLevelSum

	FlasherLevelSum = ObjLevel(1) + ObjLevel(2) + ObjLevel(3) + ObjLevel(4) + ObjLevel(5) + ObjLevel(6) + ObjLevel(7) + ObjLevel(8) + ObjLevel(9) + ObjLevel(10)
	if FlasherLevelSum > 0 then 
		if nr <> 1 Or nr <> 7 then 'flasher #1 different rule
			if FlasherLevelSum >= 3 and FlasherLevelSum <= 5  Then
				ObjLevel(nr) = ObjLevel(nr) * (3 / FlasherLevelSum)
			Elseif FlasherLevelSum >= 5 Then
				ObjLevel(nr) = ObjLevel(nr) * (4 / FlasherLevelSum)
			end if
		Else
			if FlasherLevelSum >= 5 Then
				ObjLevel(nr) = ObjLevel(nr) * (5 / FlasherLevelSum)
			end if
		end if
	end if
end sub


Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objbloom(nr).visible = 1 : objlit(nr).visible = 1 : End If

	LimitFlasherLevels nr

	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objbloom(nr).opacity = 100 *  FlasherBloomIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 3 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 

	'Other objects
	Select Case nr
		Case 1
			Flasher_BS_1.visible = FlasherFlash1.visible
			Flasher_BS_1.opacity = 10 *  FlasherWallIntensity * ObjLevel(nr)^2.5
			Flasher1.visible = FlasherFlash1.visible
			Flasher1.opacity = 60 *  FlasherWallIntensity * ObjLevel(nr)^2.5
			Flasher1light.visible = FlasherFlash1.visible
			Flasher1light.opacity =  10 *  FlasherWallIntensity * ObjLevel(nr)^2.5
			FlasherPF1.opacity = 1000 *  FlasherPFIntensity * ObjLevel(nr)^2.5
			FlasherPF1.visible = FlasherFlash1.visible
		Case 2
			Flasher_BS_2.visible = FlasherFlash2.visible
			Flasher_BS_2.opacity = 10 *  FlasherWallIntensity * ObjLevel(nr)^2.5
			Flasher2light.visible = FlasherFlash2.visible
			Flasher2light.opacity = 10 *  FlasherWallIntensity * ObjLevel(nr)^2.5
			FlasherPF2.opacity = 100 *  FlasherPFIntensity * ObjLevel(nr)^2.5
			FlasherPF2.visible = FlasherFlash2.visible
		Case 3
			Flasher_BS_3.visible = FlasherFlash3.visible
			Flasher_BS_3.opacity = 3 *  FlasherWallIntensity * ObjLevel(nr)^2.5
			Flasher3light.visible = FlasherFlash3.visible
			Flasher3light.opacity = 10 *  FlasherWallIntensity * ObjLevel(nr)^2.5
			FlasherPF3.opacity = 1000 *  FlasherPFIntensity * ObjLevel(nr)^2.5
			FlasherPF3.visible = FlasherFlash3.visible
		Case 4
			Flasher_BS_4.visible = FlasherFlash4.visible
			Flasher_BS_4.opacity = 10 *  FlasherWallIntensity * ObjLevel(nr)^2.5
'			Flasher4.visible = FlasherFlash4.visible
'			Flasher4.opacity = 10 *  FlasherWallIntensity * ObjLevel(nr)^2.5
			Flasher4a.visible = FlasherFlash4.visible
			Flasher4a.opacity = 360 *  FlasherWallIntensity * ObjLevel(nr)^2.5
			FlasherPF4.opacity = 1000 *  FlasherPFIntensity * ObjLevel(nr)^2.5
			FlasherPF4.visible = FlasherFlash4.visible
		Case 5
			Flasher_BS_5.visible = FlasherFlash5.visible
			Flasher_BS_5.opacity = 10 *  FlasherWallIntensity * ObjLevel(nr)^2.5
			Flasher5light.visible = FlasherFlash5.visible
			Flasher5light.opacity = 10 *  FlasherWallIntensity * ObjLevel(nr)^2.5
			FlasherPF5.opacity = 1000 *  FlasherPFIntensity * ObjLevel(nr)^2.5
			FlasherPF5.visible = FlasherFlash5.visible
			Flasher5a.visible = FlasherFlash5.visible
			Flasher5a.opacity = 800 *  FlasherWallIntensity * ObjLevel(nr)^2.5
			DuffBeer.blenddisablelighting = Flasherlit5.blenddisablelighting / 1.1 + (0.045 * gilvl - 0.01)
		Case 6
			Flasher6.visible = FlasherFlash6.visible
			Flasher6.opacity = 2 *  FlasherWallIntensity * ObjLevel(nr)^2.5
			FlasherPF6.opacity = 1000 *  FlasherPFIntensity * ObjLevel(nr)^2.5
			FlasherPF6.visible = FlasherFlash6.visible
		Case 7
			Flasher_BS_7.visible = FlasherFlash7.visible
			Flasher_BS_7.opacity = 10 *  FlasherWallIntensity * ObjLevel(nr)^2.5
			Flasher7light.visible = FlasherFlash7.visible
			Flasher7light.opacity = 10 *  FlasherWallIntensity * ObjLevel(nr)^2.5
			FlasherPF7.opacity = 1000 *  FlasherPFIntensity * ObjLevel(nr)^2.5
			FlasherPF7.visible = FlasherFlash7.visible
		Case 8
			Flasher8.visible = FlasherFlash8.visible
			Flasher8.opacity = 10 *  FlasherWallIntensity * ObjLevel(nr)^2.5
			FlasherPF8.opacity = 1000 *  FlasherPFIntensity * ObjLevel(nr)^2.5
			FlasherPF8.visible = FlasherFlash8.visible
		Case 9
			FlasherPF9.opacity = 1000 *  FlasherPFIntensity * ObjLevel(nr)^2.5
			FlasherPF9.visible = FlasherFlash9.visible
			'ComicBookGuy.blenddisablelighting = Flasherlit9.blenddisablelighting / 4 + 0.25
		Case 10
			HHead.blenddisablelighting = Flasherlit10.blenddisablelighting / 10 '+ (0.045 * gilvl - 0.01)	'1.1
	End Select


	'ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	if ObjTargetLevel(nr) = 1 and ObjLevel(nr) < ObjTargetLevel(nr) Then			'solenoid ON happened
		ObjLevel(nr) = (ObjLevel(nr) + 0.35) * RndNum(1.05, 1.15)					'fadeup speed. ~4-5 frames * 30ms
		if ObjLevel(nr) > 1 then ObjLevel(nr) = 1
	Elseif ObjTargetLevel(nr) = 0 and ObjLevel(nr) > ObjTargetLevel(nr) Then		'solenoid OFF happened
		ObjLevel(nr) = ObjLevel(nr) * 0.75 - 0.01									'fadedown speed. ~16 frames * 30ms
		if ObjLevel(nr) < 0.02 then ObjLevel(nr) = 0								'slight perf optimization to cut the very tail of the fade
	Else																			'do nothing here
		ObjLevel(nr) = ObjTargetLevel(nr)
'		debug.print objTargetLevel(nr) &" = " & ObjLevel(nr)
		objflasher(nr).TimerEnabled = False											'timer can be stopped as desired level was reached.
	end if

	If ObjLevel(nr) <= 0 Then 
		objflasher(nr).TimerEnabled = False
		objflasher(nr).visible = 0
		objbloom(nr).visible = 0
		objlit(nr).visible = 0
	End If
'	debug.print "timer check" & nr													'this should not be bloating the debug log. If it keeps printing out, there is some flaw in the code.
End Sub


Sub UpdateCaps(nr, aValue) ' nf UseVPMModSol dynamic fading version of 'FlashFlasher' (PWM Update). You can see most of the ramp up has been commented out in favor of Pinmame's curves
	if aValue > 0 then 
		objflasher(nr).visible = 1 : objbloom(nr).visible = 1 : objlit(nr).visible = 1
	else 
		objflasher(nr).visible = 1 : objbloom(nr).visible = 1 : objlit(nr).visible = 1
	end if
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * aValue' * ObjLevel(nr)^2.5
	objbloom(nr).opacity = 100 *  FlasherBloomIntensity * aValue' * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * aValue' * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * aValue '* ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * aValue '* ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr, 0,0,0,0,0,0,Round(aValue,1),RGB(255,255,255),0,0,False,True,0,0,0,0 

	'Other objects
	Select Case nr
		Case 1
			Flasher_BS_1.visible = FlasherFlash1.visible
			Flasher_BS_1.opacity = 10 *  FlasherWallIntensity * aValue' ObjLevel(nr)^2.5
			Flasher1.visible = FlasherFlash1.visible
			Flasher1.opacity = 60 *  FlasherWallIntensity * aValue' ObjLevel(nr)^2.5
			Flasher1light.visible = FlasherFlash1.visible
			Flasher1light.opacity =  10 *  FlasherWallIntensity * aValue' ObjLevel(nr)^2.5
			FlasherPF1.opacity = 1000 *  FlasherPFIntensity * aValue' ObjLevel(nr)^2.5
			FlasherPF1.visible = FlasherFlash1.visible
		Case 2
			Flasher_BS_2.visible = FlasherFlash2.visible
			Flasher_BS_2.opacity = 10 *  FlasherWallIntensity * aValue' ObjLevel(nr)^2.5
			Flasher2light.visible = FlasherFlash2.visible
			Flasher2light.opacity = 10 *  FlasherWallIntensity * aValue' ObjLevel(nr)^2.5
			FlasherPF2.opacity = 100 *  FlasherPFIntensity * aValue' ObjLevel(nr)^2.5
			FlasherPF2.visible = FlasherFlash2.visible
		Case 3
			Flasher_BS_3.visible = FlasherFlash3.visible
			Flasher_BS_3.opacity = 3 *  FlasherWallIntensity * aValue' ObjLevel(nr)^2.5
			Flasher3light.visible = FlasherFlash3.visible
			Flasher3light.opacity = 10 *  FlasherWallIntensity * aValue' ObjLevel(nr)^2.5
			FlasherPF3.opacity = 1000 *  FlasherPFIntensity * aValue' ObjLevel(nr)^2.5
			FlasherPF3.visible = FlasherFlash3.visible
		Case 4
			Flasher_BS_4.visible = FlasherFlash4.visible
			Flasher_BS_4.opacity = 10 *  FlasherWallIntensity * aValue' ObjLevel(nr)^2.5
'			Flasher4.visible = FlasherFlash4.visible
'			Flasher4.opacity = 10 *  FlasherWallIntensity * ObjLevel(nr)^2.5
			Flasher4a.visible = FlasherFlash4.visible
			Flasher4a.opacity = 360 *  FlasherWallIntensity * aValue' ObjLevel(nr)^2.5
			FlasherPF4.opacity = 1000 *  FlasherPFIntensity * aValue' ObjLevel(nr)^2.5
			FlasherPF4.visible = FlasherFlash4.visible
		Case 5
			Flasher_BS_5.visible = FlasherFlash5.visible
			Flasher_BS_5.opacity = 10 *  FlasherWallIntensity * aValue' ObjLevel(nr)^2.5
			Flasher5light.visible = FlasherFlash5.visible
			Flasher5light.opacity = 10 *  FlasherWallIntensity * aValue' ObjLevel(nr)^2.5
			FlasherPF5.opacity = 1000 *  FlasherPFIntensity * aValue' ObjLevel(nr)^2.5
			FlasherPF5.visible = FlasherFlash5.visible
			Flasher5a.visible = FlasherFlash5.visible
			Flasher5a.opacity = 800 *  FlasherWallIntensity * aValue' ObjLevel(nr)^2.5
			DuffBeer.blenddisablelighting = Flasherlit5.blenddisablelighting / 1.1 + (0.045 * gilvl - 0.01)
		Case 6
			Flasher6.visible = FlasherFlash6.visible
			Flasher6.opacity = 2 *  FlasherWallIntensity * aValue' ObjLevel(nr)^2.5
			FlasherPF6.opacity = 1000 *  FlasherPFIntensity * aValue' ObjLevel(nr)^2.5
			FlasherPF6.visible = FlasherFlash6.visible
		Case 7
			Flasher_BS_7.visible = FlasherFlash7.visible
			Flasher_BS_7.opacity = 10 *  FlasherWallIntensity * aValue' ObjLevel(nr)^2.5
			Flasher7light.visible = FlasherFlash7.visible
			Flasher7light.opacity = 10 *  FlasherWallIntensity * aValue' ObjLevel(nr)^2.5
			FlasherPF7.opacity = 1000 *  FlasherPFIntensity * aValue' ObjLevel(nr)^2.5
			FlasherPF7.visible = FlasherFlash7.visible
		Case 8
			Flasher8.visible = FlasherFlash8.visible
			Flasher8.opacity = 10 *  FlasherWallIntensity * aValue' ObjLevel(nr)^2.5
			FlasherPF8.opacity = 1000 *  FlasherPFIntensity * aValue' ObjLevel(nr)^2.5
			FlasherPF8.visible = FlasherFlash8.visible
		Case 9
			FlasherPF9.opacity = 1000 *  FlasherPFIntensity * aValue' ObjLevel(nr)^2.5
			FlasherPF9.visible = FlasherFlash9.visible
			'ComicBookGuy.blenddisablelighting = Flasherlit9.blenddisablelighting / 4 + 0.25
		Case 10
			HHead.blenddisablelighting = Flasherlit10.blenddisablelighting / 10 '+ (0.015 * gilvl - 0.01)	'1.1
			debug.print HHead.blenddisablelighting
	End Select

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

'******************************************************
'******  END FLUPPER DOMES
'******************************************************




'******************************************************
' GI Updates
'******************************************************

Dim gilvl : gilvl = 0

Sub GIUpdates(ByVal aLvl)
'	debug.print "gi update: " & aLvl
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	dim bulb
	for each bulb in Inserts
		bulb.intensity = bulb.uservalue*(1 + (1-aLvl)*0.5)
	next

	if aLvl = 0 then									'GI OFF, let's hide ON prims
		OnPrimsVisible False
		If gilvl = 1 Then OffPrimsVisible true
	Elseif aLvl = 1 then								'GI ON, let's hide OFF prims
		OffPrimsVisible False
		If gilvl = 0 Then OnPrimsVisible True
	Else
		if gilvl = 0 Then								'GI has just changed from OFF to fading, let's show ON
			OnPrimsVisible True
		elseif gilvl = 1 Then							'GI has just changed from ON to fading, let's show OFF
			OffPrimsVisible true
		Else
			'no change
		end if
	end if

	DisableLighting LFLogo,0.04,aLvl
	DisableLighting RFlogo,0.04,aLvl
	DisableLighting RF2logo,0.04,aLvl

	UpdateMaterial "OpaqueON",0,0,0,0,0,0,aLvl,RGB(255,255,255),0,0,False,True,0,0,0,0

	HHead.blenddisablelighting = 0.045 * aLvl - 0.01
'	HHead.blenddisablelighting = -0.01
'	HHead.blenddisablelighting = 0.035
	
	moe.blenddisablelighting = 0.005 * aLvl
	bart1a.blenddisablelighting = 0.1 * aLvl + 0.3
	board1a.blenddisablelighting = 0.01 * aLvl
	Itchy.blenddisablelighting = 0.03 * aLvl
	Scratchy.blenddisablelighting = 0.03 * aLvl
	DuffBeer.blenddisablelighting = 0.02 * aLvl
	comicbookguy.blenddisablelighting = 0.4 * aLvl + 0.4
	Flasherbase4.blenddisablelighting = 0.7 * aLvl + 0.3
	Flasherbase7.blenddisablelighting = 1 * aLvl +0.3
	Flasherbase2.blenddisablelighting = 0.5 * aLvl + 0.3
	micro.blenddisablelighting = 0.15 * aLvl + 0.12
	Primitive78.blenddisablelighting = 0.035 * aLvl
	Primitive_shootdecal_original.blenddisablelighting = 0.015 * aLvl
	Primitive_shootdecal_MOD.blenddisablelighting = 0.01 * aLvl



	GDoor.blenddisablelighting = 0.015 * aLvl - (0.085 * (Gdoor.RotX / 60)) + (1-gilvl) * 0.02

	Primitive83.blenddisablelighting = 0.005 * aLvl

	SW17.blenddisablelighting = 0.015 * aLvl - 0.015
	SW18.blenddisablelighting = 0.015 * aLvl - 0.015
	SW19.blenddisablelighting = 0.015 * aLvl - 0.015

	bart1aLit.opacity = 200 * alvl

	ComicBookGuyLit.opacity = 150 * alvl

	gilvl = alvl

End Sub

sub OnPrimsVisible(aValue)
	dim kk
	If aValue then
		For each kk in ON_Prims:kk.visible = 1:next
		If SideBladeArt = 0 then
			Sidewalls_gion.visible = 1
			Sidewalls_alt_gion.visible = 0
		Else
			Sidewalls_gion.visible = 0
			Sidewalls_alt_gion.visible = 1
		End If
	Else
		For each kk in ON_Prims:kk.visible = 0:next
	end If
end Sub

sub OffPrimsVisible(aValue)
	dim kk
	If aValue then
		For each kk in OFF_Prims:kk.visible = 1:next
		If SideBladeArt = 0 then
			Sidewalls_gioff.visible = 1
			Sidewalls_alt_gioff.visible = 0
		Else
			Sidewalls_gioff.visible = 0
			Sidewalls_alt_gioff.visible = 1
		End If
	Else
		For each kk in OFF_Prims:kk.visible = 0:next
	end If
end Sub


'******************************************************
'****  LAMPZ by nFozzy
'******************************************************



Dim NullFader : set NullFader = new NullFadingObject
Dim Lampz : Set Lampz = New LampFader
Dim ModLampz : Set ModLampz = new NoFader2 ' NoFader2 requires .Update

InitLampsNF              	' Setup lamp assignments
LampTimer.Interval = -1		' Using fixed value so the fading speed is same for every fps
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
	ModLampz.Update
End Sub


Sub DisableLighting(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity
End Sub

Sub DisableLightingHead(pri, DLintensity, ByVal aLvl)	'cp's script  DLintensity = disabled lighting intesity
	if Lampz.UseFunction then aLvl = Lampz.FilterOut(aLvl)	'Callbacks don't get this filter automatically
	pri.blenddisablelighting = aLvl * DLintensity + 0.1
End Sub

Sub InsertIntensityUpdate(ByVal aLvl)
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	dim bulb
	for each bulb in Inserts
		bulb.intensity = bulb.uservalue*(1 + (1-aLvl)*0.5)
	next
End Sub

sub bump1LitOpacity(ByVal aLvl)
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	bcap1Lit.opacity = 200 * aLvl
end sub


sub bump2LitOpacity(ByVal aLvl)
	if Lampz.UseFunction then aLvl = LampFilter(aLvl)	'Callbacks don't get this filter automatically
	bcap2Lit.opacity = 200 * aLvl
end sub


Sub InitLampsNF()

	'Filtering (comment out to disable)
	Lampz.Filter = "LampFilter"	'Puts all lamp intensityscale output (no callbacks) through this function before updating

	'Adjust fading speeds (1 / full MS fading time)
	dim x : for x = 0 to 140 : Lampz.FadeSpeedUp(x) = 1/40 : Lampz.FadeSpeedDown(x) = 1/120 : next
	Lampz.FadeSpeedUp(111) = 1/180 : Lampz.FadeSpeedDown(111) = 1/180 'GI related

	'bumper towers
	Lampz.FadeSpeedUp(18) = 1/150 : Lampz.FadeSpeedDown(18) = 1/300
	Lampz.FadeSpeedUp(19) = 1/150 : Lampz.FadeSpeedDown(19) = 1/300

	'Lampz Assignments
	'MassAssign is an optional way to do assignments. It'll create arrays automatically / append objects to existing arrays
	Lampz.MassAssign(1)= l1
	Lampz.MassAssign(1)= bl1
	Lampz.Callback(1) = "DisableLighting p1, 40,"
	Lampz.MassAssign(2)= l2
	Lampz.MassAssign(2)= bl2
	Lampz.Callback(2) = "DisableLighting p2, 40,"
	Lampz.MassAssign(3)= l3
	Lampz.MassAssign(3)= bl3
	Lampz.Callback(3) = "DisableLighting p3, 70,"
	Lampz.MassAssign(4)= l4
	Lampz.MassAssign(4)= bl4
	Lampz.Callback(4) = "DisableLighting p4, 40,"
	Lampz.MassAssign(5)= l5
	Lampz.MassAssign(5)= bl5
	Lampz.Callback(5) = "DisableLighting p5, 30,"
	Lampz.MassAssign(6)= l6
	Lampz.MassAssign(6)= bl6
	Lampz.Callback(6) = "DisableLighting p6, 80,"
	Lampz.MassAssign(7)= l7
	Lampz.MassAssign(7)= bl7
	Lampz.Callback(7) = "DisableLighting p7, 80,"
	Lampz.MassAssign(8)= l8
	Lampz.MassAssign(8)= bl8
	Lampz.Callback(8) = "DisableLighting p8, 90,"
	Lampz.MassAssign(9)= l9
	Lampz.MassAssign(9)= bl9
	Lampz.Callback(9) = "DisableLighting p9, 15,"

	Lampz.MassAssign(10)= l10
	Lampz.MassAssign(10)= bl10
	Lampz.Callback(10) = "DisableLighting p10, 25,"
	Lampz.MassAssign(11)= l11
	Lampz.MassAssign(11)= bl11
	Lampz.Callback(11) = "DisableLighting p11, 15,"
	Lampz.MassAssign(12)= l12
	Lampz.MassAssign(12)= bl12
	Lampz.Callback(12) = "DisableLighting p12, 15,"
	Lampz.MassAssign(13)= l13
	Lampz.MassAssign(13)= bl13
	Lampz.Callback(13) = "DisableLighting p13, 50,"
	Lampz.MassAssign(14)= l14
	Lampz.MassAssign(14)= bl14
	Lampz.Callback(14) = "DisableLighting p14, 10,"
	Lampz.MassAssign(15)= l15
	Lampz.MassAssign(15)= bl15
	Lampz.Callback(15) = "DisableLighting p15, 15,"
	Lampz.MassAssign(16)= l16
	Lampz.MassAssign(17)= l17
	Lampz.MassAssign(18)= l18a
	Lampz.Callback(18) = "bump1LitOpacity" 
'	Lampz.Callback(18) = "bump1imageswap" 
	Lampz.MassAssign(19)= l19a
	Lampz.Callback(19) = "bump2LitOpacity" 
'	Lampz.Callback(19) = "bump2imageswap" 

	Lampz.MassAssign(20)= l20
	Lampz.MassAssign(20)= bl20
	Lampz.Callback(20) = "DisableLighting p20, 50,"
	Lampz.MassAssign(21)= l21
	Lampz.MassAssign(21)= bl21
	Lampz.Callback(21) = "DisableLighting p21, 60,"
	Lampz.MassAssign(22)= l22
	Lampz.MassAssign(22)= bl22
	Lampz.Callback(22) = "DisableLighting p22, 50,"
	Lampz.MassAssign(23)= l23
	Lampz.MassAssign(23)= bl23
	Lampz.Callback(23) = "DisableLighting p23, 80,"
	Lampz.MassAssign(24)= l24
	Lampz.MassAssign(24)= bl24
	Lampz.Callback(24) = "DisableLighting p24, 80,"
	Lampz.MassAssign(25)= l25
	Lampz.MassAssign(25)= bl25
	Lampz.Callback(25) = "DisableLighting p25, 10,"
	Lampz.MassAssign(26)= l26
	Lampz.MassAssign(26)= bl26
	Lampz.Callback(26) = "DisableLighting p26, 20,"
	Lampz.MassAssign(27)= l27
	Lampz.MassAssign(27)= bl27
	Lampz.Callback(27) = "DisableLighting p27, 15,"
	Lampz.MassAssign(28)= l28
	Lampz.MassAssign(28)= bl28
	Lampz.Callback(28) = "DisableLighting p28, 15,"
	Lampz.MassAssign(29)= l29
	Lampz.MassAssign(29)= bl29
	Lampz.Callback(29) = "DisableLighting p29, 15,"

	Lampz.MassAssign(30)= l30
	Lampz.MassAssign(30)= bl30
	Lampz.Callback(30) = "DisableLighting p30, 15,"
	Lampz.MassAssign(31)= l31
	Lampz.MassAssign(33)= l33
	Lampz.MassAssign(33)= bl33
	Lampz.Callback(33) = "DisableLighting p33, 50,"
	Lampz.MassAssign(34)= l34
	Lampz.MassAssign(34)= bl34
	Lampz.Callback(34) = "DisableLighting p34, 10,"
	Lampz.MassAssign(35)= l35
	Lampz.MassAssign(35)= bl35
	Lampz.Callback(35) = "DisableLighting p35, 25,"
	Lampz.MassAssign(36)= l36
	Lampz.MassAssign(36)= bl36
	Lampz.Callback(36) = "DisableLighting p36, 80,"
	Lampz.MassAssign(37)= l37
	Lampz.MassAssign(37)= bl37
	Lampz.Callback(37) = "DisableLighting p37, 50,"
	Lampz.MassAssign(38)= l38
	Lampz.MassAssign(38)= bl38
	Lampz.Callback(38) = "DisableLighting p38, 10,"
	Lampz.MassAssign(39)= l39
	Lampz.MassAssign(39)= bl39
	Lampz.Callback(39) = "DisableLighting p39, 25,"

	Lampz.MassAssign(40)= l40
	Lampz.MassAssign(40)= bl40
	Lampz.Callback(40) = "DisableLighting p40, 80,"
	Lampz.MassAssign(41)= l41
	Lampz.MassAssign(41)= bl41
	Lampz.Callback(41) = "DisableLighting p41, 80,"
	Lampz.MassAssign(42)= l42
	Lampz.MassAssign(42)= bl42
	Lampz.Callback(42) = "DisableLighting p42, 25,"
	Lampz.MassAssign(43)= l43
	Lampz.MassAssign(43)= bl43
	Lampz.Callback(43) = "DisableLighting p43, 80,"
	Lampz.MassAssign(44)= l44
	Lampz.MassAssign(44)= bl44
	Lampz.Callback(44) = "DisableLighting p44, 80,"
	Lampz.MassAssign(45)= l45
	Lampz.MassAssign(45)= bl45
	Lampz.Callback(45) = "DisableLighting p45, 15,"
	Lampz.MassAssign(46)= l46
	Lampz.MassAssign(46)= bl46
	Lampz.Callback(46) = "DisableLighting p46, 25,"
	Lampz.MassAssign(47)= l47
	Lampz.MassAssign(47)= bl47
	Lampz.Callback(47) = "DisableLighting p47, 80,"
	Lampz.MassAssign(48)= l48
	Lampz.MassAssign(48)= bl48
	Lampz.Callback(48) = "DisableLighting p48, 50,"
	Lampz.MassAssign(49)= l49
	Lampz.MassAssign(49)= bl49
	Lampz.Callback(49) = "DisableLighting p49, 80,"

	Lampz.MassAssign(50)= l50
	Lampz.MassAssign(50)= bl50
	Lampz.Callback(50) = "DisableLighting p50, 10,"
	Lampz.MassAssign(51)= l51
	Lampz.MassAssign(51)= bl51
	Lampz.Callback(51) = "DisableLighting p51, 25,"
	Lampz.MassAssign(52)= l52
	Lampz.MassAssign(52)= bl52
	Lampz.Callback(52) = "DisableLighting p52, 80,"
	Lampz.MassAssign(53)= l53
	Lampz.MassAssign(53)= bl53
	Lampz.Callback(53) = "DisableLighting p53, 80,"
	Lampz.MassAssign(54)= l54
	Lampz.MassAssign(54)= bl54
	Lampz.Callback(54) = "DisableLighting p54, 80,"
	Lampz.MassAssign(55)= l55
	Lampz.MassAssign(55)= bl55
	Lampz.Callback(55) = "DisableLighting p55, 80,"
	Lampz.MassAssign(56)= l56
	Lampz.MassAssign(56)= bl56
	Lampz.Callback(56) = "DisableLighting p56, 80,"

	Lampz.MassAssign(57)= l57
	Lampz.MassAssign(58)= l58
	Lampz.MassAssign(59)= l59
	Lampz.MassAssign(60)= l60
	Lampz.MassAssign(61)= l61
	Lampz.MassAssign(62)= l62

	Lampz.MassAssign(63)= l63
	Lampz.MassAssign(63)= bl63
	Lampz.Callback(63) = "DisableLighting p63, 40,"
	Lampz.MassAssign(64)= l64
	Lampz.MassAssign(64)= bl64
	Lampz.Callback(64) = "DisableLighting p64, 40,"
	Lampz.MassAssign(65)= l65 
	Lampz.MassAssign(65)= bl65 
	Lampz.Callback(65) = "DisableLighting p65, 400,"
	Lampz.MassAssign(66)= l66
	Lampz.MassAssign(66)= bl66
	Lampz.Callback(66) = "DisableLighting p66, 50,"
	Lampz.MassAssign(67)= l67
	Lampz.MassAssign(67)= bl67
	Lampz.Callback(67) = "DisableLighting p67, 50,"
	Lampz.MassAssign(68)= l68
	Lampz.MassAssign(68)= bl68
	Lampz.Callback(68) = "DisableLighting p68, 10,"
	Lampz.MassAssign(69)= l69
	Lampz.MassAssign(69)= bl69
	Lampz.Callback(69) = "DisableLighting p69, 80,"
	Lampz.MassAssign(70)= l70
	Lampz.MassAssign(70)= bl70
	Lampz.Callback(70) = "DisableLighting p70, 40,"


	Lampz.Callback(73) = "DisableLighting l73, 150,"
	Lampz.Callback(74) = "DisableLighting l74, 145,"
	Lampz.Callback(75) = "DisableLighting l75, 140,"
	Lampz.Callback(76) = "DisableLighting l76, 135,"
	Lampz.Callback(77) = "DisableLighting l77, 130,"
	Lampz.Callback(78) = "DisableLighting l78, 125,"
	Lampz.Callback(79) = "DisableLighting l79, 125,"
	Lampz.Callback(80) = "DisableLighting l80, 5,"
	Lampz.Callback(80) = "DisableLighting l80b, 5,"
'	Lampz.Callback(111) = "bartimageswap" 

	'Flasher related
'	Lampz.Callback(100) = "DisableLightingHead HHead, 0.4,"
'	Lampz.Callback(101) = "DisableLighting duffbeer, 0.6,"
'	Lampz.Callback(102) = "DisableLightingHead ComicBookGuy, 0.4,"

	'PWM Flashers
	if UseVPMModSol then 
		ModLampz.Callback(21) = "ModFlash21"
		ModLampz.Callback(22) = "ModFlash22"
		ModLampz.Callback(23) = "ModFlash23"
		ModLampz.Callback(25) = "ModFlash25"
		ModLampz.Callback(26) = "ModFlash26"
		ModLampz.Callback(27) = "ModFlash27"
		ModLampz.Callback(28) = "ModFlash28"
		ModLampz.Callback(29) = "ModFlash29"
		ModLampz.Callback(31) = "ModFlash31"
		ModLampz.Callback(32) = "ModFlash32"
		ModLampz.Init
	end if

	'Updates related to GI
	Lampz.obj(111) = ColtoArray(GI)
	Lampz.Callback(111) = "GIUpdates"

	'Turn off all lamps on startup
	Lampz.Init	'This just turns state of any lamps to 1

	'Immediate update to turn on GI, turn off lamps
	Lampz.Update

End Sub


'Helper functions

Function ColtoArray(aDict)	'converts a collection to an indexed array. Indexes will come out random probably.
	redim a(999)
	dim count : count = 0
	dim x  : for each x in aDict : set a(Count) = x : count = count + 1 : Next
	redim preserve a(count-1) : ColtoArray = a
End Function

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



' version 0.1alt - Drop-in replacement of DynamicLamps, but stripped down For PWM pinmame update. removed all fading, objects are simply updated on a timer
' Setup: 
'	.MassAssign index, object or array of objects	-- Appends flasher or lamp object to a lamp index. For other objects, see '.Callback'
'	.MassAssign(index) = object or array of objects	-- same as above
'	.Init	--	Optional, makes sure all currently assigned lamp object have their states set to 1
'	.Update	--	must be set

' Usage: 
'	.SetLamp index value	--	 set a lamp index
'	.SetModLamp index value	--	set a lamp index - input will be divided by 255
'	.state(index) = value	--	same as SetLamp but uses a different syntax

' Special features: 
'	.Callback = "methodname" -- calls a method with current fading value (0 to 1 float value). For handling .blenddisablelighting or other lighting tricks
'	.Mult(index) = value  -- sets a multiplier on the lighting, for balancing lamps against each GI or other tricks
'	.Filter = "functionname" -- puts *all* lamp indexes through a function. Function must take a numeric value and return a numeric value

Class NoFader2 'Lamps that fade up and down. GI and Flasher handling for PWM pinmame update
	Private UseCallback(32), cCallback(32)
	private Lvl(32)
	private Obj(32)
	private Lock(32)
	private Mult(32)
	'Private UseFunction, cFilter

	Private Sub Class_Initialize() 
		dim x : for x = 0 to uBound(Lock) 
			mult(x) = 1
			Lock(x)=True
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader
		next
	End Sub

	Public Sub Init() : TurnOnStates : end sub	'just call turnonstates for now

	Public Property Let Callback(aIdx, String) : cCallback(aIdx) = String : UseCallBack(aIdx) = True : End Property 'execute
	'Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	'Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function

	'solcallback (solmodcallback) handler 
	Public Sub SetLamp(aIdx, aInput) : Lvl(aIdx) = aInput: Lock(aIdx)=false : End Sub	'0->1 Input
	Public Sub SetModLamp(aIdx, aInput) : Lvl(aIdx) = aInput/255: Lock(aIdx)=false : End Sub	'0->255 Input
	'REM Sub SetModLamp(aIdx, aInput) : Update aIDX, aInput/255 : arrayFLASH(aIDX) = aInput : End Sub	'0->255 Input DEBUG

	Public Property Let State(aIdx, aInput) : Lvl(aIdx) = aInput: Lock(aIdx)=false : End Property
	Public Property Get state(aIdx ) : state = Lvl(aIdx) : end Property

	'Mass assign, Builds arrays where necessary
'	Public Sub MassAssign(aIdx, aInput)
'		If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
'			if IsArray(aInput) then
'				obj(aIdx) = aInput
'			Else
'				Set obj(aIdx) = aInput
'			end if
'		Else
'			Obj(aIdx) = AppendArray(obj(aIdx), aInput)
'		end if	
'	end Sub
	
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

	Private Sub TurnOnStates()	'If obj contains any light objects, set their states to 1
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then 
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x) ': debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.0000001
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx) ': debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.0000001
			end if
		Next
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1
		
	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Lock(aIdx)=False : End Property          ' timer update
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property 


	Public Sub Update()	'Handle object updates. Call on timer 
		dim x,xx : for x = 0 to uBound(Obj)
			if not Lock(x) then
				Lock(x)=True
				if IsArray(obj(x)) Then
					for each xx in obj(x) : xx.IntensityScale = Lvl(x)*Mult(x) : Next
				else 
					obj(x).IntensityScale = Lvl(x)*Mult(x)
				end If

				If UseCallBack(x) then 
'					msgbox "execute" & cCallback(x) & " " & (Lvl(x))
					'execute cCallback(x) & " " & (Lvl(x))
					proc cCallback(x), (Lvl(x))
					
				end If
			end if
		Next
		
	End Sub	
	
End Class


'Class NullFadingObject : Public Property Let IntensityScale(input) : : End Property : End Class



' version 0.1 - Drop-in replacement of DynamicLamps, but stripped down For PWM pinmame update. removed all fading, objects are simply updated as values come in
' Setup: 
'	.MassAssign index, object or array of objects	-- Appends flasher or lamp object to a lamp index. For other objects, see '.Callback'
'	.MassAssign(index) = object or array of objects	-- same as above
'	.Init	--	Optional, makes sure all currently assigned lamp object have their states set to 1

' Usage: 
'	.SetLamp index value	--	 set a lamp index
'	.SetModLamp index value	--	set a lamp index - input will be divided by 255
'	.state(index) = value	--	same as SetLamp but uses a different syntax

' Special features: 
'	.Callback = "methodname" -- calls a method with current fading value (0 to 1 float value). For handling .blenddisablelighting or other lighting tricks
'	.Mult(index) = value  -- sets a multiplier on the lighting, for balancing lamps against each GI or other tricks
'	.Filter = "functionname" -- puts *all* lamp indexes through a function. Function must take a numeric value and return a numeric value

Class NoFader 'Lamps that fade up and down. GI and Flasher handling for PWM pinmame update
	Private UseCallback(32), cCallback(32)
	Public Lvl(32)
	Public Obj(32)
	Private UseFunction, cFilter
	private Mult(32)

	Private Sub Class_Initialize() 
		dim x : for x = 0 to uBound(Obj) 
			mult(x) = 1
			if IsEmpty(obj(x) ) then Set Obj(x) = NullFader
		next
	End Sub

	Public Sub Init() : TurnOnStates : end sub	'just call turnonstates for now

	Public Property Let Callback(idx, String) : cCallback(idx) = String : UseCallBack(idx) = True : End Property 'execute
	Public Property Let Filter(String) : Set cFilter = GetRef(String) : UseFunction = True : End Property
	Public Function FilterOut(aInput) : if UseFunction Then FilterOut = cFilter(aInput) Else FilterOut = aInput End If : End Function

	'solcallback (solmodcallback) handler 
	Public Sub SetLamp(aIdx, aInput) : Update aIDX, aInput : End Sub	'0->1 Input
	'REM Public Sub SetModLamp(aIdx, aInput) : Update aIDX, aInput/255 : End Sub	'0->255 Input
	Public Sub SetModLamp(aIdx, aInput) : Update aIDX, aInput/255 : arrayFLASH(aIDX) = aInput : End Sub	'0->255 Input DEBUG

	Public Property Let State(idx,Value) : Update idx, Value : End Property
	Public Property Get state(idx) : state = Lvl(idx) : end Property

	'Mass assign, Builds arrays where necessary
	' Public Sub MassAssign(aIdx, aInput)
	' 	If typename(obj(aIdx)) = "NullFadingObject" Then 'if empty, use Set
	' 		if IsArray(aInput) then
	' 			obj(aIdx) = aInput
	' 		Else
	' 			Set obj(aIdx) = aInput
	' 		end if
	' 	Else
	' 		Obj(aIdx) = AppendArray(obj(aIdx), aInput)
	' 	end if	
	' end Sub
	
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

	Private Sub TurnOnStates()	'If obj contains any light objects, set their states to 1
		dim debugstr
		dim idx : for idx = 0 to uBound(obj)
			if IsArray(obj(idx)) then 
				dim x, tmp : tmp = obj(idx) 'set tmp to array in order to access it
				for x = 0 to uBound(tmp)
					if typename(tmp(x)) = "Light" then DisableState tmp(x) ': debugstr = debugstr & tmp(x).name & " state'd" & vbnewline
					tmp(x).intensityscale = 0.0000001
				Next
			Else
				if typename(obj(idx)) = "Light" then DisableState obj(idx) ': debugstr = debugstr & obj(idx).name & " state'd (not array)" & vbnewline
				obj(idx).intensityscale = 0.0000001
			end if
		Next
	End Sub
	Private Sub DisableState(ByRef aObj) : aObj.FadeSpeedUp = 1000 : aObj.State = 1 : End Sub	'turn state to 1
		
	Public Property Let Modulate(aIdx, aCoef) : Mult(aIdx) = aCoef : Update aIdx, Lvl(aIdx) : End Property
	Public Property Get Modulate(aIdx) : Modulate = Mult(aIdx) : End Property 

	Private Sub Update(aIDX, aLVL) ' NOT called on a timer!
		Lvl(aIDX) = aLVL ' keep state for reference
		dim xx
		if IsArray(obj(aIDX) ) Then	'if array
			If UseFunction then 
				for each xx in obj(aIDX) : xx.IntensityScale = cFilter(aLVL*mult(aIDX)) : Next
			Else
				for each xx in obj(aIDX) : xx.IntensityScale = aLVL*mult(aIDX) : Next
			End If
		else						'if single lamp or flasher
			If UseFunction then 
				obj(aIDX).Intensityscale = cFilter(aLVL*mult(aIDX))
			Else
				obj(aIDX).Intensityscale = aLVL*mult(aIDX)
			End If
		end if
'		If UseCallBack(aIDX) then execute cCallback(aIDX) & " " & (aLVL*mult(aIDX))	'Callback (execute)
		If UseCallBack(aIDX) then proc cCallback(aIDX), (aLVL*mult(aIDX))	'Callback (execute)

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

Sub FadeDisableLighting(nr, a, alvl)
Select Case FadingLevel(nr)
	Case 0 : a.UserValue = a.UserValue - 0.2
			 If a.UserValue < 0 Then a.UserValue = 0 : FadingLevel(nr) = -1 : end If 
				a.BlendDisableLighting = alvl * a.UserValue 'brightness 
	Case 1 : a.UserValue = a.UserValue + 0.5
		  If a.UserValue > 1 Then a.UserValue = 1 : FadingLevel(nr) = -1 : end If
             a.BlendDisableLighting = alvl * a.UserValue 'brightness
End Select
End Sub

'3 COLOR LED TV-DISPLAY ***********************************************************************************************************************************
 Dim YELLOWL, REDL, GREENL
 YELLOWL = Array(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14,_
    y16, y17, y18, y19, y20, y21, y22, y23, y24, y25, y26, y27, y28, y29,_
    y31, y32, y33, y34, y35, y36, y37, y38, y39, y40, y41, y42, y43, y44,_
    y46, y47, y48, y49, y50, y51, y52, y53, y54, y55, y56, y57, y58, y59,_
    y61, y62, y63, y64, y65, y66, y67, y68, y69, y70, y71, y72, y73, y74,_
    y76, y77, y78, y79, y80, y81, y82, y83, y84, y85, y86, y87, y88, y89,_
    y91, y92, y93, y94, y95, y96, y97, y98, y99, y100, y101, y102, y103, y104,_
    y106, y107, y108, y109, y110, y111, y112, y113, y114, y115, y116, y117, y118, y119,_
    y121, y122, y123, y124, y125, y126, y127, y128, y129, y130, y131, y132, y133, y134,_
    y136, y137, y138, y139, y140, y141, y142, y143, y144, y145, y146, y147, y148, y149)
 
 REDL = Array(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14,_
    r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29,_
    r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44,_
    r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57, r58, r59,_
    r61, r62, r63, r64, r65, r66, r67, r68, r69, r70, r71, r72, r73, r74,_
    r76, r77, r78, r79, r80, r81, r82, r83, r84, r85, r86, r87, r88, r89,_
    r91, r92, r93, r94, r95, r96, r97, r98, r99, r100, r101, r102, r103, r104,_
    r106, r107, r108, r109, r110, r111, r112, r113, r114, r115, r116, r117, r118, r119,_
    r121, r122, r123, r124, r125, r126, r127, r128, r129, r130, r131, r132, r133, r134,_
    r136, r137, r138, r139, r140, r141, r142, r143, r144, r145, r146, r147, r148, r149)
 
 GREENL = Array(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14,_
    g16, g17, g18, g19, g20, g21, g22, g23, g24, g25, g26, g27, g28, g29,_     
    g31, g32, g33, g34, g35, g36, g37, g38, g39, g40, g41, g42, g43, g44,_        
    g46, g47, g48, g49, g50, g51, g52, g53, g54, g55, g56, g57, g58, g59,_      
    g61, g62, g63, g64, g65, g66, g67, g68, g69, g70, g71, g72, g73, g74,_       
    g76, g77, g78, g79, g80, g81, g82, g83, g84, g85, g86, g87, g88, g89,_
    g91, g92, g93, g94, g95, g96, g97, g98, g99, g100, g101, g102, g103, g104,_
    g106, g107, g108, g109, g110, g111, g112, g113, g114, g115, g116, g117, g118, g119,_
    g121, g122, g123, g124, g125, g126, g127, g128, g129, g130, g131, g132, g133, g134,_
    g136, g137, g138, g139, g140, g141, g142, g143, g144, g145, g146, g147, g148, g149)
' 

Sub UpdateLeds
	Dim ChgLED, ii, jj, num, stat, s, x
		ChgLED = Controller.ChangedLEDs(0, &HFFFFF)
			If Not IsEmpty(ChgLED) Then
				For ii = 0 To UBound(chgLED)
				num = chgLED(ii, 0):stat = chgLED(ii, 2)
				for jj = 0 to 6
				x = num * 7 + 6-jj
				s = stat And 3
		Select Case S
			Case 3:
				REDL(x).visible = 0
				GREENL(x).visible = 0
				YELLOWL(x).visible = 1
			Case 2:
				YELLOWL(x).visible = 0
				REDL(x).visible = 0
				GREENL(x).visible = 1
			Case Else:
				YELLOWL(x).visible = 0
				GREENL(x).visible = 0
				REDL(x).visible = 1
			If S = 0 Then REDL(x).visible = 0
		End Select
			stat = (stat And &H3FFC) \ 4
			next
			Next
		End If
 End Sub
'RED LED TV-DISPLAY ***************************************************************************************************************************************

 Dim RLED(20)
 
 RLED(0) = Array(r7, r6, r5, r4, r3, r2, r1)
 RLED(1) = Array(r14, r13, r12, r11, r10, r9, r8)
 RLED(2) = Array(r22, r21, r20, r19, r18, r17, r16)
 RLED(3) = Array(r29, r28, r27, r26, r25, r24, r23)
 RLED(4) = Array(r37, r36, r35, r34, r33, r32, r31)
 RLED(5) = Array(r44, r43, r42, r41, r40, r39, r38)
 RLED(6) = Array(r52, r51, r50, r49, r48, r47, r46)
 RLED(7) = Array(r59, r58, r57, r56, r55, r54, r53)
 RLED(8) = Array(r67, r66, r65, r64, r63, r62, r61)
 RLED(9) = Array(r74, r73, r72, r71, r70, r69, r68)
 RLED(10) = Array(r82, r81, r80, r79, r78, r77, r76)
 RLED(11) = Array(r89, r88, r87, r86, r85, r84, r83)
 RLED(12) = Array(r97, r96, r95, r94, r93, r92, r91)
 RLED(13) = Array(r104, r103, r102, r101, r100, r99, r98)
 RLED(14) = Array(r112, r111, r110, r109, r108, r107, r106)
 RLED(15) = Array(r119, r118, r117, r116, r115, r114, r113)
 RLED(16) = Array(r127, r126, r125, r124, r123, r122, r121)
 RLED(17) = Array(r134, r133, r132, r131, r130, r129, r128)
 RLED(18) = Array(r142, r141, r140, r139, r138, r137, r136)
 RLED(19) = Array(r149, r148, r147, r146, r145, r144, r143)
 
 Sub UpdateRedLeds
Dim ChgLED, ii, jj, chg, num, stat, obj, x
	ChgLED = Controller.ChangedLEDs(0, &HFFFFF)
	If Not IsEmpty(ChgLED) Then
		For ii = 0 To UBound(chgLED)
			num = chgLED(ii, 0):chg = chgLED(ii, 1):stat = chgLED(ii, 2)
			For Each obj in RLED(num)
				If chg And 3 Then obj.visible = ABS((stat And 3)> 0)
				chg = chg \ 4:stat = stat \ 4
			next
		Next
		End If
 End Sub
 

' *********************************************************************
' *********************************************************************

					'Start of VPX call back Functions

' *********************************************************************
' *********************************************************************

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	RS.VelocityCorrect(activeball)
	vpmTimer.PulseSw 62
    RandomSoundSlingshotRight sling1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -37
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 2:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -24
        Case 3:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	LS.VelocityCorrect(activeball)
	vpmTimer.PulseSw 59
    RandomSoundSlingshotLeft sling2
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -37
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 2:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -24
        Case 3:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
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
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * BallRollVolume * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))
		ElseIf BallVel(BOT(b)) > 1 AND BOT(b).z > 145 AND BOT(b).z < 175 Then
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
dim RampBalls(6,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(6)	

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






'***************************************************************
'****  VPW DYNAMIC BALL SHADOWS by Iakki, Apophis, and Wylte
'***************************************************************

Const fovY					= 0		'Offset y position under ball to account for layback or inclination (more pronounced need further back)
Const DynamicBSFactor 		= 0.99	'0 to 1, higher is darker
Const AmbientBSFactor 		= 0.8	'0 to 1, higher is darker
Const AmbientMovement		= 2		'1 to 4, higher means more movement as the ball moves left and right
Const Wideness				= 20	'Sets how wide the dynamic ball shadows can get (20 +5 thinness should be most realistic for a 50 unit ball)
Const Thinness				= 5		'Sets minimum as ball moves away from source

Dim sourcenames, currentShadowCount, DSSources(30), numberofsources, numberofsources_hold
sourcenames = Array ("","","","","","")
currentShadowCount = Array (0,0,0,0,0,0)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!
dim objrtx1(12), objrtx2(12)
dim objBallShadow(12)
Dim BallShadowA
BallShadowA = Array (BallShadowA0,BallShadowA1,BallShadowA2,BallShadowA3,BallShadowA4,BallShadowA5)

DynamicBSInit

sub DynamicBSInit()
	Dim iii, source

	for iii = 0 to tnob									'Prepares the shadow objects before play begins
		Set objrtx1(iii) = Eval("RtxBallShadow" & iii)
		objrtx1(iii).material = "RtxBallShadow" & iii
		objrtx1(iii).z = iii/1000 + 1.01
		objrtx1(iii).visible = 0

		Set objrtx2(iii) = Eval("RtxBall2Shadow" & iii)
		objrtx2(iii).material = "RtxBallShadow2_" & iii
		objrtx2(iii).z = (iii)/1000 + 1.02
		objrtx2(iii).visible = 0

		currentShadowCount(iii) = 0

		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = iii/1000 + 1.04
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




'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1														'volume level; range [0, 1]
NudgeLeftSoundLevel = 1													'volume level; range [0, 1]
NudgeRightSoundLevel = 1												'volume level; range [0, 1]
NudgeCenterSoundLevel = 1												'volume level; range [0, 1]
StartButtonSoundLevel = 0.1												'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr									'volume level; range [0, 1]
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
'****  FLEEP MECHANICAL SOUNDS
'******************************************************





' VR Room code - Rawd...***************************************************************


Sub VRStartBartTimer_Timer()
BartRunning = true
End Sub


Sub MainVRTimer_Timer()

'Farther Passing Cars animation
If PassingCar = 1 then 
VRCar3.y = vrcar3.y - 60
VRCar3.z = vrcar3.z - 6
if vrcar3.y <= -80000 then 
VRcar3.x = 81263.44
VRcar3.y = 70865.46
VRcar3.z = 5400
Dim tmp
tmp=Int(6 * Rnd (1) +1)
vrcar3.image = "Cartexture" & tmp
PassingCar = 2
end if
end if 
If PassingCar = 2 then 
VRCar4.y = vrcar4.y - 90
VRCar4.z = vrcar4.z - 9
if vrcar4.y <= -80000 then 
VRcar4.x = 81263.44
VRcar4.y = 70865.46
VRcar4.z = 5400
Dim tmp6
tmp6=Int(6 * Rnd (1) +1)
vrcar4.image = "Cartexture" & tmp6
PassingCar = 1
end if
end if 

'clouds 
Sky.rotz = Sky.rotz - .002
Sky2.rotz = Sky2.rotz + .003

'Eagle
VREagle.ObjRotZ=VREagle.ObjRotZ+.08
VREagle2.ObjRotZ=VREagle2.ObjRotZ-.06

'move Moe
VRMoe.roty = VRMoe.roty + MoeSpeed
if VRMoe.roty =>200 then MoeSpeed = -0.08 
if VRMoe.roty =<80 then MoeSpeed = 0.08
If MaggieMove1 = true then
VRMaggieRED.x = VRMaggieRED.x - 2
VRMaggieBlue.x = VRMaggieBlue.x - 2
VRMaggieYellow.x = VRMaggieYellow.x -2
VRMaggieWhite.x = VRMaggieWhite.x -2
VRMaggieBlack.x = VRMaggieBlack.x -2
VRMaggieShadow.x = VRMaggieShadow.x -2
if VRMaggieYellow.x =< 2848.022 then 
MaggieMove1 = false
MaggieMove2 = true
end If
End If

' Maggie Soother here
If Maggiemove2 = true then
VRMaggieRED.X = VRMaggieRED.X + SootherSpeed
If VRMaggieRed.X => 2857 then SootherSpeed = -0.4
If VRMaggieRed.X =< 2833 then SootherSpeed = 0.4
end if

'HomerBrainAnimation..
HomerBrain.Rotx = HomerBrain.Rotx + 0.01
HomerBrain.Roty = HomerBrain.Roty + 0.012
HomerBrain.Rotz = HomerBrain.Rotz + 0.014
VR_beercan.Roty = VR_beercan.Roty + 0.15
VRBacon.Roty = VRBacon.Roty + 0.15
VRDonut.Roty = VRDonut.Roty + 0.15
HomerBrain.Size_X = HomerBrain.Size_X + BrainMove
If HomerBrain.Size_X => 20 then BrainMove = -0.02
If HomerBrain.Size_X =< 16 then BrainMove = 0.02
HomerBrain.Size_y = HomerBrain.Size_y + BrainMove2
If HomerBrain.Size_y => 20 then BrainMove2 = -0.025
If HomerBrain.Size_y =< 16 then BrainMove2 = 0.025
HomerBrain.Size_z = HomerBrain.Size_y + BrainMove3
If HomerBrain.Size_z => 20 then BrainMove3 = -0.03
If HomerBrain.Size_z =< 16 then BrainMove3 = 0.03

'Bart Animation
If BartRunning = true then
if BartMove0 = true Then
VRBart2.y = VRBart2.y + 1.2
VRBoard2.y = VRBoard2.y + 1.2
VRBart2.z = VRBart2.z + 2
VRBoard2.z = VRBoard2.z + 2
if VRBart2.z => 9080 Then
Bartmove0 = False
Bartmove1 = True
End If
End if
If Bartmove1 = True then 
VRBart2.y = VRBart2.y + 6
VRBoard2.y = VRBoard2.y + 6
VRBart2.z = VRBart2.z - 10
VRBoard2.z = VRBoard2.z - 10
VRBart2.Rotx = VRBart2.Rotx - 0.4
VRBoard2.Rotx = VRBoard2.Rotx - 0.4
If VRBart2.Rotx =< 70 then 
Bartmove1 = False  
Bartmove2 = true
VRBart2.Rotx = 71
end If
end If
If Bartmove2 = true then 
VRBart2.y = VRBart2.y + 52
VRBoard2.y = VRBoard2.y + 52
VRBart2.z = VRBart2.z - 20.5
VRBoard2.z = VRBoard2.z - 20.5
If VRBart2.y => -8800 then 
Bartmove2 = False
Bartmove3 = true
end If
end If
If Bartmove3 = true then 
VRBart2.Rotx = 90
VRBoard2.Rotx = 90
VRBart2.y = VRBart2.y + 85
VRBoard2.y = VRBoard2.y + 85
VRBart2.z = VRBart2.z - 98
VRBoard2.z = VRBoard2.z - 98
If VRBart2.z =< -1870 then 
VRBartShadow.visible = true
VRBart2.Rotx = 96
VRBoard2.Rotx = 96
Bartmove3 = False
Bartmove4 = true
end If
end If

If Bartmove4 = true then  'start of drivway moving...
VRBart2.Rotz = -10
VRBoard2.Rotz = -10
VRBart2.x = VRBart2.x + 28
VRBoard2.x = VRBoard2.x + 28
VRBart2.y = VRBart2.y + 48
VRBoard2.y = VRBoard2.y + 48
VRBart2.z = VRBart2.z + 4.8
VRBoard2.z = VRBoard2.z + 4.8
VRBartShadow.x = VRBoard2.x + 500
VRBartShadow.y = VRBoard2.y - 1000
VRBartShadow.Height = VRBoard2.z + 8

if VRBart2.y => 5300 then 
 Bartmove4 = False
 Bartmove5 = true
End if
end If
If Bartmove5 = true then 

VRBart2.Roty = VRBart2.Roty +2
VRBoard2.Roty = VRBoard2.Roty +2
VRBart2.x = VRBart2.x - 28
VRBoard2.x = VRBoard2.x - 28
VRBart2.y = VRBart2.y + 48
VRBoard2.y = VRBoard2.y + 48
VRBart2.z = VRBart2.z + 4.8
VRBoard2.z = VRBoard2.z + 4.8
VRBartShadow.x = VRBoard2.x + 800
VRBartShadow.y = VRBoard2.y + 500
VRBartShadow.Height = VRBoard2.z + 58

if VRBart2.Roty = 75 then 
VRBart2.Rotz = 0
VRBoard2.Rotz = 0
Bartmove5 = False
Bartmove6 = true
end if
end If
If Bartmove6 = true then 
VRBart2.X = VRBart2.X - 52
VRBoard2.X = VRBoard2.X - 52

VRBartShadow.x = VRBoard2.x + 800
VRBartShadow.y = VRBoard2.y + 500
VRBartShadow.Height = VRBoard2.z + 65

if VRBart2.X =<  -85000 Then
VRBart2.roty = 140
VRBoard2.roty = 140
VRBart2.y = VRBart2.y - 52
VRBoard2.y = VRBoard2.y - 52
VRBart2.z = VRBart2.z - 7
VRBoard2.z = VRBoard2.z - 7

if VRBart2.y =<  -30000 Then
VRBartShadow.visible = false
BartRunning = false
VRStartBartTimer.interval = 30000
VRStartBartTimer.enabled = true
VRBart2.x = 12256.48  'RESET BART POSITION
VRBart2.y = -18629.27
VRBart2.z = 5900
VRBoard2.x = 12256.48
VRBoard2.y = -18629.27
VRBoard2.z = 5900
VRBart2.Rotx = 96
VRBart2.Roty = -15
VRBart2.Rotz = 0
VRBoard2.Rotx = 96
VRBoard2.Roty = -15
VRBoard2.Rotz = 0
BartMove0 = True ' RESET DIMS for fresh start
BartMove6 = false
exit sub
end if
end if
End If
end if

'Main Moving Car animation
If Truckmove1 = true Then
For Each obj in VRCars:obj.Y = obj.Y + 70:next
For Each obj in VRCars:obj.z = obj.z + 7:next
if VRNewCar1.y => 8200 Then  ' or z = -490 would be better?
Truckmove1 = False
Truckmove2 = True
End If
end if
If Truckmove2 = true then 
   ' turns truck right
For Each obj in VRCars:obj.roty = obj.roty +0.4:next
For Each obj in VRCars:obj.y = obj.y + 15:next ' moves forward a bit while turning:next
For Each obj in VRCars:obj.z = obj.z +1:next
if VRNewCar1.Roty => 180 Then
TruckMove2 = False
TruckMove3 = True
end If
End If
If Truckmove3 = true Then
VRCarShadow.visible = true
For Each obj in VRCars:obj.X = obj.x - 50:next
VRCarShadow.x = VRNewCar1.x + 600 'offset under truck
if VRNewCar1.x =< -89000 Then
Truckmove3 = False
Truckmove4 = True
end If
End if
If Truckmove4 = true Then
VRCarShadow.visible = false 
For Each obj in VRCars:obj.roty = obj.roty +0.4:next   ' turns truck right
For Each obj in VRCars:obj.y = obj.y - 15:next ' moves forward a bit while turning
For Each obj in VRCars:obj.z = obj.z - 1.5:next
if VRnewcar1.Roty => 270 Then
Truckmove4 = False
Truckmove5 = True
end If
End if
If Truckmove5 = true Then
For Each obj in VRCars:obj.y = obj.y - 70:next
For Each obj in VRCars:obj.z = obj.z - 7:next 
If VRnewcar1.y =< - 40000 Then
Truckmove5 = False
'reset cars
VRnewcar1.x = 70594.9
VRnewcar1.y = -41166.79
VRnewcar1.z = -5590
VRnewcar1.roty = 90

VRNewCar2.x = 70594.9
VRNewCar2.y = -41166.79
VRNewCar2.z = -5590
VRNewCar2.roty = 90

VRNewCar3.x = 70594.9
VRNewCar3.y = -41166.79
VRNewCar3.z = -5590
VRNewCar3.roty = 90

VRNewCar4.x = 70594.9
VRNewCar4.y = -41166.79
VRNewCar4.z = -5590
VRNewCar4.roty = 90

Truckmove1 = true
Carnumber =Int(4 * Rnd (1) +1)

If Carnumber = 1 then
VRNewCar1.visible = true
VRNewCar2.visible = false
VRnewCar3.visible = false
VRNewCar4.visible = false
Dim tmp2
tmp2=Int(6 * Rnd (1) +1)
VRNewCar1.image = "Cartexture" & tmp2
end If

If Carnumber = 2 then
VRNewCar1.visible = false
VRNewCar2.visible = true
VRnewCar3.visible = false
VRNewCar4.visible = false
Dim tmp3
tmp3=Int(6 * Rnd (1) +1)
VRNewCar2.image = "Cartexture" & tmp3
end If

If Carnumber = 3 then
VRNewCar1.visible = false
VRNewCar2.visible = false
VRnewCar3.visible = true
VRNewCar4.visible = false
Dim tmp4
tmp4=Int(6 * Rnd (1) +1)
VRNewCar3.image = "Cartexture" & tmp4
end If

If Carnumber = 4 then
VRNewCar1.visible = false
VRNewCar2.visible = false
VRnewCar3.visible = false 
VRNewCar4.visible = true
if VRNewCar4.image = "Cartexture7" Then
VRNewCar4.image = "Cartexture6" 
Else
VRNewCar4.image = "Cartexture7" 
end if
end If
end If
end if
End Sub

Sub HomerBrainTimer_timer()
BrainCount = BrainCount + 1
If BrainCount = 5 then BrainCount = 1
if BrainCount = 1 then VR_beercan.visible = false:VRDonut.visible = false:VRChoke.visible = true:VRBacon.visible = false
if BrainCount = 2 then VR_beercan.visible = false:VRDonut.visible = true:VRChoke.visible = False:VRBacon.visible = false
if BrainCount = 3 then VR_beercan.visible = true:VRDonut.visible = false:VRChoke.visible = false:VRBacon.visible = false
if BrainCount = 4 then VRBacon.visible = true:VR_beercan.visible = false:VRDonut.visible = false:VRChoke.visible = false

End Sub


' VR Plunger stuff below..........
Sub TimerVRPlunger_Timer
  If VRPlungerTest.Y < 2420 then    'guessing on number..  trial and error..		
	 VRPlungerTest.y = VRPlungerTest.y + 5	
  End If
End Sub

Sub TimerVRPlunger2_Timer
  VRPlungerTest.Y = 2310.241 + (5* Plunger.Position) -20
End Sub

Sub SetBackglass()
	Dim obj

	For Each obj In VRBackglass
		obj.x = obj.x 
		obj.height = - obj.y + 340
		obj.y = -65 'adjusts the distance from the backglass towards the user
		obj.rotx=-84
	Next
End Sub


'VR Room Code END *************************************

If Cabinetmode Then
    PinCab_Rails.visible = 0

    If CabinetSideBladeStretch Then
        Sidewalls_alt_gioff.size_y = 200
        Sidewalls_alt_gion.size_y = 200
        Sidewalls_gioff.size_y = 200
        Sidewalls_gion.size_y = 200
		GI_058.BulbHaloHeight = 360
		GI_022.BulbHaloHeight = 360
    Else
        Sidewalls_alt_gioff.size_y = 100
        Sidewalls_alt_gion.size_y = 100
        Sidewalls_gioff.size_y = 100
        Sidewalls_gion.size_y = 100
		GI_058.BulbHaloHeight = 180
		GI_022.BulbHaloHeight = 180
    End If

    'Flasherbase7.rotx = 90
    'Flasherbase7.roty = 1
    'Flasherbase7.rotz = -38
    'Flasherbase7.objrotx = 0
    'Flasherbase7.objroty = 55
    'Flasherbase7.objrotz = 90
    'Flasherlit7.rotx = 90
    'Flasherlit7.roty = 1
    'Flasherlit7.rotz = -38
    'Flasherlit7.objrotx = 0
    'Flasherlit7.objroty = 55
    'Flasherlit7.objrotz = 90
    
Else
    PinCab_Rails.visible = 1
    Sidewalls_alt_gioff.size_y = 100
    Sidewalls_alt_gion.size_y = 100
    Sidewalls_gioff.size_y = 100
    Sidewalls_gion.size_y = 100

    'Flasherbase7.rotx = 90
    'Flasherbase7.roty = 0
    'Flasherbase7.rotz = -45
    'Flasherbase7.objrotx = -35
    'Flasherbase7.objroty = 0
    'Flasherbase7.objrotz = -8.77131
    'Flasherlit7.rotx = 90
    'Flasherlit7.roty = 0
    'Flasherlit7.rotz = -45
    'Flasherlit7.objrotx = -35
    'Flasherlit7.objroty = 0
    'Flasherlit7.objrotz = -8.77131
End If



'001 idigstuff - nfozzy physics
'014 idigstuff - fleep sounds 
'019 idigstuff - ramp triggers and wire ramp sounds 
'027 idigstuff - flupper domes and flashers
'028 idigstuff - insert primitives 
'030 idigstuff - walls rebuild to allow gi lighting to pass through.added gi bulbs to upper PF couch ramp. fixed right ramp post prim and missing slingshot post. added missing itchy flasher.
'031 apophis - insert layover . pf cutouts
'032 idigstuff - wall post fixes
'033 iaakki - blender gi primitve walls.
'034 idigstuff - insert primitives, gi work
'036 idigstuff - lampz code, upf inserts completed.rework simpsons heads to match og
'038 bord - Lots of repositioning for the new playfield - moved all playfield level posts, lane guides and rubbers from Blender to VPX. Replaced nfozzy rubber elements with repositioneed meshes. Added pf mesh
'040 bord - upper playfield metal/rubber/plastic/nfozzy elements remodeled/replaced. New bumper toppers.
'041 idigstuff - added bumper lit prims and code.Duff beer can prim and code. 
'     tomate - duff can model. blender assist
'042 apophis - updated physics, fleep, and ball rolling code to latest versions. Clean up some collections. Locked some objects.
'043 idigstuff - Scratchy flasher added, Enabled sol callbacks HHead and CBG.Adjusted insert overlay depth bias. Auto flipper sound code fix (RAWD)
'044 apophis - Fixed flippers that I broke in 042. Auto flipper sounds work now too. Tuned flasher domes brightness. Added homer head, duff can, and CB guy to lampz so they fade now.
'045 apophis - Added dynamic shadows. Added slingshot corrections.
'046 apophis - Tuned plunger wall and plunger release speed. Fixed ball stuck issue. Made itchy scratcy kicker stronger. Reduced slingshot strength.
'047 idigstuff - created wall for and applied new material lower TV. fixed mode board indicator lights.
'048i iaakki - merged the VR room, tuned homer's head
'049 tomate - new plastic and plastic bevels prims added, new apron model and texture added, new monorail ramp prim added
'050 Sixtoe - Realigned everything, redid everything else.
'051 apophis - Added missing insert for L48. Realigned all inserts. Updated some inserts with correct textures.
'052 tomate - added new wireRamps prims, added new rampsHolders prims, added new monorailRamp prim, plastics and plastic ramps slightly modified to match the new geometries, physical ramps realigned with new prims, LP collidable ramp added to remplace Ramp2
'053 apophis - Fixed some ramp rolling triggers (and error on left ramp). Added insert blooms. Carved out GI around inserts. Made inserts brightness adjust with GI. Tuned some insert colors and brightness. Fixed plunger wall for skillshot.
'053a idigstuff - Added insert lighting to INSERT collection
'054 tomate - New baked wireRamps textures added, new baked monorail ramp texture added, POV corrected
'054 hauntfreaks - image webp conversions
'055 idigstuff - Added missing CBG flasher 
'056 tomate - added new baked sidewalls textures (black wood gion/gioff and alternate art gion), added new baked backwall texture, apron position corrected
'057 apophis - Hooked up baked sidewall and backwall to GI. Made sideblade art option. Fixed homer head texture swap. Adjusted Homer head scales. Shrank Duff Can. Adjusted animated slings. Updated PF image. Automated cabientmode, desktopmode, and vrroom options. Tuned insert bloom intensities. Updated ball image. Deleted old DT image, made simple black one. Updated flasher dome init code. Adjusted Ramp24 and Ramp28. Adjusted nudge strength. Tried to fix ball stuck issue on left ramp.
'058 Sixtoe - Loads of flasher work, made beer can smaller, adjusted foam, hooked up homer head, comic book guy and beer can to flasher system, added playfield reflections
'059 tomate - Plastic ramp collision with upperpf fixed, HaunteFreaks background for desktop mode added
'060 iaakki - readjusted hhead size, rotation point and position, fixed a logic bug in hhead texture swaps, flashers using objtargetlevel now so they follow rom events better, flasherlit DL reduced, UpperPF wall adjusted, various depth bias issues fixed, 
'061 apophis - Hooked up sidewall and backwall bakes to GIUpdates. Added desktop DMD. Fixed right inlane geometry. Reduced insert brightness a slightly when GI OFF. Added some walls to prevent stuck balls. Fixed drop target sound effects.
'062 apophis - Flasher objects now hooked up in FlasherFlash sub with own equations. Tuned flashers to best of my ability. Made flashers not visible on boot. Fixed some minor ball stuck issues. 
'063 apophis - Added flipper primitives (only large flippers). Tuned flashers and blooms again. Fixed dynamic shadow clash with shadow layer. Fixed more missing sound effects. Fixed more stuck ball issues. Adjusted upperPF and upper right flipper physics parameters. 
'064 idigstuff- POV + hide vr cabinet when not in vr mode. updated credits
'065 Rawd - Major VR Room work - Room objects and materials all tweaked and complete code re-do. Cabinet work. Animated all cabinet parts. Added Minimal Room and Ultra Minimal room Options.  More.
'066 Sixtoe - 
'* - Redid upper playfield, altered & moved ramp & ramp prim, rebuilt/fixed ramp prim, new flippers and made larger, moved targets.
'* - Fixed upper playfield insert lights being at normal playfield level
'* - Adjusted ramp5 upper playfield entrance ramp
'* - Messed about with figure lighting, hooking up to GI and also making them fade with flashers properly (may have slight issues in gioff state, but *shrug*)
'* - Moved insert blooms to height 1 from 30 (may have been my fault adjusting one light with everything else connected? don't know)
'* - Remade all mounts and screws for ramp gate mounts and aligned all gates
'Fixed (Duplicate) - Flickering screw and post on the right side of table (Deleting RampsHolders prim fixes it) - not sure what's going on there. (duplicate?)  Yes.
'Fixed (Hooked up to GI) - VR BG should dim with GI? - I think it may be a hair too bright at its current ON setting also?
'Fixed (Raised Apron Blocker Height) - Ball from left flipper, hit right sling bumper and bounced back, up and over the apron and out of play - maybe need a physical PF glass?
'Improved, Not Fixed (some things can't get away from due to how table built) - Dancing lights on the left plastics - Sixtoe fixed this in Tommy.  I can't figure it out again.
'Fixed (was there but set up wrong) - Upper flipper on the main playfield could use a shadow
'Fixed (Adjusted) - Red lighting in the plunger lane doesn't look correct in VR - needs shaping and positioning
'Fixed (Duplicated non collidable apron) - There are no walls under the apron
'Fixed (made invisible, added prim kicker) - Right side kickerhole can be seen through the cabinet
'Fixed? (added resliance to options) - Sideblades are invisible until game resets (Only sometimes? odd)
'Not worth changing, size differential minimal - I brought in a couple of new images that are not .Webp and cannot convert them (My version of PS doesn't support it) (find them in image manager under my name)
'Fixed (size_y x2 in cabinet mode) - Add some more cabinet mode stuff
'Fixed (were there, needed angles adjusting) - Flipper prims!
'Fixed-ish (changed material, could be better) - Toys too glossy'
'
'rc 1.0.0 idigstuff - Release prep, table info updated. Added option for sideblade height. Fixed ball trap under rightflipper2. re-added intro sound
'rc 1.0.3 idigstuff - change intro assignment to backbox for cab. lowered intensity insert blooms.
'rc 1.0.4 tomate - Bart's eyes fixed, new inverted hdri enviromental image to improve right ramp reflections
'rc 1.0.5 rawd - Swapped out large VR bart prim with Tomates fixed model - Reduced glass scratches opacity
're 1.0.6 Sixtoe - Fixed upper playfield gaps, added nuts to sling and inlane plastics, adjust bart captive ball, fixed prim119 db issue, 
'rc 1.0.7 leojreimroc - Reajusted VR Backglass lighting, changed env image
'rc 1.0.8 iaakki - fixed plungerlane from PF image, moved change log to bottom of the script

'101 skitso - remade GI and overall lighting, tweaked all inserts, tweaked flashers
'102 iaakki - fixed some light/flasher logic
'103 skitso - further GI and flasher tweaks
'104 iaakki - reimported physics for flippers and materials
'105 iaakki - Implemented flasher sum limiter, removed hhead texture swaps as they really didn't match the head default texture, fixed flasher code bugs, defaulted to black side blades
'106 iaakki - added backwall flashers
'107 skitso - resized and moved Duff can so Bart won't clip it, further tweaks to GI and inserts, small texture and material tweaks
'108 iaakki - flip eos change, various flasher fixes, DuffCanToy option added
'109 skitso - added original style bumper cap lighting (needs mod switch for the green ones), created a completely new LUT as old had really aggressive gamma curve, further tweaked GI and inserts
'110 skitso - improved upper playfield/miniplayfield/backwall lighting, added spotlighting to sofa and upper middle playfield, fixed few details and visual issues.
'111 skitso - added a separate GI on texture for bart toy (thanks iaakki for the render), improved flasher dome off textures for correct brightness and added disable lighting for GI
'112 skitso - added one missing GI lamp under tv, improved mini playfield lighting, added few missing screws, added option for modded/original garage decal, moved garage decal to correct position, placed new micro toy, new minipf ramp, decal and dome (thanks Tomate!)
'113 skitso - Added ton of missing screws, nuts and bolts, added option for Moe toy, improved garage door area lighting (spot light), improved GI lighting, added missing wall around Homer head, added visible plywood edge to miniplayfield target hole.
'114 skitso - fixed modes sign texture and lights, added bracket and backwall shadow for mode sign, improved garage door texture and lighting, fixed micro screen transparency
'115 skitso - redrew the backwall texture, made on/off option for red plunger light, improved bumper lighting, improved homer head texture, tweaked plunger texture
'116 iaakki - perf improvement for flashers by increasing fading interval 30->35ms and increasing fading speed in equation from 0.8 to 0.75. HHead flash brightness divider set from 1.1 to 1.9 -> lower brightness. Flasher level limiter equation changed -> stays hotter when others flash at the same time. 
'			  GI_050 raised up a bit to eliminate annoying glare in Desktop POV. Some lights cutted not to go outside VR cabinet, Pincab_bottom set visible always so bg not shown through switch holes, Bart's texture fixed by painting, difference mask done and tied to GI fading using additive primitive, 
'			  bumper towers has now option for blue towers, bumber tower lighting redone using additive primitives, bumpers fixed (hit threshold 1.8), flasher preloader added
'117 tomate - post-proc AA disabled. reimported fixed ramps prims. reimported micro with a black background. reimported all the gates with a fixed position. reimported spotlights with a fixed position. fixed upper PF collidable ramp shape to match with the new ramp
'118RC iaakki - rebuild slings and fixed bart off texture a bit
'119RC iaakki - Fixed physics values *one more time*
'120RC iaakki - bumpers rebuild with visible ring and correct animation, comic guy lighting baked and tied to GI
'121RC skitso - Fixed DB issue for insert outline overlay
'122RC iaakki - Fixed grey towers texture, changed flashlight1 db -100 -> 100. Slightly lowered the bumper rings under towers
'123RC Wylte  - Added sound effects to upper pf, fixed some ramp and rubber sounds, extended Bart sw23 enough to not trigger on nudge
'124RC apophis - Fixed stretched side blade lighting height
'125RC Wylte  - Aligned:  Screw009, Primitive61, Primitive62, Screw008, Primitive3, Primitive007, Primitive008, Primitive138, zCol_Rubber_Peg014, Rubber30, zCol_Rubber_Corner_030
'				Hid:  Screw006, Screw007, Primitive60, Primitive67, Screw010, GatePost13, Primitive102, RubberPostT22, Primitive137
'				Created: Primitive1locknut001,4,6,8-17, Screw016
'125bRC Wylte - Gates and Spinner
'126RC Tomate - Fixed CBG-ramp and added metal ramp plate, and removed legacy post primitives, added missing screws
'127RC Skitso - Garage door texture remade and fixed garage door position, corrected yellow garage roof position, fixed GBC ramp gate position, GI_017 height fixed, added missing mini pf support and improved another 
'128RC iaakki - Garage door lighting reworked. VUK to upf sound timing fixed
'130RC iaakki - reworked mini ramp
'v2.0 Release

'v2.0.1 iaakki - Fixed Bart and ComicBookGuy in VRRoom (good catch Roth)
'v2.0.2 apophis - Fixed Bart and ComicBookGuy in VRRoom. Automated VRRoom. Added new flasher PWM support along with Lampz and Modlampz updates.
'v2.0.3 apophis - Fixed flasher_side image (thanks Rawd). Bumped up the DN slider one tick.
