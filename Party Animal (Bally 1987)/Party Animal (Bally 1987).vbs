Option Explicit
Randomize

On Error Resume Next
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "You need the controller.vbs in order to run this table, available in the vp10 package"
On Error Goto 0

Const cGameName="prtyanim",UseSolenoids=1,UseLamps=0,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff", SCoin="coin3"

LoadVPM"01100000","6803.VBS",3.1

Dim DesktopMode: DesktopMode = Table1.ShowDT
If DesktopMode = True Then 'Show Desktop components
Ramp16.visible=1
Ramp15.visible=1
Primitive13.visible=1
Else
Ramp16.visible=0
Ramp15.visible=0
Primitive13.visible=1
End if


' Thalamus 2019 October : Improved directional sounds

' Options
' Volume devided by - lower gets higher sound

Const VolDiv = 2500    ' Lower number, louder ballrolling/collition sound
Const VolCol = 10      ' Ball collition divider ( voldiv/volcol )



'*************************************************************
'Solenoid Call backs
'**********************************************************************************************************

SolCallback(6)="bsR.SolOut"
SolCallback(7)="bsT.SolOut"
SolCallback(8)="bsL.SolOut"
SolCallback(9)="dtDrop.SolDropUp"
SolCallback(12)="bsTrough.SolOut"
SolCallback(14)="bsTrough.SolIn"
SolCallback(15)= "vpmSolSound SoundFX(""Knocker"",DOFKnocker),"
SolCallback(19)="vpmNudge.SolGameOn"

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
     If Enabled Then
		LF.fire
        ' PlaySound SoundFX("fx_Flipperup",DOFContactors)
        PlaySoundAtVol SoundFX("fx_Flipperup",DOFContactors), LeftFlipper, 1
		LeftFlipper1.RotateToEnd
     Else
         ' PlaySound SoundFX("fx_Flipperdown",DOFContactors):LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart
         PlaySoundAtVol SoundFX("fx_Flipperdown",DOFContactors), LeftFlipper, 1:LeftFlipper.RotateToStart:LeftFlipper1.RotateToStart
     End If
  End Sub

Sub SolRFlipper(Enabled)
     If Enabled Then
		RF.fire
        ' PlaySound SoundFX("fx_Flipperup",DOFContactors)
        PlaySoundAtVol SoundFX("fx_Flipperup",DOFContactors), RightFlipper, 1
     Else
         ' PlaySound SoundFX("fx_Flipperdown",DOFContactors):RightFlipper.RotateToStart
         PlaySoundAtVol SoundFX("fx_Flipperdown",DOFContactors), RightFlipper, 1:RightFlipper.RotateToStart
     End If
End Sub


'**********************************************************************************************************
'Solenoid Controlled toys
'**********************************************************************************************************

'*****GI Lights On
dim xx
For each xx in GI:xx.State = 1: Next


'**********************************************************************************************************
'Initiate Table
'**********************************************************************************************************

Dim bsTrough, bsR, bsL, bsT, dtDrop

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "Party Animal Bally"&chr(13)&"You Suck"
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
     End With
     On Error Goto 0

	PinMAMETimer.Interval=1
	PinMAMETimer.Enabled=1

	vpmNudge.TiltSwitch=15
	vpmNudge.Sensitivity=2
	vpmNudge.TiltObj=Array(Bumper1,Bumper2,Bumper3,LeftSlingshot,RightSlingshot)

	Set bsTrough=New cvpmBallStack
		bsTrough.InitSw 8,48,47,46,0,0,0,0
		bsTrough.InitKick BallRelease,90,6
		bsTrough.InitExitSnd SoundFX("ballrelease",DOFContactors), SoundFX("Solenoid",DOFContactors)
		bsTrough.Balls=3

 	Set bsR=New cvpmBallStack
		bsR.InitSaucer sw22,22,302,40
		bsR.InitExitSnd SoundFX("popper_ball",DOFContactors), SoundFX("Solenoid",DOFContactors)

 	Set bsL=New cvpmBallStack
		bsL.InitSaucer sw23,23,85,40
		bsL.InitExitSnd SoundFX("popper_ball",DOFContactors), SoundFX("Solenoid",DOFContactors)

 	Set bsT=New cvpmBallStack
		bsT.InitSaucer sw24,24,180,6
		bsT.InitExitSnd SoundFX("popper_ball",DOFContactors), SoundFX("Solenoid",DOFContactors)

 	Set dtDrop=New cvpmDropTarget
		dtDrop.InitDrop Array(SW33,SW34,SW35),Array(33,34,35)
		dtDrop.InitSnd SoundFX("DTDrop",DOFContactors),SoundFX("DTReset",DOFContactors)

End Sub


'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)

	If keycode = PlungerKey Then Plunger.Pullback:playsoundAtVol"plungerpull", Plunger, 1
	If KeyCode = LeftMagnaSave Then Controller.Switch(5) = 1
    If KeyCode = RightMagnaSave Then Controller.Switch(7) = 1
	If KeyDownHandler(keycode) Then Exit Sub
End Sub

Sub Table1_KeyUp(ByVal KeyCode)

	If keycode = PlungerKey Then Plunger.Fire:PlaySoundAtVol"plunger", Plunger, 1
	If KeyCode = LeftMagnaSave Then Controller.Switch(5) = 0
    If KeyCode = RightMagnaSave Then Controller.Switch(7) = 0
	If KeyUpHandler(keycode) Then Exit Sub
End Sub


'**********************************************************************************************************

 ' Drain hole and kickers
Sub Drain_Hit:bsTrough.addball me : playsoundAtVol"drain" , Drain, 1: End Sub
Sub sw22_Hit:bsR.AddBall 0 : playsoundAtVol "popper_ball", sw22, 1: End Sub
Sub sw23_Hit:bsL.AddBall 0 : playsoundAtVol "popper_ball", sw23, 1: End Sub
Sub sw24_Hit:bsT.AddBall 0 : playsoundAtVol "popper_ball", sw24, 1: End Sub

'Drop Targets
Sub SW33_Dropped:dtDrop.Hit 1:End Sub
Sub SW34_Dropped:dtDrop.Hit 2:End Sub
Sub SW35_Dropped:dtDrop.Hit 3:End Sub

 'Stand Up Targets
Sub SW1_Hit:vpmTimer.PulseSw 1:End Sub
Sub SW25_Hit:vpmTimer.PulseSw 25:End Sub
Sub SW26_Hit:vpmTimer.PulseSw 26:End Sub
Sub SW27_Hit:vpmTimer.PulseSw 27:End Sub
Sub SW28_Hit:vpmTimer.PulseSw 28:End Sub
Sub SW29_Hit:vpmTimer.PulseSw 29:End Sub
Sub SW30_Hit:vpmTimer.PulseSw 30:End Sub
Sub SW36_Hit:vpmTimer.PulseSw 36:End Sub
Sub SW37_Hit:vpmTimer.PulseSw 37:End Sub
Sub SW38_Hit:vpmTimer.PulseSw 38:End Sub

'Wire Triggers
Sub SW2_Hit:Controller.Switch(2)=1 : playsoundAtVol"rollover" , ActiveBall, 1: End Sub
Sub SW2_UnHit:Controller.Switch(2)=0:End Sub
Sub SW3_Hit:Controller.Switch(3)=1 : playsoundAtVol"rollover" , ActiveBall, 1: End Sub
Sub SW3_unHit:Controller.Switch(3)=0:End Sub
Sub SW12_Hit:Controller.Switch(12)=1 : playsoundAtVol"rollover" , ActiveBall, 1: End Sub
Sub SW12_UnHit:Controller.Switch(12)=0:End Sub
Sub SW13_Hit:Controller.Switch(13)=1 : playsoundAtVol"rollover" , ActiveBall, 1: End Sub
Sub SW13_UnHit:Controller.Switch(13)=0:End Sub
Sub SW31_Hit:Controller.Switch(31)=1 : playsoundAtVol"rollover" , ActiveBall, 1: End Sub
Sub SW31_UnHit:Controller.Switch(31)=0:End Sub
Sub SW32_Hit:Controller.Switch(32)=1 : playsoundAtVol"rollover" , ActiveBall, 1: End Sub
Sub SW32_UnHit:Controller.Switch(32)=0:End Sub

'Ramp Triggers
Sub SW4_Hit:Controller.Switch(4)=1 : playsoundAtVol"rollover" , ActiveBall, 1: End Sub
Sub SW4_UnHit:Controller.Switch(4)=0:End Sub

'Bumpers
Sub Bumper1_Hit:vpmTimer.PulseSw 17 : playsoundAtVol SoundFX("fx_bumper1",DOFContactors), ActiveBall, 1: End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 18 : playsoundAtVol SoundFX("fx_bumper1",DOFContactors), ActiveBall, 1: End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 19 : playsoundAtVol SoundFX("fx_bumper1",DOFContactors), ActiveBall, 1: End Sub


 'Scoring Rubber
Sub sw16_Hit:vpmTimer.PulseSw 16 : playsoundAtVol"flip_hit_3" , ActiveBall, 1: End Sub
Sub SW40_Hit:vpmTimer.PulseSw 40 : playsoundAtVol"flip_hit_3" , ActiveBall, 1: End Sub


'******************************************************
'			STEPS 2-4 (FLIPPER POLARITY SETUP
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

' This second InitPolarity was crashing vpinball, so I commented it out
'InitPolarity

Sub InitPolarity()
	dim x, a : a = Array(LF, RF)
	for each x in a
		'safety coefficient (diminishes polarity correction only)
		x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1	'disabled
		x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1

		x.enabled = True
		x.TimeDelay = 44
	Next

	'"Polarity" Profile
	AddPt "Polarity", 0, 0, 0
	AddPt "Polarity", 1, 0.368, -4
	AddPt "Polarity", 2, 0.451, -3.7
	AddPt "Polarity", 3, 0.493, -3.88
	AddPt "Polarity", 4, 0.65, -2.3
	AddPt "Polarity", 5, 0.71, -2
	AddPt "Polarity", 6, 0.785,-1.8
	AddPt "Polarity", 7, 1.18, -1
	AddPt "Polarity", 8, 1.2, 0


	'"Velocity" Profile
	addpt "Velocity", 0, 0, 	1
	addpt "Velocity", 1, 0.16, 1.06
	addpt "Velocity", 2, 0.41, 	1.05
	addpt "Velocity", 3, 0.53, 	1'0.982
	addpt "Velocity", 4, 0.702, 0.968
	addpt "Velocity", 5, 0.95,  0.968
	addpt "Velocity", 6, 1.03, 	0.945

	LF.Object = LeftFlipper
	LF.EndPoint = EndPointLp	'you can use just a coordinate, or an object with a .x property. Using a couple of simple primitive objects
	RF.Object = RightFlipper
	RF.EndPoint = EndPointRp
End Sub



'Trigger Hit - .AddBall activeball
'Trigger UnHit - .PolarityCorrect activeball

Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub


'**********************************************************************************************************
'Frog and Mushroom Bumper
'**********************************************************************************************************

Dim FrogHopStep

Sub sw39_hit()
	vpmTimer.PulseSw(39)
	FrogHopStep = 0
	sw39.TimerEnabled = 1
	playsoundAtVol SoundFX("fx_bumper1",DOFContactors), ActiveBall, 1
End Sub


Sub sw39_Timer()
	Select Case FrogHopStep
	Case 1:FrogPrim.ObjRotX = 10:CapPrim.TransY = 10
	Case 2:FrogPrim.ObjRotX = 20:CapPrim.TransY = 20
	Case 3:FrogPrim.ObjRotX = 30:CapPrim.TransY =30
	Case 4:FrogPrim.ObjRotX = 40:CapPrim.TransY = 40
	Case 5:FrogPrim.ObjRotX = 30:CapPrim.TransY = 30
	Case 6:FrogPrim.ObjRotX = 20:CapPrim.TransY =20
	Case 7:FrogPrim.ObjRotX = 10:CapPrim.TransY =10
	Case 8:FrogPrim.ObjRotX = 0:CapPrim.TransY = 0:FrogHopStep = 0:sw39.TimerEnabled = 0
 End Select

FrogHopStep = FrogHopStep + 1

End Sub


'******************************************************
'		FLIPPER CORRECTION SUPPORTING FUNCTIONS
'******************************************************

Sub AddPt(aStr, idx, aX, aY)	'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub

'Methods:
'.TimeDelay - Delay before trigger shuts off automatically. Default = 80 (ms)
'.AddPoint - "Polarity", "Velocity", "Ycoef" coordinate points. Use one of these 3 strings, keep coordinates sequential. x = %position on the flipper, y = output
'.Object - set to flipper reference. Optional.
'.StartPoint - set start point coord. Unnecessary, if .object is used.

'Called with flipper -
'ProcessBalls - catches ball data.
' - OR -
'.Fire - fires flipper.rotatetoend automatically + processballs. Requires .Object to be set to the flipper.

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt	'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay	'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart, FlipperEnd, LR, PartialFlipCoef
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
	Public Property Let EndPoint(aInput) : if IsObject(aInput) then FlipperEnd = aInput.x else FlipperEnd = aInput : end if : End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property

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
				if DebugOn then StickL.visible = True : StickL.x = balldata(x).x		'debug TODO
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
		if abs(Flipper.currentAngle - Flipper.EndAngle) < 30 Then
			PartialFlipCoef = 0
		End If
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function	'Timer shutoff for polaritycorrect

	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1
			dim teststr : teststr = "Cutoff"
			tmp = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
			if tmp < 0.1 then 'if real ball position is behind flipper, exit Sub to prevent stucks	'Disabled 1.03, I think it's the Mesh that's causing stucks, not this
				if DebugOn then TestStr = "real pos < 0.1 ( " & round(tmp,2) & ")" : tbpl.text = Teststr
				'RemoveBall aBall
				'Exit Sub
			end if

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				if DebugOn then teststr = "y velocity: " & round(aBall.vely, 3) & "exit sub" : tbpl.text = teststr
				RemoveBall aBall
				exit Sub
			end if
			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					'TB.TEXT = balldata(x).id & " " & BALLDATA(X).X & VBNEWLINE & FLIPPERSTART & " " & FLIPPEREND
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)				'find safety coefficient 'ycoef' data
				end if
			Next

			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				if DebugOn then set tmp = new spoofball : tmp.data = aBall : End If
				if IsEmpty(BallData(idx).id) and aBall.VelY < -12 then 'if tip hit with no collected data, do vel correction anyway
					if PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1) > 1.1 then 'adjust plz
						VelCoef = LinearEnvelope(5, VelocityIn, VelocityOut)
						if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
						if Enabled then aBall.Velx = aBall.Velx*VelCoef'VelCoef
						if Enabled then aBall.Vely = aBall.Vely*VelCoef'VelCoef
						if DebugOn then teststr = "tip protection" & vbnewline & "velcoef: " & round(velcoef,3) & vbnewline & round(PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1),3) & vbnewline
						'debug.print teststr
					end if
				Else
		 : 			VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
					if Enabled then aBall.Velx = aBall.Velx*VelCoef
					if Enabled then aBall.Vely = aBall.Vely*VelCoef
				end if
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1	'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
			'debug
			if DebugOn then
				TestStr = teststr & "%pos:" & round(BallPos,2)
				if IsEmpty(PolarityOut(0) ) then
					teststr = teststr & vbnewline & "(Polarity Disabled)" & vbnewline
				else
					teststr = teststr & "+" & round(1 *(AddX*ycoef*PartialFlipcoef),3)
					if BallPos >= PolarityOut(uBound(PolarityOut) ) then teststr = teststr & "(MAX)" & vbnewline else teststr = teststr & vbnewline end if
					if Ycoef < 1 then teststr = teststr &  "ycoef: " & ycoef & vbnewline
					if PartialFlipcoef < 1 then teststr = teststr & "PartialFlipcoef: " & round(PartialFlipcoef,4) & vbnewline
				end if

				teststr = teststr & vbnewline & "Vel: " & round(BallSpeed(tmp),2) & " -> " & round(ballspeed(aBall),2) & vbnewline
				teststr = teststr & "%" & round(ballspeed(aBall) / BallSpeed(tmp),2)
				tbpl.text = TestSTR
			end if
		Else
			'if DebugOn then tbpl.text = "td" & timedelay
		End If
		RemoveBall aBall
	End Sub
End Class


'******************************************************
'		HELPER FUNCTIONS
'******************************************************


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

Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub


Function BallSpeed(ball) 'Calculates the ball speed
    BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

Function PSlope(Input, X1, Y1, X2, Y2)	'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

Function NullFunctionZ(aEnabled):End Function	'1 argument null function placeholder	 TODO move me or replac eme

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


Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)	'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)	'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	'Clamp if on the boundry lines
	'if L=1 and Y < yLvl(LBound(yLvl) ) then Y = yLvl(lBound(yLvl) )
	'if L=uBound(xKeyFrame) and Y > yLvl(uBound(yLvl) ) then Y = yLvl(uBound(yLvl) )
	'clamp 2.0
	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) ) 	'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )	'Clamp upper

	LinearEnvelope = Y
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

	   'Special Handling
	   'If chgLamp(ii,0) = 2 Then solTrough chgLamp(ii,1)
	   'If chgLamp(ii,0) = 4 Then PFGI chgLamp(ii,1)

        Next
    End If
    UpdateLamps
End Sub


Sub UpdateLamps()
NFadeL 1, L1
NFadeL 2, L2
NFadeL 3, L3
NFadeL 4, L4
NFadeL 5, L5
NFadeL 6, L6
NFadeL 7, L7
NFadeL 8, L8
NFadeL 9, L9
NFadeL 10, L10
NFadeL 11, L11
FlupperFlash 12, Flasherflash1, Flasherlit1, Flasherbase1, Flasherlight1'NFadeL 12, L12
NFadeL 13, L13 'Flasher Toad
NFadeL 14, L14
NFadeL 15, L15

NFadeL 17, L17
NFadeL 18, L18
NFadeL 19, L19
NFadeL 20, L20
NFadeL 21, L21
NFadeL 22, L22
NFadeL 23, L23
NFadeL 24, L24
NFadeL 25, L25
NFadeL 26, L26
NFadeL 27, L27
Flash 28, Flasher28 'dim Flasher Fox
NFadeL 29, L29
Flash 30, Flasher30 'Flasher Fox
NFadeL 31, L31

NFadeL 33, L33
NFadeL 34, L34
NFadeL 35, L35
NFadeL 36, L36
NFadeL 37, L37
NFadeL 38, L38
NFadeL 39, L39
NFadeL 40, L40
NFadeL 41, L41
NFadeL 42, L42
FlupperFlashm 43, Flasherflash5, Flasherlit5, Flasherbase5, Flasherlight5'Flash 60, Flasher60 'R BackWall
Flash 43, Flasher25
FlupperFlash 44, Flasherflash4, Flasherlit4, Flasherbase4, Flasherlight4'NFadeL 44, L44
NFadeL 45, L45
Flash 46, Flasher46 'Flasher Hippo
NFadeL 47, L47

NFadeLm 49, L49  'Bumper 1
NFadeL 49, L49a
NFadeL 50, L50
NFadeL 51, L51
NFadeL 52, L52
NFadeL 53, L53
NFadeL 54, L54
NFadeL 55, L55
NFadeL 56, L56
NFadeL 57, L57
NFadeL 58, L58
NFadeL 59, L59
FlupperFlashm 60, Flasherflash6, Flasherlit6, Flasherbase6, Flasherlight6'Flash 43, Flasher43 'L Backwall
Flash 60, Flasher26
NFadeLm 61, L61 'Flasher InLine Drop Targets
NFadeLm 61, L61a
NFadeL 61, L61b
'Flashm 62, Flasher62 'JukeBox Left Inner
'Flash 62, Flasher62a 'JukeBox Left Inner
NFadeL 63, L63

NFadeL 65, L65  'Bumper 2
NFadeL 65, L65a
NFadeL 66, L66
NFadeLm 67, L67
Flash 67, Flasher67
NFadeL 68, L68
NFadeL 69, L69
NFadeL 70, L70
NFadeL 71, L71
NFadeL 72, L72
NFadeL 73, L73
NFadeL 74, L74
NFadeL 75, L75
FlupperFlash 76, Flasherflash3, Flasherlit3, Flasherbase3, Flasherlight3'NFadeL 76, L76
'Flashm 77, Flasher77 'JukeBox Top  "ON"
'Flash 77, Flasher77a 'JukeBox Top  "ON"
'Flashm 78, Flasher78 'JukeBox Right Inner
'Flash 78, Flasher78a 'JukeBox Right Inner
NFadeL 79, L79

NFadeL 81, L81 'Bumper 3
NFadeL 81, L81a
NFadeL 82, L82
NFadeL 83, L83
NFadeL 84, L84
NFadeL 85, L85
NFadeL 86, L86
NFadeL 87, L87
NFadeL 88, L88
NFadeL 89, L89
NFadeL 90, L90
FlupperFlash 91, Flasherflash2, Flasherlit2, Flasherbase2, Flasherlight2'NFadeL 91, L91
Flash 92, Flasher92 'dim Flasher Hippo
'Flashm 93, Flasher93 'JukeBox Right Outer
'Flash 93, Flasher93a 'JukeBox Right Outer
'Flashm 94, Flasher94 'JukeBox Right Outer
'Flash 94, Flasher94a 'JukeBox Right Outer
NFadeL 95, L95



	If LampState(77) = 0 and LampState(93) = 0 and LampState(62) = 0 and LampState(78) = 0 and LampState(94) = 0 Then
		pJukeBox.Image = "00000"
		pJukeBox.DisableLighting = .1
	End If

	If LampState(77) = 1 and LampState(93) = 0 and LampState(62) = 0 and LampState(78) = 0 and LampState(94) = 0 Then
		pJukeBox.Image = "10000"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 0 and LampState(93) = 1 and LampState(62) = 0 and LampState(78) = 0 and LampState(94) = 0 Then
		pJukeBox.Image = "01000"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 0 and LampState(93) = 0 and LampState(62) = 1 and LampState(78) = 0 and LampState(94) = 0 Then
		pJukeBox.Image = "00100"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 0 and LampState(93) = 0 and LampState(62) = 0 and LampState(78) = 1 and LampState(94) = 0 Then
		pJukeBox.Image = "00010"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 0 and LampState(93) = 0 and LampState(62) = 0 and LampState(78) = 0 and LampState(94) = 1 Then
		pJukeBox.Image = "00001"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 1 and LampState(93) = 1 and LampState(62) = 0 and LampState(78) = 0 and LampState(94) = 0 Then
		pJukeBox.Image = "11000"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 1 and LampState(93) = 0 and LampState(62) = 1 and LampState(78) = 0 and LampState(94) = 0 Then
		pJukeBox.Image = "10100"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 1 and LampState(93) = 0 and LampState(62) = 0 and LampState(78) = 1 and LampState(94) = 0 Then
		pJukeBox.Image = "10010"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 1 and LampState(93) = 0 and LampState(62) = 0 and LampState(78) = 0 and LampState(94) = 1 Then
		pJukeBox.Image = "10001"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 0 and LampState(93) = 1 and LampState(62) = 1 and LampState(78) = 0 and LampState(94) = 0 Then
		pJukeBox.Image = "01100"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 0 and LampState(93) = 1 and LampState(62) = 0 and LampState(78) = 1 and LampState(94) = 0 Then
		pJukeBox.Image = "01010"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 0 and LampState(93) = 1 and LampState(62) = 0 and LampState(78) = 0 and LampState(94) = 1 Then
		pJukeBox.Image = "01001"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 0 and LampState(93) = 0 and LampState(62) = 1 and LampState(78) = 1 and LampState(94) = 0 Then
		pJukeBox.Image = "00110"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 0 and LampState(93) = 0 and LampState(62) = 1 and LampState(78) = 0 and LampState(94) = 1 Then
		pJukeBox.Image = "00101"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 0 and LampState(93) = 0 and LampState(62) = 0 and LampState(78) = 1 and LampState(94) = 1 Then
		pJukeBox.Image = "00011"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 1 and LampState(93) = 1 and LampState(62) = 1 and LampState(78) = 0 and LampState(94) = 0 Then
		pJukeBox.Image = "11100"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 1 and LampState(93) = 0 and LampState(62) = 1 and LampState(78) = 1 and LampState(94) = 0 Then
		pJukeBox.Image = "10110"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 1 and LampState(93) = 0 and LampState(62) = 0 and LampState(78) = 1 and LampState(94) = 1 Then
		pJukeBox.Image = "10011"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 1 and LampState(93) = 0 and LampState(62) = 1 and LampState(78) = 0 and LampState(94) = 1 Then
		pJukeBox.Image = "10101"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 1 and LampState(93) = 1 and LampState(62) = 0 and LampState(78) = 1 and LampState(94) = 0 Then
		pJukeBox.Image = "11010"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 1 and LampState(93) = 1 and LampState(62) = 0 and LampState(78) = 0 and LampState(94) = 1 Then
		pJukeBox.Image = "11001"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 0 and LampState(93) = 1 and LampState(62) = 0 and LampState(78) = 1 and LampState(94) = 1 Then
		pJukeBox.Image = "01011"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 0 and LampState(93) = 0 and LampState(62) = 1 and LampState(78) = 1 and LampState(94) = 1 Then
		pJukeBox.Image = "00111"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 1 and LampState(93) = 1 and LampState(62) = 1 and LampState(78) = 1 and LampState(94) = 0 Then
		pJukeBox.Image = "11110"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 0 and LampState(93) = 1 and LampState(62) = 1 and LampState(78) = 1 and LampState(94) = 1 Then
		pJukeBox.Image = "01111"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 1 and LampState(93) = 0 and LampState(62) = 1 and LampState(78) = 1 and LampState(94) = 1 Then
		pJukeBox.Image = "10111"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 1 and LampState(93) = 1 and LampState(62) = 0 and LampState(78) = 1 and LampState(94) = 1 Then
		pJukeBox.Image = "11011"
		pJukeBox.DisableLighting = 1
	End If


	If LampState(77) = 1 and LampState(93) = 1 and LampState(62) = 1 and LampState(78) = 0 and LampState(94) = 1 Then
		pJukeBox.Image = "11101"
		pJukeBox.DisableLighting = 1
	End If

	If LampState(77) = 1 and LampState(93) = 1 and LampState(62) = 1 and LampState(78) = 1 and LampState(94) = 1 Then
		pJukeBox.Image = "11111"
		pJukeBox.DisableLighting = 1
	End If

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


'******************************************************
'		Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'******************************************************

Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 21
    ' PlaySound SoundFX("right_slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    PlaySoundAtVol SoundFX("right_slingshot",DOFContactors), sling1, 1
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 20
    ' PlaySound SoundFX("left_slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    PlaySoundAtVol SoundFX("left_slingshot",DOFContactors), sling2, 1
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
End Sub


'***************************************************
'       FLUPPER FLASH
'***************************************************

Dim FlashLevel1, FlashLevel2, FlashLevel3, FlashLevel4, FlashLevel5, FlashLevel6, FlasherLevel7, FlasherLevel8, FlasherLevel9, FlasherLevel10
FlasherLight1.IntensityScale = 0
Flasherlight2.IntensityScale = 0
Flasherlight3.IntensityScale = 0
Flasherlight4.IntensityScale = 0
Flasherlight5.IntensityScale = 0
Flasherlight6.IntensityScale = 0
'Flasherlight7.IntensityScale = 0
'Flasherlight8.IntensityScale = 0


Sub FlupperFlash(nr, FlashObject, LitObject, BaseObject, LightObject)
	FadeEmpty nr
	FlupperFlashm nr, FlashObject, LitObject, BaseObject, LightObject
End Sub

Sub FlupperFlashm(nr, FlashObject, LitObject, BaseObject, LightObject)
	'exit sub
	dim flashx3
	Select Case FadingLevel(nr)
        Case 4, 5
			' This section adapted from Flupper's script
			flashx3 = FlashLevel(nr) * FlashLevel(nr) * FlashLevel(nr)
            FlashObject.IntensityScale = flashx3
			LitObject.BlendDisableLighting = 10 * flashx3
			BaseObject.BlendDisableLighting = (flashx3 * .6) + .4
			LightObject.IntensityScale = flashx3
			LitObject.material = "domelit" & Round(9 * FlashLevel(nr))
			LitObject.visible = 1
			FlashObject.visible = 1
		case 3:
			LitObject.visible = 0
			FlashObject.visible = 0
	end select
End Sub

Sub FadeEmpty(nr)	'Fade a lamp number, no object updates
    Select Case FadingLevel(nr)
		Case 3
			FadingLevel(nr) = 0
        Case 4 'off
            FlashLevel(nr) = FlashLevel(nr) - FlashSpeedDown(nr)
            If FlashLevel(nr) < FlashMin(nr) Then
                FlashLevel(nr) = FlashMin(nr)
               FadingLevel(nr) = 3 'completely off
            End if
            'Object.IntensityScale = FlashLevel(nr)
        Case 5 ' on
            FlashLevel(nr) = FlashLevel(nr) + FlashSpeedUp(nr)
            If FlashLevel(nr) > FlashMax(nr) Then
                FlashLevel(nr) = FlashMax(nr)
                FadingLevel(nr) = 6 'completely on
            End if
            'Object.IntensityScale = FlashLevel(nr)
		Case 6
			FadingLevel(nr) = 1
    End Select
End Sub


' *******************************************************************************************************
' Positional Sound Playback Functions by DJRobX and Rothbauerw
' PlaySound sound, 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
' *******************************************************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position

Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
  PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Set position as table object (Use object or light but NOT wall) and Vol to 1

Sub PlaySoundAt(soundname, tableobj)
  PlaySound soundname, 1, 1, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed.

Sub PlaySoundAtBall(soundname)
  PlaySoundAt soundname, ActiveBall
End Sub

'Set position as table object and Vol manually.

Sub PlaySoundAtVol(sound, tableobj, Volume)
  PlaySound sound, 1, Volume, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Set all as per ball position & speed, but Vol Multiplier may be used eg; PlaySoundAtBallVol "sound",3

Sub PlaySoundAtBallVol(sound, VolMult)
  PlaySound sound, 0, Vol(ActiveBall) * VolMult, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 1, AudioFade(ActiveBall)
End Sub

'Set position as bumperX and Vol manually.

Sub PlaySoundAtBumperVol(sound, tableobj, Vol)
  PlaySound sound, 1, Vol, AudioPan(tableobj), 0,0,1, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBOTBallZ(sound, BOT)
    PlaySound sound, 0, ABS(BOT.velz)/17, Pan(BOT), 0, Pitch(BOT), 1, 0, AudioFade(BOT)
End Sub

' play a looping sound at a location with volume
Sub PlayLoopSoundAtVol(sound, tableobj, Vol)
	PlaySound sound, -1, Vol, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub


'*********************************************************************
'                     Supporting Ball & Sound Functions
'*********************************************************************

Function RndNum(min, max)
    RndNum = Int(Rnd() * (max-min + 1) ) + min ' Sets a random number between min and max
End Function

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.y * 2 / table1.height-1
  If tmp > 0 Then
    AudioFade = Csng(tmp ^10)
  Else
    AudioFade = Csng(-((- tmp) ^10) )
  End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = tableobj.x * 2 / table1.width-1
  If tmp > 0 Then
    AudioPan = Csng(tmp ^10)
  Else
    AudioPan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
  Dim tmp
  On Error Resume Next
  tmp = ball.x * 2 / table1.width-1
  If tmp > 0 Then
    Pan = Csng(tmp ^10)
  Else
    Pan = Csng(-((- tmp) ^10) )
  End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
  Vol = Csng(BallVel(ball) ^2 / VolDiv)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
  Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
  BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function BallVelZ(ball) 'Calculates the ball speed in the -Z
    BallVelZ = INT((ball.VelZ) * -1 )
End Function

Function VolZ(ball) ' Calculates the Volume of the sound based on the ball speed in the Z
    VolZ = Csng(BallVelZ(ball) ^2 / 200)*1.2
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

Sub RollingTimer_Timer()
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
        If BallVel(BOT(b) ) > 1 AND BOT(b).z < 30 Then
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

'***Ball Drop Sounds***

		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
			PlaySound "fx_ball_drop" & b, 0, ABS(BOT(b).velz)/17, AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
		End If

    Next
End Sub


'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub


'*****************************************
'	ninuzzu's	FLIPPER SHADOWS
'*****************************************

sub FlipperTimer_Timer()
	FlipperLSh.RotZ = LeftFlipper.currentangle
	FlipperRSh.RotZ = RightFlipper.currentangle
	FlipperLSh1.RotZ = LeftFlipper1.currentangle


	LFlip.RotY = LeftFlipper.CurrentAngle + 232
	LFlip1.RotY = LeftFlipper1.CurrentAngle + 203
	RFlip.RotY = RightFlipper.CurrentAngle + 128

	If GI_007.state=1 Then
		flasher1.visible=1
		flasher2.visible=0
	Else
		flasher1.visible=0
		flasher2.visible=1
	End if

End Sub


'*****************************************
'	ninuzzu's	BALL SHADOW
'*****************************************

Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

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
        If BOT(b).X < Table1.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) + 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Table1.Width/2))/7)) - 6
        End If
        ballShadow(b).Y = BOT(b).Y + 12
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
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
  debug.print "pinhit_low"
	PlaySound "pinhit_low", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit (idx)
  debug.print "target"
	PlaySound "target", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit (idx)
  debug.print "metalhit_thin"
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit (idx)
  debug.print "metalhit_medium"
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit (idx)
  debug.print "metalhit2"
	PlaySound "metalhit2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit (idx)
  debug.print "fx_gate"
	PlaySound "gate4", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
  debug.print "fx_spinner"
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then
  debug.print "fx_rubber2"
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
  debug.print "fx_rubber2"
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
  debug.print "rubber_hit_X"
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
  debug.print "flipper_hit_X"
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
	End Select
End Sub

