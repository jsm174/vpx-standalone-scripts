Option Explicit
Randomize

Const BallSize = 50

Const cGameName="hvymetal",UseSolenoids=1,UseLamps=1,UseGI=0,SSolenoidOn="SolOn",SSolenoidOff="SolOff",SFlipperOn="fx_Flipperup",SFlipperOff="fx_Flipperdown"
Const SCoin="coin"


LoadVPM "01000100", "6803.VBS", 1.2
Dim DesktopMode: DesktopMode = Table1.ShowDT

'******************* Options *********************
' DMD/Backglass Controller Setting
Const cController = 3		'0=Use value defined in cController.txt, 1=VPinMAME, 2=UVP server, 3=B2S server
'*************************************************

'*************************************************************
Dim cNewController
Sub LoadVPM(VPMver, VBSfile, VBSver)
	Dim FileObj, ControllerFile, TextStr

	On Error Resume Next
	If ScriptEngineMajorVersion < 5 Then MsgBox "VB Script Engine 5.0 or higher required"
	ExecuteGlobal GetTextFile(VBSfile)
	If Err Then MsgBox "Unable to open " & VBSfile & ". Ensure that it is in the same folder as this table. " & vbNewLine & Err.Description

	cNewController = 1
	If cController = 0 then
		Set FileObj=CreateObject("Scripting.FileSystemObject")
		If Not FileObj.FolderExists(UserDirectory) then 
			Msgbox "Visual Pinball\User directory does not exist. Defaulting to vPinMame"
		ElseIf Not FileObj.FileExists(UserDirectory & "cController.txt") then
			Set ControllerFile=FileObj.CreateTextFile(UserDirectory & "cController.txt",True)
			ControllerFile.WriteLine 1: ControllerFile.Close
		Else
			Set ControllerFile=FileObj.GetFile(UserDirectory & "cController.txt")
			Set TextStr=ControllerFile.OpenAsTextStream(1,0)
			If (TextStr.AtEndOfStream=True) then
				Set ControllerFile=FileObj.CreateTextFile(UserDirectory & "cController.txt",True)
				ControllerFile.WriteLine 1: ControllerFile.Close
			Else
				cNewController=Textstr.ReadLine: TextStr.Close
			End If
		End If
	Else
		cNewController = cController
	End If

	Select Case cNewController
		Case 1
			Set Controller = CreateObject("VPinMAME.Controller")
			If Err Then MsgBox "Can't Load VPinMAME." & vbNewLine & Err.Description
			If VPMver>"" Then If Controller.Version < VPMver Or Err Then MsgBox "VPinMAME ver " & VPMver & " required."
			If VPinMAMEDriverVer < VBSver Or Err Then MsgBox VBSFile & " ver " & VBSver & " or higher required."
		Case 2
			Set Controller = CreateObject("UltraVP.BackglassServ")
		Case 3,4
			Set Controller = CreateObject("B2S.Server")
	End Select
	On Error Goto 0
End Sub
'*************************************************************

'Solenoid Call backs
'**********************************************************************************************************

SolCallback(6)  = "bsLSaucer.SolOut Not"
SolCallback(7)  = "bsRSaucer.SolOut Not"
SolCallback(10)	= "PinUp Not"
SolCallback(11)	= "SolPinDownSol11"
SolCallback(12) = "bsTrough.SolOut"
SolCallback(14) = "bsTrough.SolIn Not"
SolCallback(15) = "vpmSolSound""knocker"","
SolCallback(19) = "vpmNudge.SolGameOn"

'Flashers BackGlass Only
'SolCallback(16)  = 'Backbox Light
'SolCallback(17)  = 'Top Light
'SolCallback(18)  = 'Flipper (Backbox)

SolCallback(sLLFlipper)="vpmSolFlipper LeftFlipper,nothing,"
SolCallback(sLRFlipper)="vpmSolFlipper RightFlipper,nothing,"

'**********************************************************************************************************
'Primitive Flipper
Sub FlipperTimer_Timer
	LFLogo.roty = LeftFlipper.currentangle
	RFlogo.roty = RightFlipper.currentangle
End Sub


'Solenoid Controlled toys
'**********************************************************************************************************
Sub SolPinDownSol11(enabled)
If enabled Then
		PinDownSol11.IsDropped = True
		PinUp1.IsDropped = True
        PosLP.Z = -50
        PosRP.Z = -50
		If LockBalls>0 Then
			Select Case LockBalls
			Case 5:Kicker3.Kick 180,4:Kicker3.Enabled=1:Kicker4.Kick 180,1:Kicker4.Enabled=1:Kicker5.Kick 180,4:Kicker5.Enabled=1:Kicker6.Kick 180,4:Kicker6.Enabled=1:Kicker7.Kick 180,4
			Case 4:Kicker3.Kick 180,4:Kicker3.Enabled=1:Kicker4.Kick 180,1:Kicker4.Enabled=1:Kicker5.Kick 180,4:Kicker5.Enabled=1:Kicker6.Kick 180,4
			Case 3:Kicker3.Kick 180,4:Kicker3.Enabled=1:Kicker4.Kick 180,1:Kicker4.Enabled=1:Kicker5.Kick 180,4
			Case 2:Kicker3.Kick 180,4:Kicker3.Enabled=1:Kicker4.Kick 180,1
			Case 1:Kicker3.Kick 180,4
			End Select
			LockBalls=LockBalls-1
		End If
	End If
End Sub

Sub PinUp(enabled)
	If enabled Then
		PinDownSol11.IsDropped = False
		PinUp1.IsDropped = False
		Kicker3.Kick 175,6
        PosLP.Z = 0
        PosRP.Z = 0
	End If
End Sub

'**********************************************************************************************************

'Initiate Table
'**********************************************************************************************************

Dim bsTrough, bsLSaucer, bsRSaucer, LockBalls, DVEL

Sub Table1_Init
	vpmInit Me
	On Error Resume Next
		With Controller
		.GameName = cGameName
		If Err Then MsgBox "Can't start Game" & cGameName & vbNewLine & Err.Description : Exit Sub
		.SplashInfoLine = "heavy Metal Meltdown "&chr(13)&"You Suck"
		.HandleMechanics=0
		.HandleKeyboard=0
		.ShowDMDOnly=1
		.ShowFrame=0
		.ShowTitle=0
        .hidden = 1
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

    vpmNudge.TiltSwitch = 15
	vpmNudge.Sensitivity = 2 
    vpmNudge.tiltobj = Array(LeftSlingShot,RightSlingShot,Bumper1,Bumper2,Bumper3)

	Set bsTrough = New cvpmBallStack
	    bsTrough.InitSw 0,8,36,37,38,39,40,0
	    bsTrough.InitKick BallRelease, 90, 10
	    bsTrough.InitExitSnd "ballrelease", "solenoid"
	    bsTrough.Balls = 5

    Set bsLSaucer = New cvpmBallStack
        bsLSaucer.InitSaucer sw1, 1, 173, 5
        bsLSaucer.InitExitSnd "Popper", "solenoid"

    Set bsRSaucer = New cvpmBallStack
        bsRSaucer.InitSaucer sw2, 2, 180, 5
        bsRSaucer.InitExitSnd "Popper", "solenoid"

End sub

'**********************************************************************************************************
'Plunger code
'**********************************************************************************************************

Sub Table1_KeyDown(ByVal KeyCode)
	If KeyDownHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Pullback:playsound"plungerpull"
End Sub

Sub Table1_KeyUp(ByVal KeyCode)
	If KeyUpHandler(keycode) Then Exit Sub
	If keycode = PlungerKey Then Plunger.Fire:PlaySound"plunger"
End Sub

'**********************************************************************************************************

' *******************
'   Drain and Switch
' *******************
Sub Drain_Hit:PlaySound "drain":bsTrough.AddBall Me:End Sub
Sub Sw1_Hit:bsLSaucer.AddBall 0 End Sub
Sub Sw2_Hit:bsRSaucer.AddBall 0 End Sub

'**********
' Bumpers
'**********
Sub Bumper1_Hit:vpmTimer.PulseSw 17: playsound"fx_bumper1": End Sub
Sub Bumper2_Hit:vpmTimer.PulseSw 18: playsound"fx_bumper1": End Sub
Sub Bumper3_Hit:vpmTimer.PulseSw 19: playsound"fx_bumper1": End Sub


Sub Sw3_Hit:Controller.Switch(3)=1
	If ActiveBall.VelY>-1.4 Then ActiveBall.VelY=1.4
	If ActiveBall.VelY<-8 Then ActiveBall.VelY=-8
End Sub
Sub Sw3_UnHit():Controller.Switch(3)=0: End Sub

Sub Sw4_Hit():PlaySound "rail_low_slower":Controller.Switch(4)=1
If LockBalls>0 Then
	Select Case LockBalls
		Case 1:Kicker3.Enabled=0
		Case 2:Kicker4.Enabled=0
		Case 3:Kicker5.Enabled=0
		Case 4:Kicker6.Enabled=0
		Case 5:Kicker7.Enabled=0
	End Select
LockBalls=LockBalls-1
End If
Controller.Switch(4)=1:End Sub	
Sub Sw4_UnHit():Controller.Switch(4)=0: End Sub

 
Sub Sw12_Hit(): Controller.Switch(12)=1 : playsound"rollover" : End Sub 
Sub Sw12_UnHit():Controller.Switch(12)=0: End Sub
Sub Sw13_Hit(): Controller.Switch(13)=1 : playsound"rollover" : End Sub 
Sub Sw13_UnHit():Controller.Switch(13)=0: End Sub
Sub Sw22_Hit(): Controller.Switch(22)=1 : playsound"rollover" : End Sub 
Sub Sw22_UnHit():Controller.Switch(22)=0: End Sub
Sub Sw23_Hit(): Controller.Switch(23)=1 : playsound"rollover" : End Sub 
Sub Sw23_UnHit():Controller.Switch(23)=0: End Sub
Sub Sw24_Hit(): Controller.Switch(24)=1 : playsound"rollover" : End Sub 
Sub Sw24_UnHit():Controller.Switch(24)=0: End Sub
Sub Sw25_Hit(): Controller.Switch(25)=1 : playsound"rollover" : End Sub 
Sub Sw25_UnHit():Controller.Switch(25)=0: End Sub
Sub Sw26_Hit(): Controller.Switch(26)=1 : playsound"rollover" : End Sub 
Sub Sw26_UnHit():Controller.Switch(26)=0: End Sub
Sub Sw27_Hit(): Controller.Switch(27)=1 : playsound"rollover" : End Sub 
Sub Sw27_UnHit():Controller.Switch(27)=0: End Sub
Sub Sw28_Hit(): Controller.Switch(28)=1 : playsound"rollover" : End Sub 
Sub Sw28_UnHit():Controller.Switch(28)=0: End Sub
Sub Sw29_Hit(): Controller.Switch(29)=1 : playsound"rollover" : End Sub 
Sub Sw29_UnHit():Controller.Switch(29)=0: End Sub
Sub Sw30_Hit(): Controller.Switch(30)=1 : playsound"rollover" : End Sub 
Sub Sw30_UnHit():Controller.Switch(30)=0: End Sub

'Stand Up targets
Sub Sw31_Hit:vpmTimer.PulseSw 31:End Sub
Sub Sw32_Hit:vpmTimer.PulseSw 32:End Sub
Sub Sw33_Hit:vpmTimer.PulseSw 33:End Sub
Sub Sw34_Hit:vpmTimer.PulseSw 34:End Sub
Sub Sw35_Hit:vpmTimer.PulseSw 35:End Sub


Sub Trigger5_Hit
If Not PinUp1.IsDropped And LockBalls>0 Then
	DVEL=-ActiveBall.VelY
	If DVEL>3 Then
	Select Case LockBalls
		Case 1:Kicker3.Enabled=1:Kicker3.Kick 10,DVEL
		Case 2:Kicker4.Enabled=1:Kicker4.Kick 10,DVEL
		Case 3:Kicker5.Enabled=1:Kicker5.Kick 10,DVEL
		Case 4:Kicker6.Enabled=1:Kicker6.Kick 10,DVEL
		Case 5:Kicker7.Enabled=1:Kicker7.Kick 10,DVEL
	End Select
	End If
End If
If LockBalls=0 Then Kicker3.Enabled=0:kicker4.Enabled=0:Kicker5.Enabled=0:kicker6.enabled=0:kicker7.enabled=0
End Sub

Sub Trigger10_Hit:ActiveBall.VelY=0:ActiveBall.VelX=4
Select Case LockBalls
	Case 0:Kicker3.Enabled=1:Kicker7.Enabled=0:Kicker6.Enabled=0:Kicker5.Enabled=0:Kicker4.Enabled=0
	Case 1:Kicker4.Enabled=1:Kicker7.Enabled=0:Kicker6.Enabled=0:Kicker5.Enabled=0
	Case 2:Kicker5.Enabled=1:Kicker7.Enabled=0:Kicker6.Enabled=0
	Case 3:Kicker6.Enabled=1:Kicker7.Enabled=0
	Case 4:Kicker7.Enabled=1
End Select
LockBalls=LockBalls+1
End Sub


	Set Lights(1) = Light1
	Set Lights(2) = Light2
	Set Lights(3) = Light3
	Set Lights(6) = Light6
	Set Lights(7) = Light7
	Set Lights(8) = Light8
	Set Lights(9) = Light9
	Set Lights(10) = Light10
	Set Lights(12) = Light12 'Right Ramp Flasher
	Lights(13) = array(Light13,Light13a,Light13b) 'Upper Left Flasher
	Set Lights(14) = Light14 'Left Ramp Flasher
	Set Lights(15) = Light15
	Set Lights(17) = Light17
	Set Lights(18) = Light18
	Set Lights(19) = Light19
	Set Lights(21) = Light21
	Set Lights(22) = Light22
	Set Lights(23) = Light23
	Set Lights(24) = Light24
 	Set Lights(25) = Light25
	Set Lights(26) = Light26
	Lights(28) = array(Light28,Light28a,Light28b) 'Middle Blue Flasher
	Lights(29) = array(Light29,Light29a,Light29b) 'Upper right Flasher
	Set Lights(30) = Light30
	Set Lights(31) = Light31
 	Set Lights(33) = Light33
	Set Lights(34) = Light34
	Set Lights(37) = Light37
	Set Lights(38) = Light38
	Set Lights(39) = Light39
	Set Lights(40) = Light40
	Set Lights(41) = Light41
	Set Lights(42) = Light42
	Lights(44) = array(Light44,Light44a,Light44b) 'Middle red Flasher
	Set Lights(46) = Light46
	Set Lights(47) = Light47
	Set Lights(49) = Light49
	Set Lights(50) = Light50
	Set Lights(51) = Light51
	Set Lights(54) = Light54
	Lights(55) = array(Light55,Light55a) 'Right Bumper
	Set Lights(56) = Light56
	Set Lights(57) = Light57
	Set Lights(58) = Light58
	Set Lights(59) = Light59
	Set Lights(60) = Light60
	Set Lights(63) = Light63
	Set Lights(65) = Light65
	Set Lights(66) = Light66
	Set Lights(67) = Light67
	Lights(70) = array(Light70,Light70a)' Left Bumper
	Set Lights(72) = Light72
	Set Lights(73) = Light73
	Set Lights(74) = Light74
	Set Lights(75) = Light75
	Set Lights(76) = Light76
	Set Lights(77) = Light77'Target 33 flahser
	Set Lights(78) = Light78
	Set Lights(79) = Light79
	Set Lights(81) = Light81
	Set Lights(82) = Light82
	Set Lights(85) = Light85
	Lights(86) = array(Light86,Light86a)' Middle Bumper
	Set Lights(88) = Light88
	Set Lights(89) = Light89
	Set Lights(90) = Light90
	Set Lights(91) = Light91
	Set Lights(92) = Light92
	Set Lights(93) = Light93
	Set Lights(94) = Light94
	Set Lights(95) = Light95

	'backwall drum lighs
	Set Lights(43) = Light43 'upper Right backwall 90 degree flasher
	Set Lights(45) = Light45 'upper Left backwall 90 degree flasher
	Set Lights(11) = Light11' Top Box Left
	Set Lights(27) = Light27' Top Box Middle
	Set Lights(87) = Light87' Top Box Righ
	Set Lights(52) = Light52' Snare Drum Middle
	Set Lights(61) = Light61' Drum Metal
	Set Lights(62) = Light62' Drum Heavy
	Set Lights(68) = Light68' Snare Drum Right
	Set Lights(83) = Light83' Snare Drum Left

'Flahser Controlled by lights
Sub LSample_Timer()
	Flasher11.visible = Light11.state
	Flasher27.visible = Light27.state
	Flasher87.visible = Light87.state
	Flasher43.visible = Light43.state
	Flasher43a.visible = Light43.state
	Flasher45.visible = Light45.state
	Flasher45a.visible = Light45.state
	Flasher52.visible = Light52.state
	Flasher61.visible = Light61.state
	Flasher62.visible = Light62.state
	Flasher68.visible = Light68.state
	Flasher43.visible = Light83.state
End Sub

'**********************************************************************************************************
' Backglass Light Displays
Dim Digits(28)
Digits(0)=Array(a00,a01,a02,a03,a04,a05,a06,n,a08)
Digits(1)=Array(a10,a11,a12,a13,a14,a15,a16,n,a18)
Digits(2)=Array(a20,a21,a22,a23,a24,a25,a26,n,a28)
Digits(3)=Array(a30,a31,a32,a33,a34,a35,a36,n,a38)
Digits(4)=Array(a40,a41,a42,a43,a44,a45,a46,n,a48)
Digits(5)=Array(a50,a51,a52,a53,a54,a55,a56,n,a58)
Digits(6)=Array(b00,b01,b02,b03,b04,b05,b06,n,b08)

Digits(7)=Array(b10,b11,b12,b13,b14,b15,b16,n,b18)
Digits(8)=Array(b20,b21,b22,b23,b24,b25,b26,n,b28)
Digits(9)=Array(b30,b31,b32,b33,b34,b35,b36,n,b38)
Digits(10)=Array(b40,b41,b42,b43,b44,b45,b46,n,b48)
Digits(11)=Array(b50,b51,b52,b53,b54,b55,b56,n,b58)
Digits(12)=Array(c00,c01,c02,c03,c04,c05,c06,n,c08)
Digits(13)=Array(c10,c11,c12,c13,c14,c15,c16,n,c18)

Digits(14)=Array(c20,c21,c22,c23,c24,c25,c26,n,c28)
Digits(15)=Array(c30,c31,c32,c33,c34,c35,c36,n,c38)
Digits(16)=Array(c40,c41,c42,c43,c44,c45,c46,n,c48)
Digits(17)=Array(c50,c51,c52,c53,c54,c55,c56,n,c58)
Digits(18)=Array(d00,d01,d02,d03,d04,d05,d06,n,d08)
Digits(19)=Array(d10,d11,d12,d13,d14,d15,d16,n,d18)
Digits(20)=Array(d20,d21,d22,d23,d24,d25,d26,n,d28)

Digits(21)=Array(d30,d31,d32,d33,d34,d35,d36,n,d38)
Digits(22)=Array(d40,d41,d42,d43,d44,d45,d46,n,d48)
Digits(23)=Array(d50,d51,d52,d53,d54,d55,d56,n,d58)
Digits(24)=Array(e00,e01,e02,e03,e04,e05,e06,n,e08)
Digits(25)=Array(e10,e11,e12,e13,e14,e15,e16,n,e18)
Digits(26)=Array(f00,f01,f02,f03,f04,f05,f06,n,f08)
Digits(27)=Array(f10,f11,f12,f13,f14,f15,f16,n,f18)



Sub DisplayTimer_Timer
	Dim ChgLED,ii,num,chg,stat,obj
	ChgLed = Controller.ChangedLEDs(&Hffffffff, &Hffffffff)
If Not IsEmpty(ChgLED) Then
		If DesktopMode = True Then
		For ii = 0 To UBound(chgLED)
			num = chgLED(ii, 0) : chg = chgLED(ii, 1) : stat = chgLED(ii, 2)
			if (num < 28) then
				For Each obj In Digits(num)
					If chg And 1 Then obj.State = stat And 1 
					chg = chg\2 : stat = stat\2
				Next
			else
				'if char(stat) > "" then msg(num) = char(stat)
			end if
		next
		end if
end if
End Sub


'**********************************************************************************************************
'**********************************************************************************************************
'*****GI Lights On
dim xx
For each xx in GI:xx.State = 1: Next

'**********Sling Shot Animations
' Rstep and Lstep  are the variables that increment the animation
'****************
Dim RStep, Lstep

Sub RightSlingShot_Slingshot
	vpmTimer.PulseSw 21
    PlaySound "left_slingshot", 0, 1, 0.05, 0.05
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:RightSlingShot.TimerEnabled = 0:
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	vpmTimer.PulseSw 20
    PlaySound "right_slingshot",0,1,-0.05,0.05
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:LeftSlingShot.TimerEnabled = 0:
    End Select
    LStep = LStep + 1
End Sub



' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
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
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
	PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0
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

' The sound is played using the VOL, PAN and PITCH functions, so the volume and pitch of the sound
' will change according to the ball speed, and the PAN function will change the stereo position according
' to the position of the ball on the table.


'**************************************
' Explanation of the collision routine
'**************************************

' The collision is built in VP.
' You only need to add a Sub OnBallBallCollision(ball1, ball2, velocity) and when two balls collide they 
' will call this routine. What you add in the sub is up to you. As an example is a simple Playsound with volume and paning
' depending of the speed of the collision.


Sub Pins_Hit (idx)
	PlaySound "pinhit_low", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Targets_Hit (idx)
	PlaySound "target", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0
End Sub

Sub Metals_Thin_Hit (idx)
	PlaySound "metalhit_thin", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals_Medium_Hit (idx)
	PlaySound "metalhit_medium", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Metals2_Hit (idx)
	PlaySound "metalhit2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Gates_Hit (idx)
	PlaySound "gate4", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner",0,.25,0,0.25
End Sub

Sub Rubbers_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 20 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 20 then
 		RandomSoundRubber()
 	End If
End Sub

Sub Posts_Hit(idx)
 	dim finalspeed
  	finalspeed=SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
 	If finalspeed > 16 then 
		PlaySound "fx_rubber2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End if
	If finalspeed >= 6 AND finalspeed <= 16 then
 		RandomSoundRubber()
 	End If
End Sub

Sub RandomSoundRubber()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "rubber_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "rubber_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "rubber_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub

Sub LeftFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
 	RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySound "flip_hit_1", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 2 : PlaySound "flip_hit_2", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
		Case 3 : PlaySound "flip_hit_3", 0, Vol(ActiveBall), Pan(ActiveBall), 0, Pitch(ActiveBall), 1, 0
	End Select
End Sub
