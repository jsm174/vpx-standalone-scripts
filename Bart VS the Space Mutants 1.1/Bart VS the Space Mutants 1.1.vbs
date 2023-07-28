
'**  
'**          , ,\ ,'\,'\ ,'\ ,\ , 
'**    ,  ;\/ \' `'     `   '  /| 
'**    |\/                      | 
'**    :                        | 
'**    :                        | 
'**     |                       | 
'**     |                       | 
'**     :               -.     _| 
'**      :                \     `. 
'**      |         ________:______\ 
'**      :       ,'o       / o    ; 
'**      :       \       ,'-----./ 
'**       \_      `--.--'        ) 
'**      ,` `.              ,---'| 
'**      : `                     | 
'**       `,-'                   | 
'**       /      ,---.          ,' 
'**    ,-'            `-,------' 
'**   '   `.        ,--' 
'**         `-.____/ 
'**                \ 
'

'**   Blank Table With Scores by Ghost
'**   Bart VS the Space Mutants. Based on NES game
'**   Using free art from google by Matt Groening
'**    All sound effects found free online.



Option Explicit
Randomize

Const TableName = "BartVsTheSpaceMutants"
Dim Ball
Const Balls = 5 'if you want 3 balls you may change this variable
Dim Credits
Dim Match
Dim Game_Over
Dim NewBallDelay
Dim Special
Const Special1 = 62000
Const Special2 = 76000
Const Special3 = 84000
Dim Special1Awarded, Special2Awarded, Special3Awarded
Dim Score, Add10, Add100, Add1000
Dim HighScore
Dim Bonus
Dim BonusMultiplier
Dim BonusCountDelay
Dim ResetTargetsDelay
Dim MatchDelay


' HighScoreReel.SetValue Highscore
' HighScoreReel.AddValue "84000"


PlaySound "1BartMusic",-1

Sub table1_Init
Dim ii
    Ball = 0:UpdateBallNumber
    Credits = 0
    Match = 0

    NewBallDelay = 0
    Special = False
    Special1Awarded = False
    Special1Awarded = False
    Special1Awarded = False
    Score = 0
    BonusCountDelay = 0
	ResetTargetsDelay = 0
	MatchDelay = 0
    Game_Over = TRUE
    Clear_Match
    loadhs
    LoadCredits
    CreditReel.Setvalue Credits
    ScoreReel1.SetValue Highscore
    'StartShake
    GameTimer.Enabled = 1
    ' reset VP objects
' Setup the lightning according to the nightday slider
If table1.nightday < 50 Then
    for each ii in GI: ii.intensity = ii.intensity + (100 - table1.nightday)/10: next
    'bumper1light.opacity=bumper1light.opacity + (100 - table1.nightday)^2
    'bumper2light.opacity=bumper2light.opacity + (100 - table1.nightday)^2
End If
End Sub

Sub Gate2_Timer:Gateheavy.RotX = Gate2.Currentangle:End Sub

Sub GameTimer_Timer

    ' check the delays

    If NewBallDelay > 0 Then
        NewBallDelay = NewBallDelay - 1
        If NewBallDelay = 0 Then NewBall:End If
    End If

    If ResetTargetsDelay > 0 Then
        ResetTargetsDelay = ResetTargetsDelay - 1
        If ResetTargetsDelay = 0 Then ResetDroptargets:End If
    End If

    If MatchDelay > 0 Then
        MatchDelay = MatchDelay - 1
        If MatchDelay = 0 Then Verification_Match:End If
    End If



    If BonusCountDelay > 0 Then
        BonusCountDelay = BonusCountDelay - 1
        If BonusCountDelay = 0 Then CountBonus:End If
    End If
End Sub






Sub Table1_KeyDown(ByVal keycode)

     	If keycode = LeftFlipperKey Then
		LeftFlipper.RotateToEnd
		PlaySound "fx_flipperup", 0, .67, -0.05, 0.05
	End If
    
	If keycode = RightFlipperKey Then
		RightFlipper.RotateToEnd
		PlaySound "fx_flipperup", 0, .67, 0.05, 0.05
	End If



    If keycode = PlungerKey Then Plunger.Pullback:PlaySound "fx_Plungerpull"



    If keycode = AddCreditKey And Credits < 8 Then
        PlaySound "fx_coin"
        Addcredits 1
    End If

    If keycode = AddCreditKey2 And Credits < 8 Then
        PlaySound "fx_coin"
        AddCredits 2
    End If

    If keycode = StartGameKey And Credits > 0 AND Game_Over = TRUE Then
        AddCredits -1
        'PlaySound "fx_Startup"
        Initialize
        Clear_Match
        Game_Over = False
        NextBall
    End If

    If keycode = LeftTiltKey Then Nudge 90, 6:PlaySound "fx_nudge"
    If keycode = RightTiltKey Then Nudge 270, 6:PlaySound "fx_nudge"
    If keycode = CenterTiltKey Then Nudge 0, 7:PlaySound "fx_nudge"
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If keycode = PlungerKey Then Plunger.Fire:PlaySound "fx_Plunger"


    	If keycode = LeftFlipperKey Then
		LeftFlipper.RotateToStart
		PlaySound "fx_flipperdown", 0, 1, -0.05, 0.05
	End If
    
	If keycode = RightFlipperKey Then
		RightFlipper.RotateToStart
		PlaySound "fx_flipperdown", 0, 1, 0.05, 0.05
	End If
   
End Sub

'***************************************
'     Ghost  flippers
'**************************************



	







Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 6, -0.05, 0.05
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 6, 0.05, 0.05
End Sub

'**********************
' Drain and Reset subs
'**********************

' end of ball flow
'1 Drain ball - remove ball
'1 Check Tilt
'2 Count bonus
'3 Check highscore
'4 Next Ball (back to 1 until all balls are played)
'5 Game Over

Sub Drain_Hit()


Light3.State = 0
Light4.State = 0
Light5.State = 0
Light6.State = 0
Light7.State = 0
Light8.State = 0
Light9.State = 0
Light11.State = 0
Light12.State = 0
Light13.State = 0
Light14.State = 0


Light15.State = 0
Light16.State = 0
Light17.State = 0
Light18.State = 0
Light19.State = 0
Light20.State = 0
Light21.State = 0









    Drain.DestroyBall
    PlaySound "fx_Drain"
    CountBonus
End Sub

Sub Initialize 'beginning of a new game
    Score = 0
    Special = False
    Special1Awarded = False
    Special1Awarded = False
    Special1Awarded = False
    ScoreReel1.ResetToZero
    Bonus = 0
    BonusMultiplier = 0
End Sub

Sub Reset_Table() 'after each new ball
    Special = False
    'Setup other lights & objects
    l16.State = 1 '5 trigger lights
    l17.State = 1
    ResetTargetsDelay = 10
    dtdropped = 0
End Sub

Sub NewBall
    PlaySound "fx_ballrel"
    BallRelease.createsizedball(27)
    BallRelease.Kick 90, 5
End Sub

Sub NextBall()
    Ball = Ball + 1
    If Ball = Balls then l14.State = 1 'double bonus on the las ball
    If Ball > Balls Then               'this is the last ball
        GameOver
    Else
        UpdateBallNumber
        Reset_Table
        NewBallDelay = 10
    End If
End Sub

Sub GameOver
    Ball = 0:UpdateBallNumber 'display the game over sign
    Game_Over = True
    Savehs
End Sub

Sub UpdateBallNumber 'if ball = 0 shows the Game Over
    Select Case Ball
        Case 0:Ball5.SetValue 0:GameoverR.SetValue 1
        Case 1:GameoverR.SetValue 0:Ball1.SetValue 1
        Case 2:Ball1.SetValue 0:Ball2.SetValue 1
        Case 3:Ball2.SetValue 0:Ball3.SetValue 1
        Case 4:Ball3.SetValue 0:Ball4.SetValue 1
        Case 5:Ball4.SetValue 0:Ball5.SetValue 1
    End Select
End Sub

'*****************
'      Tilt
'*****************




Sub IncreaseMatch
    Match = (Match + 10) MOD 100
End Sub

Sub Verification_Match()
    PlaySound "fx_match"
    Display_Match
    If(Score MOD 100) = Match Then
        ExtraGame
    End If
End Sub

Sub AddCredits(value)
    Credits = Credits + value
    CreditReel.SetValue Credits
    SaveCredits
End Sub

Sub Clear_Match()
    Mtext0.SetValue 0
    Mtext1.SetValue 0
    Mtext2.SetValue 0
    Mtext3.SetValue 0
    Mtext4.SetValue 0
    Mtext5.SetValue 0
    Mtext6.SetValue 0
    Mtext7.SetValue 0
    Mtext8.SetValue 0
    Mtext9.SetValue 0
End Sub

Sub Display_Match()
    Select Case Match
        Case 0:Mtext0.SetValue 1
        Case 10:Mtext1.SetValue 1
        Case 20:Mtext2.SetValue 1
        Case 30:Mtext3.SetValue 1
        Case 40:Mtext4.SetValue 1
        Case 50:Mtext5.SetValue 1
        Case 60:Mtext6.SetValue 1
        Case 70:Mtext7.SetValue 1
        Case 80:Mtext8.SetValue 1
        Case 90:Mtext9.SetValue 1
    End Select
End Sub

Sub ExtraGame()
    If Credits < 8 Then
        AddCredits 1
    End If
    PlaySound "fx_knocker"
End Sub

'******************
'   GI effects
' independent routine
' it turns on the gi
' when there is a ball
' in play
'******************




'**************
'   Score
'**************

Sub AddScore(Points)
    Select Case Points
        Case 10, 100, 1000
            Score = Score + Points
            ScoreReel1.AddValue Points
            If Points = 100 AND(Score MOD 1000) \ 100 = 0 Then  'New 1000 reel
                PlaySound "fx_bell1000"
            ElseIf Points = 10 AND(Score MOD 100) \ 10 = 0 Then 'New 100 reel
                PlaySound "fx_bell100"
            Else
                PlaySound "fx_bell" &Points
            End If
        Case 50
            Add10 = Add10 + 5	
            AddScore10Timer.Enabled = TRUE
        Case 500
            Add100 = Add100 + 5	
            AddScore100Timer.Enabled = TRUE
        Case 2000, 3000, 4000, 5000
            Add1000 = Add1000 + Points \ 1000 
            AddScore1000Timer.Enabled = TRUE
    End Select

    ' check replays
    If Score >= Special1 AND Special1Awarded = False Then
        ExtraGame
        Special1Awarded = True
    End If
    If Score >= Special2 AND Special2Awarded = False Then
        ExtraGame
        Special2Awarded = True
    End If
    If Score >= Special3 AND Special3Awarded = False Then
        ExtraGame
        Special3Awarded = True
    End If
End Sub

'******************************
'TIMER DE 10, 100 y 1000 PUNTOS
'******************************

Sub AddScore10Timer_Timer()
    if Add10 > 0 then
        AddScore 10
        Add10 = Add10 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore100Timer_Timer()
    if Add100 > 0 then
        AddScore 100
        Add100 = Add100 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore1000Timer_Timer()
    if Add1000 > 0 then
        AddScore 1000
        Add1000 = Add1000 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

'***********
' Highscore
'***********

Sub Loadhs
    Dim x
    x = LoadValue(TableName, "HighScore")
    If(x <> "") Then HighScore = CDbl(x) Else HighScore = 10000 End If
End Sub

Sub Savehs
    if(Score > HighScore) Then
        HighScore = Score
        SaveValue TableName, "HighScore", HighScore
    End If
End Sub

'***********
' Credits
'***********

Sub LoadCredits
    Dim x
    x = LoadValue(TableName, "Credits")
    If(x <> "") Then Credits = CDbl(x) Else Credits = 0 End If
End Sub

Sub SaveCredits
    SaveValue TableName, "Credits", Credits
End Sub


'**************
'    Bonus
'**************

Sub AddBonus
    If Bonus < 10 Then
        Bonus = Bonus + 1
     
    End If
End Sub

Sub AddBonusMultiplier 'simply turn on the lights
    If l14.State = 0 Then
        l14.State = 1
    ElseIf l15.State = 0 Then
        l15.State = 1
    End If
End Sub



Sub CountBonus
    If Bonus > 0 Then
        Bonus = Bonus -1
        AddScore 1000 + 1000 * l14.State + 1000 * l15.State
        BonusCountDelay = 4 + l14.State * 3 + l15.State * 3
    Else
        BonusCountDelay = 0
        'reset bonus multiplier lights
        l14.State = 0
        l15.State = 0
        ' continue the end of ball procedure
        NextBall
    End If
End Sub



'********************
' Diverse Help/Sounds
'********************

Sub aRubbers_Hit(idx):PlaySound "fx_rubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aPostRubbers_Hit(idx):PlaySound "fx_rubber2", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aMetals_Hit(idx):PlaySound "fx_MetalHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aPlastics_Hit(idx):PlaySound "fx_PlasticHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aGates_Hit(idx):PlaySound "fx_Gate", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aWoods_Hit(idx):PlaySound "fx_Woodhit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub

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


'**********************************************************************
'       HIT EVENTS: tables switches and events start here
' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit sub will follow this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/modes this trigger is a member of
' *********************************************************************

'***********
' Bumpers
'***********

Sub Bumper1_Hit 'left bumper
    PlaySound "fx_bumper", 0, 1, -0.05, 0.05
    A1.State = 1:A2. State = 1
    AddScore 100
End Sub

Sub Bumper2_Hit 'right bumper
    PlaySound "fx_bumper", 0, 1, 0.05, 0.05
    A3.State = 1:A4. State = 1
    AddScore 100
End Sub

Sub Bumper3_Hit 'right bumper
    PlaySound "fx_bumper5", 0, 1, 0.05, 0.05
    A5.State = 1:A6. State = 1
    AddScore 100
End Sub

Sub Bumper4_Hit 'right bumper
    PlaySound "fx_bumper5", 0, 1, 0.05, 0.05
    A7.State = 1:A8. State = 1
    AddScore 100
End Sub

Sub Timer1_Timer
    A1.State = 0:A2. State = 0
    A3.State = 0:A4. State = 0
    A5.State = 0:A6. State = 0
    A7.State = 0:A8. State = 0
    Timer1.Enabled = True
End Sub

Sub Lane1_Hit
    PlaySound "sensor"
    AddScore 10
    Light15.State = 1
End Sub

Sub Lane2_Hit
    PlaySound "sensor"
    AddScore 10
    Light16.State = 1
End Sub


Sub Lane3_Hit
    PlaySound "sensor"
    AddScore 10
    Light17.State = 1
End Sub


Sub Lane4_Hit
    PlaySound "sensor"
    AddScore 10
    Light18.State = 1
End Sub






'***************
'  Slingshots
'***************
Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    PlaySound "fx_slingshot", 0, 1, -0.05, 0.05
    'LeftSling1.Visible = 0
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    ' add points
    AddScore 10
End Sub

Sub LeftSlingShot_Timer
      LeftSlingShot.TimerEnabled = 0
   

    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    PlaySound "fx_slingshot", 0, 1, 0.05, 0.05
    'RightSling1.Visible = 0
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    ' add points
    AddScore 10
End Sub

Sub RightSlingShot_Timer
   
     RightSlingShot.TimerEnabled = 0
    RStep = RStep + 1
End Sub

'*********************
'  Targets & switches
'*********************

' Flipper Lanes

Sub sw1_Hit ' left outlane
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        AddBonus
        'give extra ball if light is lit
        If l12.State = 1 Then
            PlaySound "fx_knocker"
            Ball = Ball - 1
            l1.State = 1
        End If
End Sub


Sub sw3_Hit ' right outlane
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        AddBonus
        'give extra game is light is lit
        If l13.State = 1 Then
            ExtraGame
        End If
End Sub

Sub sw2_Hit ' left inlane
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
End Sub

Sub sw4_Hit ' right inlane
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
End Sub

' Top Lanes

Sub sw6_Hit
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        AddBonus
        If l27.State = 1 Then
            AddBonus
        End If
End Sub

Sub sw7_Hit
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        AddBonus
        If l26.State = 1 Then
            AddBonus
        End If
End Sub

Sub sw8_Hit
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        AddBonus
        If l25.State = 1 Then
            AddBonus
        End If
End Sub

Sub sw9_Hit
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        AddBonus
        If l24.State = 1 Then
            AddBonus
        End If
End Sub

Sub sw10_Hit
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        AddBonus
        If l23.State = 1 Then
            AddBonus
        End If
End Sub

' Side Lanes

Sub sw5_Hit
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        If l22.State = 1 Then
            AddBonusMultiplier
        End If
End Sub

Sub sw11_Hit
    PlaySound "fx_sensor", 0, Vol(ActiveBall), pan(ActiveBall), 0.05
        AddScore 500
        If l21.State = 1 Then
            AddBonusMultiplier
        End If
End Sub

' Triggers on top of the red lights









' Droptargets

Dim dt1, dt2, dt3, dt4, dt5,dt6,dt7,dt8, dtdropped

Sub ResetDroptargets
    PlaySound "flip_hit_3"
    dt1 = 0
    dt2 = 0
    dt3 = 0
    dt4 = 0
    dt5 = 0
    dt6 = 0
    dt7 = 0
    dt8 = 0
    drop1.IsDropped = 0
    drop2.IsDropped = 0
    drop3.IsDropped = 0
    drop4.IsDropped = 0
    drop5.IsDropped = 0
    drop6.IsDropped = 0
    drop7.IsDropped = 0
    drop8.IsDropped = 0
End Sub

Sub CheckDroptargets
    If(dt1 + dt2 + dt3 + dt4 + dt5) = 5 Then
        dtdropped = 1
        ResetTargetsDelay = 30
    End If
    'turn on the Special and gives 5000 points but only once per ball
    If Special = False AND dtdropped AND(l16.State + l14.State + l15.State) = 0 Then
        Addscore 5000
        Special = True
        l16.State = 1
        l14.State = 1
    End If
End Sub

Sub drop1_Hit
        PlaySound "fx_droptarget", 0, 1, -0.05, 0.05
        drop1.IsDropped = 1
        Light7.State = 1
        dt1 = 1
        AddScore 500
        AddBonus
        CheckDroptargets
End Sub

Sub drop2_Hit
        PlaySound "fx_droptarget", 0, 1, -0.05, 0.05
        drop2.IsDropped = 1
        Light8.State = 1
        dt2 = 1
        AddScore 500
        AddBonus
        CheckDroptargets
End Sub

Sub drop3_Hit
        PlaySound "fx_droptarget", 0, 1, -0.05, 0.05
        drop3.IsDropped = 1
        Light9.State = 1
        dt3 = 1
        AddScore 500
        AddBonus
        CheckDroptargets
End Sub

Sub drop4_Hit
        PlaySound "fx_droptarget", 0, 1, -0.05, 0.05
        drop4.IsDropped = 1
        Light10.State = 1
        dt4 = 1
        AddScore 500
        AddBonus
        CheckDroptargets
End Sub

Sub drop5_Hit
        PlaySound "fx_droptarget", 0, 1, -0.05, 0.05
        drop5.IsDropped = 1
        Light11.State = 1
        dt5 = 1
        AddScore 500
        AddBonus
        CheckDroptargets
End Sub

Sub drop6_Hit
        PlaySound "fx_droptarget", 0, 1, -0.05, 0.05
        drop6.IsDropped = 1
        Light12.State = 1
        dt6 = 1
        AddScore 500
        AddBonus
        CheckDroptargets
End Sub

Sub drop7_Hit
        PlaySound "fx_droptarget", 0, 1, -0.05, 0.05
        drop7.IsDropped = 1
        Light13.State = 1
        dt7 = 1
        AddScore 500
        AddBonus
        CheckDroptargets
End Sub


Sub drop8_Hit
        PlaySound "fx_droptarget", 0, 1, -0.05, 0.05
        drop8.IsDropped = 1
        Light14.State = 1
        dt8 = 1
        AddScore 500
        AddBonus
        CheckDroptargets
End Sub

' Holes

Dim HolePos
HolePos = 0

Sub Hole_Hit
    PlaySound "fx_kicker_enter", 0, 1, 0.15, 0.05
    HolePos = 0
    HoleScoreTimer.Enabled = 1
End Sub

Sub HoleScoreTimer_Timer
    Select Case HolePos
        Case 0:Addscore 100 + 900 * l33.State
        Case 1:Addscore 100 + 900 * l32.State
        Case 2:Addscore 100 + 900 * l31.State
        Case 3:Addscore 100 + 900 * l30.State
        Case 4:Addscore 100 + 900 * l29.State
        Case 5:PlaySound "fx_kicker", 0, 1, 0.15, 0.05:Hole.Kick 240, 7 + RND(1) * 3:holekicker.IsDropped = 0:holekicker.TimerEnabled = 1:Me.Enabled = 0
    End Select

    HolePos = HolePos + 1
End Sub

Sub holekicker_Timer
    holekicker.IsDropped = 1
    Me.TimerEnabled = 0
End Sub

' 10 point rebounds

Sub aRebounds_Hit(idx):PlaySound "fx_rubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:AddScore 10:End Sub

' Spinner

Sub Spinner_Spin
    AddScore 10 + 900 * l28.State
    TopLightsPos = TopLightsPos + 1:UpdateTopLights
End Sub








function RandomNumber(ByVal max)
    RandomNumber = Int(max * Rnd+1)
end function


Sub Trigger2_Hit
    Select Case RandomNumber(17)
	Case 1: PlaySound "3BartMan"
	Case 2: PlaySound "3BartVs"
	Case 3: PlaySound "3Call911"
   Case 4: PlaySound "3CityGone"
   Case 5: PlaySound "3EatShorts"
   Case 6: PlaySound "3EatShorts2"
   Case 7: PlaySound "3GiantBall"
   Case 8: PlaySound "3Haha"
   Case 9: PlaySound "3KillYou"
   Case 10: PlaySound "3KillYou2"
    Case 11: PlaySound "3KillYou3"
   Case 12: PlaySound "3Loser"
   Case 13: PlaySound "3OhDear"
   Case 14: PlaySound "3Retreat"
   Case 15: PlaySound "3Stupid"
   Case 16: PlaySound "3theHell"
   Case 17: PlaySound "3WorstGame"
   End Select
   AddScore(50)

End Sub


Sub Trigger3_Hit
    Select Case RandomNumber(17)
	Case 1: PlaySound "5wiggumRoll"
	Case 2: PlaySound "5waste"
	Case 3: PlaySound "5hardGame"
   Case 4: PlaySound "3CityGone"
   Case 5: PlaySound "3EatShorts"
   Case 6: PlaySound "3EatShorts2"
   Case 7: PlaySound "3GiantBall"
   Case 8: PlaySound "3Haha"
   Case 9: PlaySound "3KillYou"
   Case 10: PlaySound "3KillYou2"
    Case 11: PlaySound "3KillYou3"
   Case 12: PlaySound "3Loser"
   Case 13: PlaySound "5geek"
   Case 14: PlaySound "5getbent"
   Case 15: PlaySound "3Stupid"
   Case 16: PlaySound "3theHell"
   Case 17: PlaySound "3WorstGame"
   End Select
   AddScore(50)

End Sub


Sub Trigger4_Hit
    Select Case RandomNumber(17)
	Case 1: PlaySound "5alright"
	Case 2: PlaySound "5alrighty"
	Case 3: PlaySound "5apuClean"
   Case 4: PlaySound "5apuThank"
   Case 5: PlaySound "5bartLaugh"
   Case 6: PlaySound "5carumba"
   Case 7: PlaySound "5dodge"
   Case 8: PlaySound "5duggHa"
   Case 9: PlaySound "5groovy"
   Case 10: PlaySound "5homerLaugh"
    Case 11: PlaySound "5krusty"
   Case 12: PlaySound "5mrBurns"
   Case 13: PlaySound "5ned"
   Case 14: PlaySound "5OhBoy"
   Case 15: PlaySound "5snakeHa"
   Case 16: PlaySound "5woo"
   Case 17: PlaySound "5yea"
   End Select
   AddScore(50)

End Sub

Sub Trigger5_Hit
    Select Case RandomNumber(17)
	Case 1: PlaySound "4boing"
	Case 2: PlaySound "4button"
	Case 3: PlaySound "4drop"
   Case 4: PlaySound "4panel"
   Case 5: PlaySound "fx_diverter"
   Case 6: PlaySound "metalhit_medium"
    Case 7: PlaySound "metalhit2"
    End Select
    AddScore(500)
   Light3.State = 1
End Sub


Sub Trigger6_Hit
    Select Case RandomNumber(17)
	Case 1: PlaySound "4boing"
	Case 2: PlaySound "4button"
	Case 3: PlaySound "4drop"
   Case 4: PlaySound "4panel"
   Case 5: PlaySound "fx_diverter"
   Case 6: PlaySound "metalhit_medium"
    Case 7: PlaySound "metalhit2"
    End Select
    AddScore(500)
    Light4.State = 1
End Sub


Sub Trigger7_Hit
    Select Case RandomNumber(17)
	Case 1: PlaySound "4boing"
	Case 2: PlaySound "4button"
	Case 3: PlaySound "4drop"
   Case 4: PlaySound "4panel"
   Case 5: PlaySound "fx_diverter"
   Case 6: PlaySound "metalhit_medium"
    Case 7: PlaySound "metalhit2"
    End Select
    AddScore(500)
    Light5.State = 1
End Sub

Sub Trigger8_Hit
    Select Case RandomNumber(17)
	Case 1: PlaySound "4boing"
	Case 2: PlaySound "4button"
	Case 3: PlaySound "4drop"
   Case 4: PlaySound "4panel"
   Case 5: PlaySound "fx_diverter"
   Case 6: PlaySound "metalhit_medium"
    Case 7: PlaySound "metalhit2"
    End Select
    AddScore(500)
    Light6.State = 1
End Sub


Sub Trigger9_Hit
    Select Case RandomNumber(17)
	Case 1: PlaySound "4boing"
	Case 2: PlaySound "4button"
	Case 3: PlaySound "4drop"
   Case 4: PlaySound "4panel"
   Case 5: PlaySound "fx_diverter"
   Case 6: PlaySound "metalhit_medium"
    Case 7: PlaySound "metalhit2"
    End Select
    AddScore(500)
    Light19.State = 1
    Light20.State = 1
    Light21.State = 1
End Sub


'**   End of Scripts 1.0 by Ghost updates may be made





