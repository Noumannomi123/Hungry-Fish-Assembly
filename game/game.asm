; 21L- 5363, Rabiya Irfan
; 21L- 5438, Nouman Ahmad
[org 0x0100]
    jmp start
toexit: db 'Are you sure you want to quit? (y/n): $'
currscore: dw 0    
Score: db'Score: '
wel: db 'Welcome to Hungry Fish';22 
hello: db 'Hello ' ; 6
madeby: db 'Made by:'; 7
dvlp1: db'21L-5363 Rabiya'; 15
dvlp2: db'21L-5438 Nouman'
maxlen: dw 49; 1 for pressing enter
askname: db 'Enter your name: $'
st: db 'Press Enter to continue or ESC to exit'; 38
Help: db 'Help:-';6
instruc: db 'W: UP, A: LEFT, D: RIGHT, S: DOWN';33
abtscore: db 'Score details:- Green coin: 10 pts, Red coin: 50 pts'; 52
name: times 50 db 0;
lenname: dw 0
greenrow: dw 19
greencol: dw 66
redrow: dw 21
redcol: dw 70
flag : dw 0
temp : dw 0
oldisr: dd 0
countu: dw 0
countd: dw 0
upfish: dw 0; stores the row number where fish starts
downfish: dw 0
presentloc: dw 0
currentcol: dw 0 
tickcount: dw 0
currred: dw 0
currgreen: dw 0 ; current location of green coin
onesec: dw 0
oldisr2: dd 0
SEED: dw 0
LCGmul: dw 25173
LCGadd: dw 13849
timerforred: dw 0
timerforgreen: dw 0
printnum: 
    push bp 
    mov bp, sp 
    push es 
    push ax 
    push bx 
    push cx 
    push dx 
    push di 
    mov ax, 0xb800 
    mov es, ax ; point es to video base 
    mov ax, [bp+4] ; load number in ax 
    mov bx, 10 ; use base 10 for division 
    mov cx, 0 ; initialize count of digits 
    nextdigit:
    mov dx, 0 ; zero upper half of dividend 
    div bx ; divide by 10 
    add dl, 0x30 ; convert digit into ascii value 
    push dx ; save ascii value on stack 
    inc cx ; increment count of values 
    cmp ax, 0 ; is the quotient zero 
    jnz nextdigit ; if no divide it again 
    mov di, 150 ; point di to 70th column 
    nextpos: pop dx ; remove a digit from the stack 
    mov dh,0x7F  ; use normal attribute 
    mov [es:di], dx ; print char on screen 
    add di, 2 ; move to next screen location 
    loop nextpos ; repeat for all digits on stack 
    pop di 
    pop dx 
    pop cx 
    pop bx 
    pop ax
    pop es
    pop bp
    ret 2

printcoin:
    push bp
    mov bp, sp
    push es
    push ax; row
    push bx; column
    push dx; attribute color
    push cx; 
    push si
    push di
    mov ax,0xb800
    mov es,ax
    mov di,[bp + 10]; current green coin's loc
    mov ax, [bp + 4]
    mov bx, [bp + 6]
    mov dx,[bp + 8]
    ; cmp ax,0
    ; je firstcoin
;     mov word[es:di], 0x3720; aqua color
; firstcoin:
    mov si,80
    mul si; ax = ax * si
    add ax,bx; row + col
    shl ax,1; *2
    mov si,ax; si has current loc
    mov word[es:si], dx
    pop di
    pop si
    pop cx
    pop dx
    pop bx
    pop ax
    pop es
    pop bp
    ret 8

CalcNew:
    push ax
    mov word ax,[LCGmul]
    ; mov     ax, 25173          ; LCG Multiplier

    mul     word [SEED]     ; DX:AX = LCG multiplier * seed
    add word ax,[LCGadd]
    ; add     ax, 13849          ; Add LCG increment value
    ; Modulo 65536, AX = (multiplier*seed+increment) mod 65536
    ; shr ax,5
    add word[LCGmul],1
    add word[LCGadd],1
    ; shl word [LCGmul],1
    ; shl word [LCGadd],1
    shr ax,5
    mov     [SEED], ax          ; Update seed = return value
    pop ax
    ret


randompos:
    push bp
    mov bp,sp
    push ax
    push bx
    push cx
    push dx
    push si
    mov bx,[bp + 6]
    ; mov bx,[bp + 6]; column 
    mov cx,0
    push cx
    ; mov ah,0x00; interrupts to get system time        
    ; int 1ah; CX:DX now hold number of clock ticks since midnight      
    ; mov  ax, dx
    call CalcNew
    mov word ax,[SEED]
    xor  dx, dx
    mov  cx, 8
    div  cx       ; here dl contains the remainder of the division - from 0 to 9
    ; mov ah, 2h   ; call interrupt to display a value in DL
    ; int 21h    
    pop cx
    mov ch,dl
    push cx
    ; mov ah,0x00; interrupts to get system time        
    ; int 1ah; CX:DX now hold number of clock ticks since midnight      
    ; mov  ax, dx
    call CalcNew
    mov word ax,[SEED]
    xor  dx, dx
    mov  cx, 10
    div  cx       
    pop cx
    mov cl,dl
    ; converting to single hex number of 2-digit
    mov ax,10
    mul byte ch
    mov ch,al
    add ch,cl; 37 -##
    mov cl,ch;  00 37
    mov ch,0
    mov word [bx],cx; storing in memeory
    ; row 
    mov cx,0
    push cx
    ; mov ah,0x00; interrupts to get system time        
    ; int 1ah; CX:DX now hold number of clock ticks since midnight      
    ; mov  ax, dx
    call CalcNew
    mov word ax,[SEED]
    xor  dx, dx
    mov  cx, 2; ; 17 - 25; 1, 2; 
    div  cx
    
    
    pop cx
    mov ch,dl
    add ch,1
    push cx

    cmp byte ch,2
    je two
    ; mov ah,0x00; interrupts to get system time        
    ; int 1ah; CX:DX now hold number of clock ticks since midnight      
    ; mov  ax, dx
    call CalcNew
    mov word ax,[SEED]
    xor  dx, dx
    mov  cx, 3;  17, 18,19
    div  cx       
    pop cx
    mov cl,dl
    add cl,7
    jmp finish
    two:
    ; mov ah,0x00; interrupts to get system time        
    ; int 1ah; CX:DX now hold number of clock ticks since midnight      
    ; mov  ax, dx
    call CalcNew
    mov word ax,[SEED]
    xor  dx, dx
    mov  cx, 5;  20,21,22,23,24
    div  cx       
    pop cx
    mov cl,dl
    ; converting to single hex number of 2-digit
    finish: 
    mov ax,10
    mul byte ch
    mov ch,al
    add ch,cl
    mov cl,ch
    mov ch,0
    mov bx,[bp + 4]
    ; mov bx,[bp+4]; row
    mov word[bx],cx; storing in memeory
    call    CalcNew
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 4

timer:
    push ax
    push bx
    push dx
    push si
    push di
    mov dx,0
    mov word ax,[cs:tickcount]
    cmp word[cs:timerforgreen], 180
    jnz skipg
    ; printing green coin
    loopr:
    mov word di,[cs:currgreen]; old
    push greencol
    push greenrow
    call randompos
    mov ax,[cs:greenrow]
    mov bx,[cs:greencol]
    mov si,80
    mul si
    add ax,bx
    shl ax,1
    mov si,ax; calculating
    cmp word si, [cs:presentloc]
    je loopr
    mov word [cs:currgreen],si; new
    cmp word di,[cs:presentloc]; if fish is on coin
    je skipaq
    mov word[es:di],0x3720; aqua
    skipaq:
    mov word[cs:timerforgreen], 0
    mov word[es:si],0xAE2A
    
    
skipg:  
    mov dx,0
    mov word ax,[cs:tickcount]
    ; ax = ax / bx
    cmp word[cs:timerforred], 90
    jnz skip
    ; printing green coin
    loopr2:
    mov word di,[cs:currred]; old
    push redcol
    push redrow
    call randompos
    mov ax,[cs:redrow]
    mov bx,[cs:redcol]
    mov si,80
    mul si
    add ax,bx
    shl ax,1
    mov si,ax; calculating
    cmp word si, [cs:presentloc]
    je loopr2
    mov word [cs:currred],si; new
    cmp word di,[cs:presentloc]
    je skipaq2
    mov word[es:di],0x3720; aqua
    skipaq2:
    mov word[es:si],0xCF2A
    mov word[cs:timerforred],0; reset the timer
    
skip:
    mov bh,0
    mov bl,01111111B
    push bx
    push 0x0044; row + col
    push Score; string to be printed
    push word 7; length of string
    call display

    push word [cs:currscore]
    call printnum
    
    call leftshift
    call rightshift
    mov ax,[cs:presentloc]; location of fish
    cmp word[cs:currgreen],ax; compared with position of green coin
    jne ee
    add word[cs:currscore],10
    call newcoing; print new coin after its collection
    mov word[cs:timerforgreen],0
    jmp ee2
    ee:
    cmp word[cs:currred],ax
    jne ee2
    add word[cs:currscore],50
    call newcoinr
    mov word[cs:timerforred],0
    ee2:
    inc word[cs:tickcount]
    inc word[cs:timerforred]; 90
    inc word[cs:timerforgreen]; 180
    mov al,0x20
    out 0x20,al
    pop di
    pop si
    pop dx
    pop bx
    pop ax
    iret

rightshift:
    mov ax,0xb800
    mov es,ax
    push ax
    push si
    push cx
    push dx
    push bx
    mov cx,10
    mov bx,0
    mov dx,1440
    mov di,2
    mov ax, 0

outt:
	mov si,dx;
    mov ax,dx
    add ax,160
    mov si,ax
    sub si,2 ; 158
    mov ax,[es : si]
    mov word[temp],  ax; last value of row
    ; pop dx
    inn:    
        sub si, 2; 156
        mov bx,[es : si]
        mov di,si
        mov [es:si + 2], bx;2, 4, 6
        cmp si,dx
        jnz inn
    ; add ax,2
    mov word bx, [temp]
    mov [es:si], bx
    add dx,160
    add cx,1; no. of row
    cmp dx,2720
    jnz outt
    pop bx
    pop dx
    pop cx
    pop si
    pop ax
    
    ret 
    
leftshift:
    mov ax,0xb800
    mov es,ax
    push ax
    push si
    push cx
    push dx
    push bx

    mov cx,1
    mov bx,0
    mov dx,160; !!!
    mov si,0;
    mov di,0
lout:
    mov si,dx;
    mov ax,dx
    add ax,160
    push ax
    mov ax, [es : si]
    mov word[temp], ax
    add si,2; 2
    pop ax

    lin:
        mov bx, [es:si]
        mov [es:si - 2], bx
        add si,2; 2nd value in 1st
        cmp si, ax; if end of the row
        jnz lin
    mov word ax,[temp]
    mov [es : si - 2], ax; at 158
    add dx,160
    add cx,1
    cmp dx, 1440
    jnz lout


    pop bx
    pop dx
    pop cx
    pop si 
    pop ax
    
    ret 



upshift:
    push ax
    push si
    mov ax,0xb800
    mov es,ax
    mov word ax, [cs:presentloc]; 3268
    mov si,ax
    mov word[es:si], 0x3720; aqua
    sub si,160
    mov word[es:si], 0x4F3E
    mov word[cs:presentloc],si
    ; mov word[es:si - 160], dx
    mov word[cs:presentloc],si
    pop si
    pop ax
    ret 

fishdown:
    push ax
    push si
    mov ax,0xb800
    mov es,ax
    mov word ax, [cs:presentloc]; 3268
    mov si,ax
    mov word[es:si], 0x3720; aqua
    add si,160
    mov word[es:si], 0x4F3E
    mov word[cs:presentloc],si
    
    pop si
    pop ax
    ret 

fishright:
    push ax
    push bx
    push si
    push di
    push dx
    mov ax,0xb800
    mov es,ax
    mov word ax,[cs:presentloc]; 3268 current
    mov word bx,[cs:currentcol]; 68 col
    mov si,ax; 3268
    mov di,bx; 44h 
    mov word[es : si], 0x3720
    add si,2
    add di,2
    cmp di,160
    jnz previousrow
    sub si,160
    mov di,0
previousrow:
    mov word[es : si],0x4F3E 
    mov word[cs:presentloc],si; new cs:presentloc
    mov word[cs:currentcol], di; new current column
    pop dx
    pop di
    pop si
    pop bx
    pop ax
    ret

fishleft:
    push ax
    push bx
    push si
    push di
    push dx
    mov ax,0xb800
    mov es,ax
    mov word ax,[cs:presentloc]; 3268 current
    mov word bx,[cs:currentcol]; 68 col
    mov si,ax; 3268
    mov di,bx; 44h 
    mov word[es : si], 0x3720
    sub si,2
    sub di,2
    cmp di,-2
    jnz previousrow2
    add si,160
    mov di,158; standing at the end
previousrow2:
    mov word[es : si],0x4F3E 
    mov word[cs:presentloc],si; new cs:presentloc
    mov word[cs:currentcol], di; new current column
    pop dx
    pop di
    pop si
    pop bx
    pop ax
    ret

delay:
    push cx
    mov cx,0xFFF
    l11: loop l11
    mov cx,0xFFFF
    l2: loop l2
    mov cx,0xFFFF
    l3: loop l3
    pop cx
    ret
delay2: 
    push cx
    mov cx,0xCCCC
    l1: loop l1
    pop cx
    ret

playsound:
    push ax
    push cx
    mov cx, 5
    loops:         mov al, 0b6h
    out 43h, al
    mov ax, 1fb4h
    out 42h, al
    mov al, ah
    out 42h, al
    in al, 61h
    mov ah,al
    or al, 3h
    out 61h, al
    call delay2
    mov al, ah
    out 61h, al
    call delay2
    mov ax, 152fh
    out 42h, al
    mov al, ah
    out 42h, al
    in al, 61h
    mov ah,al
    or al, 3h
    out 61h, al
    call delay2
    mov al, ah
    out 61h, al
    call delay2
    mov ax, 0A97h
    out 42h, al
    mov al, ah
    out 42h, al
    in al, 61h
    mov ah,al
    or al, 3h
    out 61h, al
    call delay2
    mov al, ah
    out 61h, al
    call delay2
    loop loops
    pop cx
    pop ax
    ret

kbisr:
    ; nextcmpn
    push ax
    push es
    push bx
    in al,0x60
    
    cmp al,0x11; if the 'W' key, i.e; up is pressed
    jne nextcmp
    ; if yes then move screen up and exit the program
    ; check if reached the top boundary
    mov word bx,[cs:upfish]
    cmp word [cs:countu],bx
    jne nosound
    call playsound
    jmp exit
nosound:
    call upshift
    add word [cs:countu],1
    add word[cs:downfish], 1
    jmp exit


nextcmp:
    cmp al,0x1E; if 'A', then leftshift the fish
    jne nextcmp2
    call fishleft
    jmp exit

nextcmp2:
    cmp al,0x20; if 'D', then rightshift the fish
    jne nextcmp3
    call fishright
    jmp exit
    
nextcmp3:
    cmp al,0x1F; if 'S', then downshift the fish
    jne nextcmp4
    
    mov word bx,[cs:downfish]
    cmp word [cs:countd],bx
    jne nosound2
    call playsound
    jmp exit
nosound2:
    call fishdown
    add word [cs:countd],1
    add word[cs:upfish], 1
    jmp exit

nextcmp4:
    cmp al, 0x01; if the ESC key is pressed
    jne ending
    mov word[cs:flag],1; flag that terminates the loop of main loop
    jmp exit
ending:
    pop bx
    pop es
    pop ax
    jmp far [cs:oldisr]
exit:
    mov al,0x20
    out 0x20, al
    pop bx
    pop es
    pop ax
    iret


clrscr: 
    push es 
    push ax 
    push cx 
    push di 
    mov ax, 0xb800 
    mov es, ax ; point es to video base 
    xor di, di ; point di to top left column 
    mov ax, 0x0720 ; space char in normal attribute 
    mov cx, 2000 ; number of screen locations 
    cld ; auto increment mode 
    rep stosw ; clear the whole screen 
    pop di
    pop cx
    pop ax
    pop es
    ret
scr:
    push bp
    mov bp,sp
	push es
	push ax
	mov ax,0xb800
	mov es,ax
    mov di,[bp + 4]; start from here
    mov cx,[bp + 6]; end 
    mov ax,[bp + 8]; color attribute
    cld 
    rep stosw
	pop ax
	pop es
    pop bp
	ret 6



makefish:
    push bp
    mov bp,sp
    push es
    push ax
    push bx
    push cx
    push si
    push di
    push dx
    mov bx,[bp + 4]; the row number for fish, 25
    mov cx,[bp + 6]; the column number for fish
    mov ax,0xb800
    mov es,ax
    
    mov ax,bx
    mov si,80
    mul si
    shl ax,1
    shl cx,1
    add ax,cx
    mov si,ax
    mov word [es:si], 0x4F3E
    mov word[cs:presentloc], si; 
    
    ; ax has 3268 as cs:presentloc
    ; cx has column number
    mov word[cs:currentcol], cx; 0044h; 68d

    push bx
    sub bx,17; 21 - 17 = 4
    mov word[upfish], bx; counts how many times user can move up
    pop bx
    
    push bx
    mov cx,24
    sub cx,bx; 25 - 21 = 4
    mov word[downfish], cx
    pop bx


    pop dx
    pop di
    pop si
    pop cx
    pop bx
    pop ax
    pop es
    pop bp

    ret 4

makeboat:
    push bp
    mov bp,sp
    push es
    push ax
    push bx
    push cx
    push dx
    push di
    push si
    mov ax,0xb800
    mov es,ax
    
    ; calculating the start position
    mov di,80
    mov ax,[bp + 6]; rows
    mul di
    shl ax,1
    mov bx,[bp + 4]; cols
    shl bx,1
    add ax,bx; 14*80 *2 + 38 *2
    mov di,ax; pos to start the lower part of ship
    mov cx,bx
    shr cx,1
    mov cx,[bp + 8]
    shr cx,2
    ; upper part
    mov ax,[bp + 6]
upouter:
    mov bx,[bp + 8]; blocks to fill
    mov si,di
    push di
    push ax
    mov di, 80
    mul di
    shl ax,1
    add ax,160; end of row
    upinner:
        mov word[es:si], 0x7D20
        add si,2
        cmp si,ax
        jnz prevo
        sub si,160
    prevo:
        sub bx,1
        cmp bx,0
        jnz upinner
    pop ax
    pop di
    add ax,1
    add di,160
    loop upouter
    
    ;lower part
    mov cx,[bp + 8]
    sub di,cx
    shr cx,1;no. of rows for the lower part -> size / 4
    mov dx,0; used to decrease the counter by 4 each time


lowouter: ;lower part of the ship
    mov bx,[bp + 8]; blocks to fill
    shl bx,1; doubling it
    sub bx,dx
    mov si,di
    push dx
    push di
    push ax
    mov di, 80
    mul di
    shl ax,1
    add ax,160; end of row
    lowinner:
        mov word[es:si], 0xA820
        add si,2
        cmp si,ax
        jnz previ
        sub si,160
    previ:
        sub bx,1
        cmp bx,0
        jnz lowinner
    pop ax
    pop di
    add ax,1
    add di,164
    pop dx
    add dx,4
    loop lowouter


    pop si
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    pop bp
    ret 6

makemountain:
    push bp
    mov bp,sp
    push ax
    push bx
    push cx
    push es
    push dx
    mov cx,[bp + 8]; size, no of rows, 5
    mov ax,[bp + 6]; 3, row
    mov bx,[bp + 4]; 40 col

    mov di,80
    mul di; Ax = di * ax
    shl ax, 1
    shl bx, 1
    add ax, bx
    mov di,ax

    mov ax, 0xb800
    mov es,ax
    mov bx,1
    
    mov ax,[bp + 6]; then find last block of this row's column, 2

mout:
    mov si,di
    mov dx,bx
    push di
    push ax; old ax, that is bp+6
    push dx
    mov di, 80
    mul di
    shl ax,1
    add ax,160; the next row
    pop dx
     
    minn:

        mov word[es : si], 0xE720
        add si,2
        cmp si,ax
        jnz prev
        sub si,160; if zero
    prev:
        sub dx,1
        cmp dx,0
        jnz minn 
    add bx,2 
    pop ax
    add ax,1; next time in loop, adding one in ax
    pop di
    add di, 158
    loop mout

    pop dx
    pop es
    pop cx
    pop bx
    pop ax
    pop bp 
    ret 6

userinput:
	push bp
	mov bp,sp
	push ax
	push bx
	push cx
    push di
    mov di,[bp + 6]
	mov bx,[bp + 4]
	mov cx,[maxlen]; to check if max len reached
next:
	mov ah,1; get character
	int 0x21
	mov [bx],al
    add word[di],1
	add bx,1
	sub cx,1
	cmp cx,0
	je exit2
	cmp al, 13
	jnz next
exit2:
    pop di
	pop cx
	pop bx
	pop ax
	pop bp
	ret 4
display:
    push bp
    mov bp,sp
    push ax
    push bx
    push cx
    push di
    mov di,[bp + 6]; points to string
    mov ah, 0x13		; service 13 - print string
	mov al, 0			; subservice 01 – update cursor 
    mov bx,[bp+10]
	; mov bh, 0			; output on page 0
	; mov bl, 00011111B	; normal attrib
	mov cx, [bp + 4]	; length of string
	mov dx, [bp+8]		; row 12 column 30
	push ds
	pop es				; es=ds segment of string
	mov bp,di	; bp = offset of string
	int 0x10			; call BIOS video service
    pop di
    pop cx
    pop bx
    pop ax
    pop bp
    ret 8

welcomeguyz:
	call clrscr		
	mov dx,askname	
	mov ah,9; to display string 
	int 0x21

    push lenname; len of name
    push name
    call userinput


    mov dx,0x3720
    mov di,0; 17th row start
    mov si,480
    push dx
    push si
    push di
    call scr;  make third half blue

    mov dx,0x1720
    mov di,960
    mov si,640
    push dx
    push si
    push di
    call scr;  make second half blue

    mov dx,0x3720
    mov di,2240; 17th row start
    mov si,1360; 25th row end
    push dx
    push si
    push di
    call scr;  make third half blue

    mov bh,0
    mov bl,00111111B
    push bx
    push 0x021E
    push hello
    push word 6
    call display

    mov bh,0
    mov bl,00111111B
    push bx
    push 0x0224
    push name
	push word [lenname]
    call display

    
    mov bh,0
    mov bl,00111111B
    push bx
    push 0x041A
    push wel
	push word 22
    call display


    mov bh,0
    mov bl,00111111B
    push bx
    push 0x1640
    push madeby
    push word 7
    call display

    mov bh,0
    mov bl,00111111B
    push bx
    push 0x1740
    push dvlp1
    push word 15
    call display
    
    mov bh,0
    mov bl,00111111B
    push bx
    push 0x1840
    push dvlp2
    push word 15
    call display

    mov bh,0
    mov bl,10111111B
    push bx
    push 0x1710
    push st
    push word 38
    call display

    mov bh,0
    mov bl,00111111B
    push bx
    push 0x1301
    push Help
    push word 6
    call display

    mov bh,0
    mov bl,00111111B
    push bx
    push 0x1401
    push instruc
    push word 33
    call display

    mov bh,0
    mov bl,00111111B
    push bx
    push 0x1501; row + col
    push abtscore; string to be printed
    push word 52; length of string
    call display

    
    push word 8
    push word 8
    push word 14
    call makeboat

loopstart:
    mov ax,0xb800
    mov es,ax

    mov word[es:2600],0x4F3E; hardcode
    mov word[es:2840],0xAE2A; coin display

    mov di,2600
    mov cx,32; 40 total
    fishhmove:
        mov bx,[es:di]
        mov word[es:di+2], bx
        mov word[es:di],0x3720
        call delay
        add di,2
        loop fishhmove
    mov word bx,[es:di]
    mov word[es:di+160],bx
    mov word[es:di],0x3720
    call delay
    add di,160
    mov cx,8
    fishhright:
        mov bx,[es:di]
        mov word[es:di+2], bx
        mov word[es:di],0x3720
        call delay
        add di,2
        loop fishhright
prestart:
	mov ah,1
	int 0x21; get char 
    cmp al, 27
    je nostart
	cmp al, 13; is the enter pressed
	jne prestart
nostart:
	ret

newcoing:
    push ax
    push si
    push bx
    push es
    looprr:; initial one greeen coin
    mov ax,0xb800
    mov es,ax
    push greencol
    push greenrow
    call randompos
    mov ax,[cs:greenrow]
    mov bx,[cs:greencol]
    mov si,80
    mul si
    add ax,bx
    shl ax,1
    mov si,ax; calculating
    cmp word si, [cs:presentloc]
    je looprr
    mov word [cs:currgreen],si; new
    mov word[es:si],0xAE2A
    pop es
    pop bx
    pop si
    pop ax
    ret


newcoinr:
    push ax
    push si
    push bx
    push es
    looprr2:
    mov ax,0xb800
    mov es,ax
    push redcol
    push redrow
    call randompos
    mov ax,[cs:redrow]
    mov bx,[cs:redcol]
    mov si,80
    mul si
    add ax,bx
    shl ax,1
    mov si,ax; calculating
    cmp word si, [cs:presentloc]
    je looprr2
    mov word [cs:currred],si; new
    mov word[es:si],0xCF2A
    pop es
    pop bx
    pop si
    pop ax
    ret
start:
	call welcomeguyz
    cmp al,27
    jne strt
    mov ax,0x4c00
    int 0x21
strt:
    call clrscr
        
    mov word[countu],0
    mov word[countd],0
    mov word[upfish],0
    mov word[downfish],0
    xor ax,ax
    mov es,ax
	; saving original interrupts' ISR's
    mov ax,[es:9*4]
    mov [oldisr],ax
    mov ax,[es:9*4+2]
    mov [oldisr+2],ax

    mov ax,[es:8*4]
    mov [oldisr2],ax
    mov ax,[es:8*4+2]
    mov [oldisr2 + 2],ax
    
	; hooking interrrups
	cli
	mov word[es:9*4],kbisr
	mov word[es:9*4+2], cs
	sti

	xor ax,ax
    mov es,ax

    cli 
    mov word[es:8*4], timer
    mov word[es:8*4+2], cs
    sti

    
	mov dx, 0xFE20
    mov di,0
    mov si,720
    push dx
    push si
    push di
    call scr;  make first half 

    mov dx,0x1720
    mov di,1440
    mov si,640
    push dx
    push si
    push di
    call scr;  make second half blue


    mov dx,0x3720
    mov di,2720; 17th row start
    mov si,1000; 25th row end
    push dx
    push si
    push di
    call scr;  make third half blue

    push word 34; starting col
    push word 20; starting row
    call makefish; 3268 present
    call newcoing
    ;red
    call newcoinr
	push word 6; size
    push word 3; row numnber
    push word 4; column number
    call makemountain


    push word 8; size
    push word 1; row numnber
    push word 14; column number
    call makemountain

    push word 6; size
    push word 3; row numnber
    push word 24; column number
    call makemountain


    push word 8; size
    push word 1; row numnber
    push word 34; column number
    call makemountain

	push word 6; size
    push word 3; row numnber
    push word 44; column number
    call makemountain

	push word 8; size
    push word 1; row numnber
    push word 54; column number
    call makemountain

	push word 6; size
    push word 3; row numnber
    push word 64; column number
    call makemountain

	push word 8; size
    push word 1; row numnber
    push word 74; column number
    call makemountain

    push word 8
    push word 9
    push word 44
    call makeboat

    push word 6
    push word 10
    push word 72
    call makeboat

    push word 8
    push word 11
    push word 12
    call makeboat

    ; 27 AA

    
inf:
    
    cmp word[flag],1
    jne inf
    
    
;unhooking    
end:
    xor ax,ax
    mov es,ax
	mov ax, [oldisr]
    mov bx, [oldisr+2] 
    cli 
    mov [es:9*4], ax
    mov [es:9*4+2], bx 
    sti 
	
    mov ax, [oldisr2]
    mov bx, [oldisr2+2]
    cli
    mov [es:8*4],ax
    mov [es:8*4+2],bx
    sti

    call clrscr
    
    mov dx,toexit
    mov ah,9
    int 0x21
    loopexit:
        mov ah,1
        int 0x21
        cmp byte al,'y'
        jne nextex
        mov word[cs:flag],1
        jmp ext
    nextex:
        cmp byte al,'Y'
        jne nextex2
        mov word[cs:flag],1
        jmp ext
    nextex2:
        cmp byte al,'n'
        jne nextex3
        mov word[cs:flag],0
        jmp ext
    nextex3:
        cmp byte al,'N'
        jne loopexit
        mov word [cs:flag],0
    ext:
    cmp word[cs:flag],1
    jne strt
    mov dx,start
    add dx,15
    mov cl,4
    shr dx,cl
    mov ax,0x3100
    int 0x21