[BITS 64]

SECTION .text

global memmove
global my_memmove

; if dest < src = normal else reverse

memmove:
my_memmove:
    ENTER 0, 0
    MOV rcx, rdx
    LEA rdx, [rdi]
    CMP rcx, 0
    JE end
    CMP rsi, rdi
    JG copy
    DEC rsi
    DEC rdi

loop:
    MOV al, BYTE [rsi + rcx] 
    MOV BYTE [rdi + rcx], al
    DEC rcx
    CMP rcx, 0
    JNZ loop
    JMP end

copy:
    REP MOVSB

end:
    MOV rax, rdx
    LEAVE
    RET
