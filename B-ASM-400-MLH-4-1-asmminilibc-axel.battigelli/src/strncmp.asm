[BITS 64]

global strncmp
global my_strncmp

SECTION .text

strncmp:
my_strncmp:
    ENTER 0, 0
    MOV rcx, rdx

loop:
    MOV al, BYTE [rdi]
    MOV dl, BYTE [rsi]
    CMP rcx, 0
    JE equal_return
    CMP al, dl
    JNE end
    CMP al, 0
    JE end
    INC rdi
    INC rsi
    DEC rcx
    JMP loop

equal_return:
    XOR rax, rax
    LEAVE
    RET

end:
    SUB al, dl
    MOVSX rax, al
    LEAVE
    RET
