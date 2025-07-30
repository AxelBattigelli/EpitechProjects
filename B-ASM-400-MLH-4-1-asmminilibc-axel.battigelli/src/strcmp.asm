[BITS 64]

global strcmp
global my_strcmp

SECTION .text

strcmp:
my_strcmp:
    ENTER 0, 0

loop:
    MOV al, [rdi]
    MOV dl, [rsi]
    CMP al, dl
    JNE end
    TEST al, al
    JE end
    INC rdi
    INC rsi
    JMP loop

end:
    SUB al, dl
    MOVSX rax, al
    LEAVE
    RET
