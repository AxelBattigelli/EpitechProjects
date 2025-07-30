[BITS 64]

global strstr
global my_strstr

SECTION .text

strstr:
my_strstr:
    ENTER 0, 0
    mov r8, rdi

loop:
    MOV al, BYTE [rdi]
    MOV dl, BYTE [rsi]
    CMP al, dl
    JE search
    INC rdi
    JMP loop

search:
    MOV al, BYTE [rdi]
    MOV dl, BYTE [rsi]
    CMP al, dl
    JNE loop
    TEST dl, dl
    JE end
    TEST al, al
    JE not_found
    INC rdi
    INC rsi
    JMP search

end:
    MOV rax, r8 
    LEAVE
    RET

not_found:
    XOR rax, rax
    LEAVE
    RET
