[BITS 64]

global strcasecmp
global my_strcasecmp

SECTION .text

strcasecmp:
my_strcasecmp:
    ENTER 0, 0

loop:
    MOV al, [rdi]
    MOV dl, [rsi]
    CALL tolower_al
    CALL tolower_dl    
    CMP al, dl
    JNE end
    TEST al, al
    JE end    
    INC rdi
    INC rsi
    JMP loop

end:
    SUB al, dl
    MOVSX RAX, al
    LEAVE
    RET

tolower_al:
    CMP al, 'A'
    JB return
    CMP al, 'Z'
    JA return
    ADD al, 32

tolower_dl:
    CMP dl, 'A'
    JB return
    CMP dl, 'Z'
    JA return
    ADD dl, 32

return:
    RET
