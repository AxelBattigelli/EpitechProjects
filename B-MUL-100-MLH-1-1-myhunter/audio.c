/*
** EPITECH PROJECT, 2023
** audio
** File description:
** create audio, play
*/

#include <SFML/Graphics.h>
#include <SFML/Audio.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "my_hunter.h"

void music(void)
{
    sfMusic *music;

    music = sfMusic_createFromFile("music.wav");
    sfMusic_setVolume(music, 70);
    sfMusic_setLoop(music, sfTrue);
    if (sfMusic_getStatus(music) != sfPlaying)
        sfMusic_play(music);
}

void effect(char *path_sound)
{
    sfSoundBuffer *buf = sfSoundBuffer_createFromFile(path_sound);
    sfSound *sound = sfSound_create();

    sfSound_setBuffer(sound, buf);
    sfSound_play(sound);
}
