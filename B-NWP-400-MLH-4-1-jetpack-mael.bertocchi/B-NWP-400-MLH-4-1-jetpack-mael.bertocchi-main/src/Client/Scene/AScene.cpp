/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** AScene.cpp
*/

#include "Client/Scene/AScene.hpp"
#include "Shared/Exception.hpp"

Jetpack::Client::AScene::AScene(std::shared_ptr<Database> database) : _database(database) {}

void Jetpack::Client::AScene::Update()
{
    // Don't do anything by default
}
