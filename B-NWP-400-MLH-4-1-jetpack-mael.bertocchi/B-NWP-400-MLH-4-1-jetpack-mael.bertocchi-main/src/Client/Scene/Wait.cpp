/*
** EPITECH PROJECT, 2025
** Jetpack
** File description:
** Wait.cpp
*/

#include "Client/Scene/Wait.hpp"
#include "Shared/Logger.hpp"

#include <format>

Jetpack::Client::Wait::Wait(std::shared_ptr<Database> database) : AScene(database)
{
    Shared::Logger::Info("Wait scene created", _database->IsDebugEnabled());
}

void Jetpack::Client::Wait::Load()
{
    _database->CreateText("level-0-wait-text", _database->GetWaitText(), { -1, -1 }, 25);
    Shared::Logger::Info("Wait scene loaded", _database->IsDebugEnabled());
}

void Jetpack::Client::Wait::Unload()
{
    _database->DestroyText("level-0-wait-text");
    Shared::Logger::Info("Wait scene unloaded", _database->IsDebugEnabled());
}

void Jetpack::Client::Wait::AnalyzeEvent(const sf::Event& event)
{
    if (event.type == sf::Event::KeyPressed && event.key.code == sf::Keyboard::Escape) {
        _database->PushEvent(Shared::Data::Header::WANT_TO_QUIT);
        _database->SetScene("quit");
    }
}
