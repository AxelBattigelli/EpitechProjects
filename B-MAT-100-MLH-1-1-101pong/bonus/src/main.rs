use std::{path::Path, sync::Mutex, thread};

use sfml::{
    graphics::{Color, RenderTarget, RenderWindow, Sprite, Texture, Transformable},
    window::{Event, Key, Style},
};

enum PhysicsResult {
    Nothing,
    LeftScored,
    RightScored,
}

fn run_the_physics(
    paddle_left: &mut Sprite,
    paddle_right: &mut Sprite,
    bullet: &mut Sprite,
    bullet_speed: &mut (f32, f32),
) -> PhysicsResult {
    bullet.move_(*bullet_speed);

    if bullet.position().y < 0. {
        bullet_speed.1 = 10.;
    }
    if bullet.position().y > 800. - 30. {
        bullet_speed.1 = -10.;
    }
    if bullet.position().x < 0. {
        *bullet_speed = (0., 0.);
        return PhysicsResult::RightScored;
    }
    if bullet.position().x > 1300. - 30. {
        *bullet_speed = (0., 0.);
        return PhysicsResult::LeftScored;
    }

    if bullet.position().x < 64. + 30.
        && bullet.position().y > paddle_left.position().y
        && bullet.position().y < paddle_left.position().y + 300.
    {
        bullet.set_position((64. + 30., bullet.position().y));
        bullet_speed.0 = 10.;
    }
    if bullet.position().x > 1300. - 64. - 30. - 30.
        && bullet.position().y > paddle_right.position().y
        && bullet.position().y < paddle_right.position().y + 300.
    {
        bullet.set_position((1300. - 64. - 30. - 30., bullet.position().y));
        bullet_speed.0 = -10.;
    }

    paddle_left.set_position((64., (bullet.position().y - 150.).max(0.)));
    paddle_right.set_position((1300. - 64. - 30., (bullet.position().y - 150.).max(0.)));

    PhysicsResult::Nothing
}

fn main() {
    let mut window = RenderWindow::new((1300, 800), "Pong", Style::CLOSE, &Default::default());
    let mut clock = sfml::system::Clock::start();
    let mut bullet_speed = (10., 10.);

    let mut tex1 = Texture::new().unwrap();
    tex1.load_from_file("paddleR.png", Default::default())
        .unwrap();
    let mut sprite_pr = Sprite::with_texture(&tex1);
    sprite_pr.set_position((1300. - 30. - 64., 800. / 2. - 150.));

    let mut tex2 = Texture::new().unwrap();
    tex2.load_from_file("paddleL.png", Default::default())
        .unwrap();
    let mut sprite_pl = Sprite::with_texture(&tex2);
    sprite_pl.set_position((64., 800. / 2. - 150.));

    let mut tex3 = Texture::new().unwrap();
    tex3.load_from_file("bullet.png", Default::default())
        .unwrap();
    let mut sprite_b = Sprite::with_texture(&tex3);
    sprite_b.set_position((1300. / 2. - 15., 800. / 2. - 15.));

    thread::scope(|_sc| {
        while window.is_open() {
            while let Some(event) = window.poll_event() {
                match event {
                    Event::Closed => window.close(),
                    Event::KeyPressed { code, .. } => match code {
                        Key::Up => {
                            sprite_pr.move_((0., -10.));
                        }
                        Key::Down => {
                            sprite_pr.move_((0., 10.));
                        }
                        Key::Z => {
                            sprite_pl.move_((0., -10.));
                        }
                        Key::S => {
                            sprite_pl.move_((0., 10.));
                        }
                        _ => (),
                    },
                    Event::MouseMoved { x, y } => {
                        sprite_pl.set_position((64., y as f32 - 150.));
                    }
                    _ => (),
                }
            }

            if sprite_pr.position().y < 0. {
                sprite_pr.set_position((sprite_pr.position().x, 0.));
            }
            if sprite_pr.position().y > 800. - 300. {
                sprite_pr.set_position((sprite_pr.position().x, 800. - 300.));
            }
            if sprite_pl.position().y < 0. {
                sprite_pl.set_position((sprite_pl.position().x, 0.));
            }
            if sprite_pl.position().y > 800. - 300. {
                sprite_pl.set_position((sprite_pl.position().x, 800. - 300.));
            }

            window.clear(Color::BLACK);
            window.draw_sprite(&sprite_pr, &Default::default());
            window.draw_sprite(&sprite_pl, &Default::default());
            window.draw_sprite(&sprite_b, &Default::default());
            window.display();

            if clock.elapsed_time().as_milliseconds() > 10 {
                clock.restart();
                match run_the_physics(
                    &mut sprite_pl,
                    &mut sprite_pr,
                    &mut sprite_b,
                    &mut bullet_speed,
                ) {
                    PhysicsResult::LeftScored => {
                        sprite_b.set_position((1300. / 2. - 15., 800. / 2. - 15.));
                        bullet_speed.0 = -10.;
                        bullet_speed.1 = 10.;
                    }
                    PhysicsResult::RightScored => {
                        sprite_b.set_position((1300. / 2. - 15., 800. / 2. - 15.));
                        bullet_speed.0 = 10.;
                        bullet_speed.1 = 10.;
                    }
                    _ => (),
                }
            }
        }
    })
}
