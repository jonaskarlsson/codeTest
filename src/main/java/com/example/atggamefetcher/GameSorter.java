package com.example.atggamefetcher;

import com.example.atggamefetcher.pojo.Game;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public class GameSorter {
    List<Game> todayGames;
    List<Game> leftOverGames;

    public GameSorter() {
        todayGames = new ArrayList<>();
    }

    public List<Game> getGamesForToday(List<Game> games) {
        todayGames = new ArrayList<>();
        addTodayGames(games);
        addBigGames(games);
        addLeftOverGames(games);
        return todayGames;
    }

    public List<Game> addTodayGames(List<Game> games) {
        todayGames = games.stream()
            .filter(game -> game.getStart().getDayOfYear()== LocalDateTime.now().getDayOfYear())
            .sorted(Comparator.comparing(Game::getStart))
            .collect(Collectors.toList());
        return todayGames;
    }

    public List<Game> addBigGames(List<Game> games) {
        List<Game> bigGames = games.stream()
            .filter(game -> Boolean.TRUE.equals(game.getBigGame()))
            .sorted(Comparator.comparing(Game::getStart)).toList();
        todayGames.addAll(bigGames);
        return bigGames;
    }

    public List<Game> addLeftOverGames(List<Game> games) {
        leftOverGames = games.stream()
            .filter(game -> game.getStart().getDayOfYear()!= LocalDateTime.now().getDayOfYear())
            .filter(game -> Boolean.FALSE.equals(game.getBigGame()))
            .sorted(Comparator.comparing(Game::getStart)).toList();
        todayGames.addAll(leftOverGames);
        return leftOverGames;
    }
}