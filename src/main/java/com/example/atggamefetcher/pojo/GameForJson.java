package com.example.atggamefetcher.pojo;

import java.time.LocalDateTime;
import java.util.Objects;

public class GameForJson implements Comparable<GameForJson> {

    private String name;
    private String type;
    private LocalDateTime start;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public LocalDateTime getStart() {
        return start;
    }

    public void setStart(LocalDateTime start) {
        this.start = start;
    }

    @Override
    public int compareTo(GameForJson games) {
        return start.compareTo(games.start);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        GameForJson game = (GameForJson) o;

        if (!Objects.equals(name, game.name)) return false;
        return Objects.equals(type, game.type);

    }

    @Override
    public int hashCode() {
        int result = name != null ? name.hashCode() : 0;
        result = 31 * result + (type != null ? type.hashCode() : 0);
        return result;
    }
}