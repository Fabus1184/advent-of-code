#pragma once

namespace curl {
#include <curl/curl.h>
#include <curl/easy.h>
}

#include <cstddef>
#include <format>
#include <functional>
#include <memory>
#include <string>
#include <variant>

std::size_t write_string_callback(char* data, std::size_t size, std::size_t nmemb, void* userp) {
    std::string* str = static_cast<std::string*>(userp);
    str->append(data, size * nmemb);
    return size * nmemb;
}

template <std::size_t Year>
class Aoc {
    Aoc() = delete;
    Aoc(const Aoc&) = delete;
    Aoc& operator=(const Aoc&) = delete;
    Aoc(Aoc&&) = delete;
    Aoc& operator=(Aoc&&) = delete;

   private:
    const std::uint8_t day;
    const std::string token;
    curl::CURL* curl;

    const std::string input_url() const {
        return "https://adventofcode.com/" + std::to_string(Year) + "/day/" + std::to_string(day) + "/input";
    }

   public:
    Aoc(std::uint8_t day, const std::string&& token) : day(day), token(token), curl(curl::curl_easy_init()) {
        if (!curl) {
            throw std::runtime_error("Failed to initialize curl");
        }
    }

    ~Aoc() { curl::curl_easy_cleanup(curl); }

    std::string get_input() const {
        curl::curl_easy_setopt(curl, curl::CURLOPT_URL, input_url().c_str());
        curl::curl_easy_setopt(curl, curl::CURLOPT_COOKIE, std::format("session={}", token).c_str());

        std::string input;
        curl::curl_easy_setopt(curl, curl::CURLOPT_WRITEDATA, &input);
        curl::curl_easy_setopt(curl, curl::CURLOPT_WRITEFUNCTION, write_string_callback);

        curl::CURLcode res = curl::curl_easy_perform(curl);
        if (res != curl::CURLE_OK) {
            throw std::runtime_error(std::format("Failed to get input: {}", curl::curl_easy_strerror(res)));
        }

        return input;
    }
};