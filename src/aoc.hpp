#pragma once

namespace curl {
#include <curl/curl.h>
#include <curl/easy.h>
}

#include <cstddef>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <memory>
#include <sstream>
#include <string>
#include <variant>

#define CURL_CHECK(expr, message, ...)                                                                              \
    do {                                                                                                            \
        curl::CURLcode res = (expr);                                                                                \
        if (res != curl::CURLE_OK) {                                                                                \
            throw std::runtime_error(                                                                               \
                std::format("curl error: {}: " message, curl::curl_easy_strerror(res) __VA_OPT__(, ) __VA_ARGS__)); \
        }                                                                                                           \
    } while (0)

using AocFunction = std::function<std::string(const std::string&&)>;

template <std::uint8_t Day>
struct Solution;

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
    const std::string token;
    curl::CURL* curl;

    std::unordered_map<std::uint8_t, std::string> inputs;

   public:
    Aoc(const std::string&& token) : token(token), curl(curl::curl_easy_init()) {
        if (!curl) {
            throw std::runtime_error("Failed to initialize curl");
        }

        std::unordered_map<std::uint8_t, std::string> inputs;

        std::filesystem::path cache_dir{"aoc-cache"};
        if (std::filesystem::exists(cache_dir) && std::filesystem::is_directory(cache_dir)) {
            for (const auto& entry : std::filesystem::directory_iterator(cache_dir)) {
                if (entry.is_regular_file()) {
                    std::ifstream cache{entry.path()};
                    if (cache.is_open()) {
                        std::uint8_t day;
                        std::string input;
                        cache >> day >> input;
                        inputs[day] = input;
                    } else {
                        throw std::runtime_error("Failed to open cache file");
                    }
                }
            }
        }
    }

    ~Aoc() noexcept(false) {
        curl::curl_easy_cleanup(curl);

        std::filesystem::path cache_dir{"aoc-cache"};
        if (!std::filesystem::exists(cache_dir)) {
            std::filesystem::create_directory(cache_dir);
        }

        for (const auto& [day, input] : inputs) {
            std::filesystem::path cache_file = cache_dir / std::format("day-{}.txt", day);
            std::ofstream cache{cache_file};
            if (cache.is_open()) {
                cache << day << '\n' << input;
            } else {
                throw std::runtime_error("Failed to open cache file");
            }
        }
    }

    std::string get_input(const std::uint8_t day) {
        if (inputs.contains(day)) {
            return inputs.at(day);
        } else {
            std::string input = curl_get_input(day);
            inputs[day] = input;

            return input;
        }
    }

    std::string submit(const std::uint8_t day, const std::uint8_t part, const std::string& answer) const {
        struct curl::curl_slist* headers = nullptr;
        headers = curl::curl_slist_append(headers, "Accept: text/plain");
        curl::curl_easy_setopt(curl, curl::CURLOPT_HTTPHEADER, headers);

        curl::curl_easy_setopt(curl, curl::CURLOPT_URL,
                               std::format("https://adventofcode.com/{}/day/{}/answer", Year, day).c_str());
        curl::curl_easy_setopt(curl, curl::CURLOPT_COOKIE, std::format("session={}", token).c_str());

        std::string data = std::format("level={}&answer={}", part, answer);
        curl::curl_easy_setopt(curl, curl::CURLOPT_POSTFIELDS, data.c_str());

        std::string response;
        curl::curl_easy_setopt(curl, curl::CURLOPT_WRITEDATA, &response);
        curl::curl_easy_setopt(curl, curl::CURLOPT_WRITEFUNCTION, write_string_callback);

        CURL_CHECK(curl::curl_easy_perform(curl), "Failed to submit answer ❌: {}", response);

        std::uint64_t response_code;
        CURL_CHECK(curl::curl_easy_getinfo(curl, curl::CURLINFO_RESPONSE_CODE, &response_code),
                   "Failed to get response code ❌");

        if (response_code != 200) {
            throw std::runtime_error(std::format("Failed to submit answer ❌\n-> {}\n-> {}", response_code, response));
        }

        curl::curl_slist_free_all(headers);

        return response;
    }

   private:
    std::string curl_get_input(const std::uint8_t day) const {
        curl::curl_easy_setopt(curl, curl::CURLOPT_URL,
                               std::format("https://adventofcode.com/{}/day/{}/input", Year, day).c_str());
        curl::curl_easy_setopt(curl, curl::CURLOPT_COOKIE, std::format("session={}", token).c_str());

        std::string input;
        curl::curl_easy_setopt(curl, curl::CURLOPT_WRITEDATA, &input);
        curl::curl_easy_setopt(curl, curl::CURLOPT_WRITEFUNCTION, write_string_callback);

        CURL_CHECK(curl::curl_easy_perform(curl), "Failed to get input ❌");

        std::uint64_t response_code;
        CURL_CHECK(curl::curl_easy_getinfo(curl, curl::CURLINFO_RESPONSE_CODE, &response_code),
                   "Failed to get response code ❌");

        if (response_code != 200) {
            throw std::runtime_error(std::format("Failed to get input ❌\n-> {}\n-> {}", response_code, input));
        }

        return input;
    }
};