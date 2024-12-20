((docker-compose "--project-directory ~/projets/Coopener-v5/devenv/" "--file ~/projets/Coopener-v5/devenv/docker-compose.yml")
 (docker-compose-logs "--follow")
 (docker-container-logs "-f" "--tail 1000")
 (magit-fetch "--prune" "--tags")
 (magit-log:magit-log-mode "-n100" "--date-order" "--graph" "--color" "--decorate"))
