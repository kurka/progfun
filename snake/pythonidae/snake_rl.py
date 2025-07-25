from stable_baselines3.common.env_checker import check_env
from env import GridWorldEnv

env = GridWorldEnv("human")
# It will check your custom environment and output additional warnings if needed
# check_env(env)


obs, info = env.reset()
n_steps = 10
for i in range(n_steps):
    # Random action
    print(f"Step {i}")
    action = env.action_space.sample()
    obs, reward, terminated, truncated, info = env.step(action)
    if terminated:
        obs, info = env.reset()

print("success!")
